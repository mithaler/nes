use crate::bus::CpuBus;
use crate::common::Addressable;
use crate::mappers::Mapper;

pub type Mem = Box<Vec<u8>>;

pub fn mem(slice: &[u8]) -> Mem {
    Box::new(Vec::from(slice))
}

pub struct CpuMem {
    ram: Mem,
    mapper: Mapper,
    bus: CpuBus,
    apu_registers: Mem,
}

pub fn initialized_mem(size: usize) -> Mem {
    Box::new(vec![0; size])
}

// https://wiki.nesdev.com/w/index.php/CPU_memory_map
impl CpuMem {
    pub fn new(mapper: Mapper, bus: CpuBus) -> CpuMem {
        CpuMem {
            ram: initialized_mem(0x800),  // randomized on a real console
            mapper,
            bus,
            apu_registers: initialized_mem(0x18)
        }
    }
}

impl Addressable for CpuMem {
    fn get(&self, addr: u16) -> u8 {
        match addr {
            0 ... 0x1FFF => self.ram[(addr & 0x7FF) as usize],
            0x2000 ... 0x3FFF => self.bus.borrow_mut().get(((addr - 0x2000) & 0x7) + 0x2000),
            0x4000 ... 0x4017 => self.apu_registers[(addr - 0x4000) as usize],
            // TODO support other mappers; C000 is for small carts
            0x4020 ... 0xFFFF => self.mapper.borrow().get_cpu_space(addr),
            _ => panic!()
        }
    }

    fn set(&mut self, addr: u16, value: u8) {
        match addr {
            0 ... 0x1FFF => self.ram[(addr & 0x7FF) as usize] = value,
            0x2000 ... 0x3FFF => self.bus.borrow_mut().set(((addr - 0x2000) & 0x7) + 0x2000, value),
            0x4000 ... 0x4017 => self.apu_registers[(addr - 0x4000) as usize] = value,
            // TODO support RAM inside the cart at 6000 - 7FFF
            0x4020 ... 0xFFFF => self.mapper.borrow_mut().set_cpu_space(addr, value),
            _ => panic!()
        }
    }
}

// https://wiki.nesdev.com/w/index.php/PPU_memory_map
pub struct PpuMem {
    mapper: Mapper,
    palette_ram: Mem,
    oam: Mem,

    ppuctrl: u8,
    ppumask: u8,
    vblank: bool,
}

type Pattern = (Vec<u8>, Vec<u8>);

impl PpuMem {
    pub fn new(mapper: Mapper) -> PpuMem {
        PpuMem {
            mapper,
            palette_ram: initialized_mem(0x20),
            oam: initialized_mem(0x100),

            ppuctrl: 0,
            ppumask: 0,
            vblank: false,
        }
    }

    pub fn pattern(&self, num: u16) -> Pattern {
        let mut first = Vec::with_capacity(8);
        let mut second = Vec::with_capacity(8);
        for idx in (num * 8) .. ((num + 1) * 8) {
            first.push(self.get(idx));
        }
        for idx in (num * 8 + 0x1000) .. ((num + 1) * 8 + 0x1000) {
            second.push(self.get(idx));
        }
        (first, second)
    }

    pub fn set_vblank(&mut self, vblank: bool) {
        self.vblank = vblank;
    }

    pub fn get_ppuctrl(&self) -> u8 {
        self.ppuctrl
    }

    pub fn set_ppuctrl(&mut self, ppuctrl: u8) {
        self.ppuctrl = ppuctrl;
    }

    pub fn set_ppumask(&mut self, ppumask: u8) {
        self.ppumask = ppumask;
    }

    pub fn get_ppumask(&self) -> u8 {
        self.ppumask
    }

    /// Returns the first 3 bits of PPUSTATUS; the latter 5 are remembered by the bus.
    pub fn get_ppustatus(&self) -> u8 {
        let mut out = 0;
        if self.vblank {
            out |= 0b1000_0000;
        }
        // TODO sprite 0 hit, overflow
        out
    }
}

impl Addressable for PpuMem {
    fn get(&self, addr: u16) -> u8 {
        match addr {
            0x0000 ... 0x3EFF => self.mapper.borrow().get_ppu_space(addr),
            0x3F00 ... 0x3FFF => self.palette_ram[((addr - 0x3F00) % 0x20) as usize],
            _ => panic!()
        }
    }

    fn set(&mut self, addr: u16, value: u8) {
        match addr {
            0x0000 ... 0x3EFF => self.mapper.borrow_mut().set_ppu_space(addr, value),
            0x3F00 ... 0x3FFF => self.palette_ram[((addr - 0x3F00) % 0x20) as usize] = value,
            _ => panic!()
        }
    }
}

// A lot of these are really tests of the NROM mapper, but that's the simplest one so that
// also gives us a nice test of how these interact with it.
#[cfg(test)]
mod tests {
    const TEST_MEM: &[u8; 3] = &[1, 2, 3];

    mod cpu_mem {
        use super::TEST_MEM;
        use crate::bus::Bus;
        use crate::common::{Addressable, shared};
        use crate::memory::*;
        use crate::mappers::{Mapper, test_mapper};

        fn test_mem() -> (CpuMem, Mapper) {
            let mapper = test_mapper(TEST_MEM, &[]);
            let bus = Bus::new(shared(PpuMem::new(mapper.clone())));
            (CpuMem::new(mapper.clone(), bus), mapper.clone())
        }

        #[test]
        fn test_read_and_write_ram() {
            let (mut cpu, _mapper) = test_mem();
            cpu.set(0x400, 6);
            assert_eq!(cpu.get(0x400), 6 as u8)
        }

        #[test]
        fn test_read_and_write_ram_mirror() {
            let (mut cpu, _mapper) = test_mem();
            cpu.set(0x400, 6);
            assert_eq!(cpu.get(0x800 + 0x400), 6);
            assert_eq!(cpu.get((0x800 * 2) + 0x400), 6);
            assert_eq!(cpu.get((0x800 * 3) + 0x400), 6);

            cpu.set(0x800 + 0x600, 10);
            assert_eq!(cpu.get(0x600), 10)
        }

        #[test]
        fn test_read_and_write_ppu_regs() {
            let (mut cpu, mapper) = test_mem();
            cpu.set(0x2006, 0x20);
            cpu.set(0x2006, 0x55);
            cpu.set(0x2007, 6);
            assert_eq!(mapper.borrow().get_ppu_space(0x2055), 6 as u8);
        }

        #[test]
        fn test_read_and_write_ram_ppu_regs_mirror() {
            let (mut cpu, mapper) = test_mem();
            cpu.set(0x2006 + 0x8, 0x20);
            cpu.set(0x2006 + (0x8 * 30), 0x55);
            cpu.set(0x2007 + (0x8 * 100), 6);
            assert_eq!(mapper.borrow().get_ppu_space(0x2055), 6 as u8);
        }

        #[test]
        fn test_read_rom() {
            let (cpu, _mapper) = test_mem();
            assert_eq!(cpu.get(0xC000), 1);
            assert_eq!(cpu.get(0xC001), 2);
            assert_eq!(cpu.get(0xC002), 3);
        }

        #[test]
        #[should_panic]
        fn test_write_rom_panics() {
            let (mut cpu, _mapper) = test_mem();
            cpu.set(0xC000, 5);
        }
    }

    mod ppu_mem {
        use super::TEST_MEM;
        use crate::common::Addressable;
        use crate::memory::*;
        use crate::mappers::test_mapper;

        #[test]
        fn test_read_and_write_chr_rom() {
            let mut ppu = PpuMem::new(test_mapper(&[], TEST_MEM));
            assert_eq!(ppu.get(0x0), 1);
            ppu.set(0x2, 5);
            assert_eq!(ppu.get(0x2), 5);
        }

        #[test]
        fn test_palette_ram_mirror() {
            let mut ppu = PpuMem::new(test_mapper(&[], TEST_MEM));
            ppu.set(0x3F01, 0x34);
            assert_eq!(ppu.get(0x3F01), 0x34);
            assert_eq!(ppu.get(0x3F21), 0x34);
            assert_eq!(ppu.get(0x3FA1), 0x34);
            assert_eq!(ppu.get(0x3FC1), 0x34);
        }
    }

}
