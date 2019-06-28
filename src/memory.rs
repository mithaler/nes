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
    pub bus: CpuBus,
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
            bus
        }
    }
}

impl CpuMem {
    pub fn get_page(&self, start_addr: u16) -> &[u8] {
        let addr = start_addr as usize;
        match start_addr {
            0 ... 0x1F00 => &self.ram[addr..addr+256],
            //0x4020 ... 0xFFFF => self.mapper.borrow().get_cpu_page(start_addr),
            _ => panic!("Weird start address for page read: {:04X?}", addr)
        }
    }
}

impl Addressable for CpuMem {
    fn get(&self, addr: u16) -> u8 {
        match addr {
            0 ... 0x1FFF => self.ram[(addr & 0x7FF) as usize],
            0x2000 ... 0x3FFF => self.bus.borrow_mut().get(((addr - 0x2000) & 0x7) + 0x2000),
            0x4000 ... 0x4017 => self.bus.borrow_mut().get(addr),
            // 0x4018 ... 0x401F used only for internal testing
            0x4020 ... 0xFFFF => self.mapper.borrow().get_cpu_space(addr),
            _ => panic!()
        }
    }

    fn set(&mut self, addr: u16, value: u8) {
        match addr {
            0 ... 0x1FFF => self.ram[(addr & 0x7FF) as usize] = value,
            0x2000 ... 0x3FFF => self.bus.borrow_mut().set(((addr - 0x2000) & 0x7) + 0x2000, value),
            0x4000 ... 0x4017 => self.bus.borrow_mut().set(addr, value),
            // 0x4018 ... 0x401F used only for internal testing
            0x4020 ... 0xFFFF => self.mapper.borrow_mut().set_cpu_space(addr, value),
            _ => panic!()
        }
    }
}

#[derive(Debug)]
pub struct PpuCtrl {
    pub nametable_num: u8,
    pub addr_increment_down: bool,
    pub sprite_table_addr: u16,
    pub background_table_addr: u16,
    pub sprite_size_large: bool,  // if false, 8x8; if true, 8x16
    pub send_nmi: bool
}

impl PpuCtrl {
    fn from_register(value: u8) -> PpuCtrl {
        let nametable_num = value & 0b0000_0011;
        let addr_increment_down = if (value & 0b0000_0100) != 0 { true } else { false };
        let sprite_table_addr = if (value & 0b0000_1000) != 0 { 0x1000 } else { 0x0000 };
        let background_table_addr = if (value & 0b0001_0000) != 0 { 0x1000 } else { 0x0000 };
        let sprite_size_large = if (value & 0b0010_0000) != 0 { true } else { false };
        let send_nmi = if (value & 0b1000_0000) != 0 { true } else { false };
        PpuCtrl {
            nametable_num,
            addr_increment_down,
            sprite_table_addr,
            background_table_addr,
            sprite_size_large,
            send_nmi
        }
    }
}

// https://wiki.nesdev.com/w/index.php/PPU_memory_map
pub struct PpuMem {
    pub mapper: Mapper,
    palette_ram: Mem,
    oam: Mem,

    ppuctrl: PpuCtrl,
    ppumask: u8,
    pub scroll_x: u8,
    pub scroll_y: u8,
    vblank: bool,
    sprite0hit: bool,
}

type Pattern = (Vec<u8>, Vec<u8>);

impl PpuMem {
    pub fn new(mapper: Mapper) -> PpuMem {
        PpuMem {
            mapper,
            palette_ram: initialized_mem(0x20),
            oam: initialized_mem(0x100),

            ppuctrl: PpuCtrl::from_register(0),
            ppumask: 0,
            scroll_x: 0,
            scroll_y: 0,
            vblank: false,
            sprite0hit: false,
        }
    }

    pub fn pattern(&self, num_byte: u8, offset: u16, large: bool) -> Pattern {
        let mut num = ((num_byte as u16) << 4) + offset;
        let size = if large {16} else {8};
        let mut first = Vec::with_capacity(size);
        let mut second = Vec::with_capacity(size);
        for _ in 0 .. 8 {
            first.push(self.get(num));
            num += 1;
        }
        for _ in 0 .. 8 {
            second.push(self.get(num));
            num += 1;
        }
        (first, second)
    }

    pub fn borrow_oam(&self) -> &Mem {
        &self.oam
    }

    pub fn set_vblank(&mut self, vblank: bool) {
        self.vblank = vblank;
    }

    pub fn set_sprite0hit(&mut self, sprite0hit: bool) {
        self.sprite0hit = sprite0hit;
    }

    pub fn get_ppuctrl(&self) -> &PpuCtrl {
        &self.ppuctrl
    }

    pub fn set_ppuctrl(&mut self, ppuctrl: u8) {
        self.ppuctrl = PpuCtrl::from_register(ppuctrl);
    }

    pub fn set_ppumask(&mut self, ppumask: u8) {
        self.ppumask = ppumask;
    }

    pub fn get_ppumask(&self) -> u8 {
        self.ppumask
    }

    pub fn set_oam(&mut self, addr: u8, value: u8) {
        self.oam[addr as usize] = value;
    }

    pub fn set_oamdma(&mut self, mem: &[u8]) {
        self.oam.splice(.., mem.iter().cloned());
    }

    /// Returns the first 3 bits of PPUSTATUS; the latter 5 are remembered by the bus.
    pub fn get_ppustatus(&self) -> u8 {
        let mut out = 0;
        if self.vblank {
            out |= 0b1000_0000;
        }
        if self.sprite0hit {
            out |= 0b0100_0000;
        }
        // TODO overflow
        out
    }

    fn palette_ram_address(mut addr: u16) -> usize {
        addr = (addr - 0x3F00) & 0x1F;
        usize::from(
            if addr >= 0x10 && (addr & 0b0000_0000_0000_0011) == 0 {
                addr & 0b1111_1111_1110_1111
            } else {
                addr
            }
        )
    }
}

impl Addressable for PpuMem {
    fn get(&self, addr: u16) -> u8 {
        match addr {
            0x0000 ... 0x3EFF => self.mapper.borrow().get_ppu_space(addr),
            0x3F00 ... 0x3FFF => {
                let out = self.palette_ram[PpuMem::palette_ram_address(addr)];
                if (self.ppumask & 1) != 0 {
                    out & 0x30
                } else {
                    out
                }
            },
            _ => panic!()
        }
    }

    fn set(&mut self, addr: u16, value: u8) {
        match addr {
            0x0000 ... 0x3EFF => self.mapper.borrow_mut().set_ppu_space(addr, value),
            0x3F00 ... 0x3FFF => self.palette_ram[PpuMem::palette_ram_address(addr)] = value,
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
        use crate::controllers::Controllers;
        use crate::memory::*;
        use crate::mappers::{Mapper, test_mapper};
        use crate::apu::Apu;

        fn test_mem() -> (CpuMem, Mapper) {
            let mapper = test_mapper(TEST_MEM, &[]);
            let bus = Bus::new(Apu::new(), shared(PpuMem::new(mapper.clone())), shared(Controllers::new()));
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

            assert_eq!(PpuMem::palette_ram_address(0x3F10), 0x0);
            assert_eq!(PpuMem::palette_ram_address(0x3F14), 0x4);
            assert_eq!(PpuMem::palette_ram_address(0x3F18), 0x8);
            assert_eq!(PpuMem::palette_ram_address(0x3F1C), 0xC);
        }
    }

}
