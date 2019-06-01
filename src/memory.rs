use crate::common::Addressable;
use crate::mappers::Mapper;

pub type Mem = Box<Vec<u8>>;

pub fn mem(slice: &[u8]) -> Mem {
    Box::new(Vec::from(slice))
}

pub struct CpuMem {
    ram: Mem,
    mapper: Mapper,
    ppu_registers: Mem,
    apu_registers: Mem,
}

pub fn initialized_mem(size: usize) -> Mem {
    Box::new(vec![0; size])
}

// https://wiki.nesdev.com/w/index.php/CPU_memory_map
impl CpuMem {
    pub fn new(mapper: Mapper) -> CpuMem {
        CpuMem {
            ram: initialized_mem(0x800),  // randomized on a real console
            mapper,
            ppu_registers: initialized_mem(0x8),
            apu_registers: initialized_mem(0x18)
        }
    }
}

impl Addressable for CpuMem {
    fn get(&self, addr: u16) -> u8 {
        match addr {
            0 ... 0x1FFF => self.ram[(addr & 0x7FF) as usize],
            0x2000 ... 0x3FFF => self.ppu_registers[((addr - 0x2000) & 0x7) as usize],
            0x4000 ... 0x4017 => self.apu_registers[(addr - 0x4000) as usize],
            // TODO support other mappers; C000 is for small carts
            0x4020 ... 0xFFFF => self.mapper.borrow().get_cpu_space(addr),
            _ => panic!()
        }
    }

    fn set(&mut self, addr: u16, value: u8) {
        match addr {
            0 ... 0x1FFF => self.ram[(addr & 0x7FF) as usize] = value,
            0x2000 ... 0x3FFF => self.ppu_registers[((addr - 0x2000) & 0x7) as usize] = value,
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
    // TODO
}

type Pattern<'a> = (Vec<u8>, Vec<u8>);

impl PpuMem {
    pub fn new(mapper: Mapper) -> PpuMem {
        PpuMem {
            mapper
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
}

impl Addressable for PpuMem {
    fn get(&self, addr: u16) -> u8 {
        match addr {
            0x0000 ... 0x3F00 => self.mapper.borrow().get_ppu_space(addr),
            _ => panic!()
        }
    }

    fn set(&mut self, addr: u16, value: u8) {
        unimplemented!()
    }
}

#[cfg(test)]
mod tests {
    mod cpu_mem {
        use crate::common::Addressable;
        use crate::memory::*;
        use crate::mappers::test_mapper;

        const TEST_MEM: &[u8; 3] = &[1, 2, 3];

        #[test]
        fn can_read_and_write_ram() {
            let mut cpu = CpuMem::new(test_mapper(TEST_MEM, &[]));
            cpu.set(0x400, 6);
            assert_eq!(cpu.get(0x400), 6 as u8)
        }

        #[test]
        fn can_read_and_write_ram_mirror() {
            let mut cpu = CpuMem::new(test_mapper(TEST_MEM, &[]));
            cpu.set(0x400, 6);
            assert_eq!(cpu.get(0x800 + 0x400), 6);
            assert_eq!(cpu.get((0x800 * 2) + 0x400), 6);
            assert_eq!(cpu.get((0x800 * 3) + 0x400), 6);

            cpu.set(0x800 + 0x600, 10);
            assert_eq!(cpu.get(0x600), 10)
        }

        #[test]
        fn can_read_and_write_ppu_regs() {
            let mut cpu = CpuMem::new(test_mapper(TEST_MEM, &[]));
            cpu.set(0x2000, 6);
            assert_eq!(cpu.get(0x2000), 6 as u8)
        }

        #[test]
        fn can_read_and_write_ram_ppu_regs_mirror() {
            let mut cpu = CpuMem::new(test_mapper(TEST_MEM, &[]));
            cpu.set(0x2001, 6);
            assert_eq!(cpu.get(0x2000 + 0x8 + 0x1), 6);
            assert_eq!(cpu.get(0x2000 + (0x5 * 0x8) + 0x1), 6);
        }

        #[test]
        fn can_read_rom() {
            let cpu = CpuMem::new(test_mapper(TEST_MEM, &[]));
            assert_eq!(cpu.get(0xC000), 1);
            assert_eq!(cpu.get(0xC001), 2);
            assert_eq!(cpu.get(0xC002), 3);
        }

        #[test]
        #[should_panic]
        fn cannot_write_rom() {
            let mut cpu = CpuMem::new(test_mapper(TEST_MEM, &[]));
            cpu.set(0xC000, 5);
        }
    }

}
