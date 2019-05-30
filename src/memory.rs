use crate::common::Addressable;

pub type Mem = Box<Vec<u8>>;

pub fn mem(slice: &[u8]) -> Mem {
    Box::new(Vec::from(slice))
}

pub struct CpuMem {
    ram: Mem,
    prg_rom: Mem,
    ppu_registers: Mem,
    apu_registers: Mem,
}

fn initialized_mem(size: usize) -> Mem {
    Box::new((0..size).map(|_| 0 as u8).collect())
}

// https://wiki.nesdev.com/w/index.php/CPU_memory_map
impl CpuMem {
    pub fn new(prg_rom: Mem) -> CpuMem {
        CpuMem {
            ram: initialized_mem(0x800),  // randomized on a real console
            prg_rom,
            ppu_registers: initialized_mem(8),
            apu_registers: initialized_mem(18)
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
            0xC000 ... 0xFFFF => self.prg_rom[(addr - 0xC000) as usize],
            _ => panic!()
        }
    }

    fn set(&mut self, addr: u16, value: u8) {
        match addr {
            0 ... 0x1FFF => self.ram[(addr & 0x7FF) as usize] = value,
            0x2000 ... 0x3FFF => self.ppu_registers[((addr - 0x2000) & 0x7) as usize] = value,
            0x4000 ... 0x4017 => self.apu_registers[(addr - 0x4000) as usize] = value,
            // TODO support RAM inside the cart at 6000 - 7FFF
            0xC000... 0xFFFF => panic!("Can't write to ROM!"),
            _ => panic!()
        }
    }
}

#[cfg(test)]
mod tests {
    mod cpu_mem {
        use crate::memory::*;
        use crate::common::Addressable;

        #[test]
        fn can_read_and_write_ram() {
            let mut cpu = CpuMem::new(Box::new(vec![]));
            cpu.set(0x400, 6);
            assert_eq!(cpu.get(0x400), 6 as u8)
        }

        #[test]
        fn can_read_and_write_ram_mirror() {
            let mut cpu = CpuMem::new(Box::new(vec![]));
            cpu.set(0x400, 6);
            assert_eq!(cpu.get(0x800 + 0x400), 6);
            assert_eq!(cpu.get((0x800 * 2) + 0x400), 6);
            assert_eq!(cpu.get((0x800 * 3) + 0x400), 6);

            cpu.set(0x800 + 0x600, 10);
            assert_eq!(cpu.get(0x600), 10)
        }

        #[test]
        fn can_read_and_write_ppu_regs() {
            let mut cpu = CpuMem::new(Box::new(vec![]));
            cpu.set(0x2000, 6);
            assert_eq!(cpu.get(0x2000), 6 as u8)
        }

        #[test]
        fn can_read_and_write_ram_ppu_regs_mirror() {
            let mut cpu = CpuMem::new(Box::new(vec![]));
            cpu.set(0x2001, 6);
            assert_eq!(cpu.get(0x2000 + 0x8 + 0x1), 6);
            assert_eq!(cpu.get(0x2000 + (0x5 * 0x8) + 0x1), 6);
        }

        #[test]
        fn can_read_rom() {
            let mut cpu = CpuMem::new(Box::new(vec![1, 2, 3]));
            assert_eq!(cpu.get(0xC000), 1);
            assert_eq!(cpu.get(0xC001), 2);
            assert_eq!(cpu.get(0xC002), 3);
        }

        #[test]
        #[should_panic]
        fn cannot_write_rom() {
            let mut cpu = CpuMem::new(Box::new(vec![1, 2, 3]));
            cpu.set(0xC000, 5);
        }
    }

}
