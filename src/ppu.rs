use crate::common::{Clocked, Shared, Addressable};
use crate::cpu::Cpu;
use crate::memory::PpuMem;

pub struct Ppu {
    mem: Shared<PpuMem>,
    cpu: Shared<Cpu>,

    scanline: i16,  // -1 - 261
    tick: u16  // 0 - 340
}

type Tile = Vec<Vec<u8>>;

impl Ppu {
    pub fn new(ppu_mem: Shared<PpuMem>, cpu: Shared<Cpu>) -> Ppu {
        // startup state: https://wiki.nesdev.com/w/index.php/PPU_power_up_state
        Ppu {
            mem: ppu_mem,
            cpu,
            scanline: -1,
            tick: 0,
        }
    }

    fn pattern(&self, num: u16) -> Tile {
        let pattern = self.mem.borrow().pattern(num);
        let iter = pattern.0.iter().zip(pattern.1.iter());

        let mut ret: Tile = Vec::with_capacity(8);
        for (left, right) in iter {
            let mut row = Vec::with_capacity(8);
            let mut bitmask = 0b1000_0000u8;
            for _ in (0u8..8u8).rev() {
                row.push(if (bitmask & *left & *right) != 0 {
                    3u8
                } else if (right & bitmask) != 0 {
                    2u8
                } else if (left & bitmask) != 0 {
                    1u8
                } else {
                    0u8
                });
                bitmask >>= 1;
            }
            ret.push(row);
        }
        ret
    }

    fn tile_num(&self, x: u16, y: u16) -> u8 {
        let addr = 0x2000 + x + (y * 0x20);
        self.mem.borrow().get(addr)
    }

    fn dummy_scanline(&mut self) {
        // TODO even/odd frame
        if self.tick == 1 {
            println!("-- EXITING VBLANK --");
            self.mem.borrow_mut().set_vblank(false);  // TODO sprite 0 hit, overflow
        }
    }

    fn vblank_scanline(&mut self) {
        if self.scanline == 241 && self.tick == 1 {
            println!("-- ENTERING VBLANK --");
            self.mem.borrow_mut().set_vblank(true);
            if (self.mem.borrow().get_ppuctrl() & 0b1000_0000) != 0 {
                self.cpu.borrow_mut().nmi();
            }
        }
    }

    fn visible_scanline(&mut self) {
        let ppumask = self.mem.borrow().get_ppumask();
        if (ppumask & 0b00001000) != 0 {
            //self.render_background();
        }
        if (ppumask & 0b00010000) != 0 {
            //self.render_sprite();
        }
        // TODO sprite priority?
    }

    fn render(&mut self) {
        //println!("Scanline {:?}, Tick {:?}", self.scanline, self.tick);
        match self.scanline {
            -1 => self.dummy_scanline(),
            0 ... 239 => self.visible_scanline(),
            240 => {},  // post-render
            241 ... 260 => self.vblank_scanline(),
            _ => unreachable!()
        }
        self.tick = match self.tick {
            t @ 0 ... 340 => t + 1,
            341 => {
                self.scanline = match self.scanline {
                    s @ -1 ... 259 => s + 1,
                    260 => -1,
                    _ => unreachable!()
                };
                0
            },
            _ => unreachable!()
        }
    }
}

impl Clocked for Ppu {
    fn tick(&mut self) {
        self.render();
    }
}

#[cfg(test)]
mod tests {
    use super::Ppu;
    use crate::bus::Bus;
    use crate::common::shared;
    use crate::mappers::test_mapper;
    use crate::memory::{CpuMem, PpuMem};
    use crate::cpu::Cpu;

    const LEFT: [u8; 8] = [0x41, 0xC2, 0x44, 0x48, 0x10, 0x20, 0x40, 0x80];
    const RIGHT: [u8; 8] = [0x01, 0x02, 0x04, 0x08, 0x16, 0x21, 0x42, 0x87];
    const TILE: [[u8; 8]; 8] = [
        [0, 1, 0, 0, 0, 0, 0, 3],
        [1, 1, 0, 0, 0, 0, 3, 0],
        [0, 1, 0, 0, 0, 3, 0, 0],
        [0, 1, 0, 0, 3, 0, 0, 0],
        [0, 0, 0, 3, 0, 2, 2, 0],
        [0, 0, 3, 0, 0, 0, 0, 2],
        [0, 3, 0, 0, 0, 0, 2, 0],
        [3, 0, 0, 0, 0, 2, 2, 2],
    ];

    fn test_pattern() -> Box<Vec<u8>> {
        // based on the diagram: https://wiki.nesdev.com/w/index.php/PPU_pattern_tables
        let mut chr_rom: Vec<u8> = vec![0; 0x2000];

        // tile 1, not 0 (test offset)
        chr_rom.splice(0x8..0xF, LEFT.iter().cloned());
        chr_rom.splice(0x1008..0x100F, RIGHT.iter().cloned());
        assert_eq!(LEFT, &chr_rom[0x8..0x10]);
        assert_eq!(RIGHT, &chr_rom[0x1008..0x1010]);

        Box::new(chr_rom)
    }

    fn test_ppu() -> Ppu {
        let mapper = test_mapper(&[], test_pattern().as_slice());
        let bus = Bus::new(shared(PpuMem::new(mapper.clone())));
        let cpu = shared(Cpu::new(Box::new(CpuMem::new(mapper.clone(), bus)), true));
        Ppu::new(shared(PpuMem::new(mapper.clone())), cpu)
    }

    #[test]
    fn pattern_overlay() {
        let test_ppu = test_ppu();
        let tile = test_ppu.pattern(1u16);
        assert_eq!(tile.len(), 8);
        for (idx, row) in tile.iter().enumerate() {
            assert_eq!(row.as_slice(), TILE[idx]);
        }
    }
}
