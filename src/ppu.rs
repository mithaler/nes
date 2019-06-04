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

    fn nametable_base_addr(mut x: u16, mut y: u16, mut base: u16) -> (u16, u16, u16) {
        if x >= 64 || y >= 60 {
            // Maybe do something clever here when we get to scrolling wraparound?
            panic!("Nametable coordinates out of bounds! (X: {:?}, Y: {:?})", x, y);
        }
        if x >= 32 {
            base += 0x0400;
            x -= 32;
        }
        if y >= 30 {
            base += 0x0800;
            y -= 30;
        }
        (x, y, base)
    }

    /// Returns the memory address of the tile at coordinates (x, y) in the nametable.
    fn nametable_addr(x: u16, y: u16) -> u16 {
        let (x_within, y_within, base) = Ppu::nametable_base_addr(x, y, 0x2000);
        base + x_within + (y_within * 0x20)
    }

    /// Returns the memory address of the attribute byte for the nametable tile at (x, y).
    fn tile_attr_addr(x: u16, y: u16) -> u16 {
        let (x_within, y_within, base) = Ppu::nametable_base_addr(x, y, 0x23C0);
        base + (x_within / 4) + ((y_within / 4) * 0x8)
    }

    /// Given the coordinates of a tile in the nametable, returns the byte representing the
    /// tile in the nametable.
    fn tile_pattern_num(&self, x: u16, y: u16) -> u8 {
        let addr = Ppu::nametable_addr(x, y);
        self.mem.borrow().get(addr)
    }

    /// Given the coordinates of a tile in the nametable, returns the colorset number of the
    /// tile (from 0 to 3).
    fn tile_colorset(&self, x: u16, y: u16) -> u8 {
        let mut attrs = self.mem.borrow().get(Ppu::tile_attr_addr(x, y));
        let addr = Ppu::nametable_addr(x, y);
        let right = match addr & 0b0000_0000_0000_0011 {
            0 | 1 => false,
            2 | 3 => true,
            _ => unreachable!()
        };
        let bottom = match addr & 0b0000_0000_0111_1111 {
            0 ... 0x3F => false,
            0x40 ... 0x7F => true,
            _ => unreachable!()
        };
        if right {
            attrs >>= 2
        }
        if bottom {
            attrs >>= 4
        }
        attrs & 0b0000_0011
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
    use crate::common::{Addressable, Shared, shared};
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

    fn test_ppu() -> (Shared<PpuMem>, Ppu) {
        let mapper = test_mapper(&[], test_pattern().as_slice());
        let ppu_mem = shared(PpuMem::new(mapper.clone()));
        let bus = Bus::new(ppu_mem.clone());
        let cpu = shared(Cpu::new(Box::new(CpuMem::new(mapper.clone(), bus)), true));
        (ppu_mem.clone(), Ppu::new(ppu_mem.clone(), cpu))
    }

    #[test]
    fn test_nametable_addr() {
        assert_eq!(Ppu::nametable_addr(0, 0), 0x2000);
        assert_eq!(Ppu::nametable_addr(31, 15), 0x21FF);
        assert_eq!(Ppu::nametable_addr(32, 0), 0x2400);
        assert_eq!(Ppu::nametable_addr(40, 20), 0x2688);
        assert_eq!(Ppu::nametable_addr(10, 40), 0x294A);
        assert_eq!(Ppu::nametable_addr(32, 31), 0x2C20);
    }

    #[test]
    fn test_pattern_overlay() {
        let (_ppumem, test_ppu) = test_ppu();
        let tile = test_ppu.pattern(1u16);
        assert_eq!(tile.len(), 8);
        for (idx, row) in tile.iter().enumerate() {
            assert_eq!(row.as_slice(), TILE[idx]);
        }
    }

    #[test]
    fn test_tile_attrs_read() {
        assert_eq!(Ppu::tile_attr_addr(0, 0), 0x23C0);
        assert_eq!(Ppu::tile_attr_addr(39, 0), 0x27C1);
        assert_eq!(Ppu::tile_attr_addr(10, 39), 0x2BD2);
    }

    #[test]
    fn test_tile_read_addrs() {
        let (ppu_mem, test_ppu) = test_ppu();
        {
            let mut borrowed = (*ppu_mem).borrow_mut();
            // (0, 0)
            borrowed.set(0x2000, 0xAE);
            borrowed.set(0x23C0, 0xA7);

            // (40, 0)
            borrowed.set(0x2408, 0xBC);
            borrowed.set(0x27C2, 0xA7);

            // (10, 39)
            borrowed.set(0x292A, 0x1F);
            borrowed.set(0x2BD2, 0xA7);
        }

        let mut pattern_num = test_ppu.tile_pattern_num(0, 0);
        assert_eq!(pattern_num, 0xAE);
        let mut pattern_attr_addr = test_ppu.tile_colorset(0, 0);
        assert_eq!(pattern_attr_addr, 3);

        pattern_num = test_ppu.tile_pattern_num(40, 0);
        assert_eq!(pattern_num, 0xBC);
        pattern_attr_addr = test_ppu.tile_colorset(40, 0);
        assert_eq!(pattern_attr_addr, 3);

        pattern_num = test_ppu.tile_pattern_num(10, 39);
        assert_eq!(pattern_num, 0x1F);
        pattern_attr_addr = test_ppu.tile_colorset(10, 39);
        assert_eq!(pattern_attr_addr, 1);

        // count through all the tiles under $A7 in (0, 0)
        assert_eq!(test_ppu.tile_colorset(0, 0), 3);
        assert_eq!(test_ppu.tile_colorset(1, 0), 3);
        assert_eq!(test_ppu.tile_colorset(2, 0), 1);
        assert_eq!(test_ppu.tile_colorset(3, 0), 1);
        assert_eq!(test_ppu.tile_colorset(0, 1), 3);
        assert_eq!(test_ppu.tile_colorset(1, 1), 3);
        assert_eq!(test_ppu.tile_colorset(2, 1), 1);
        assert_eq!(test_ppu.tile_colorset(3, 1), 1);
        assert_eq!(test_ppu.tile_colorset(0, 2), 2);
        assert_eq!(test_ppu.tile_colorset(1, 2), 2);
        assert_eq!(test_ppu.tile_colorset(2, 2), 2);
        assert_eq!(test_ppu.tile_colorset(3, 2), 2);
        assert_eq!(test_ppu.tile_colorset(0, 3), 2);
        assert_eq!(test_ppu.tile_colorset(1, 3), 2);
        assert_eq!(test_ppu.tile_colorset(2, 3), 2);
        assert_eq!(test_ppu.tile_colorset(3, 3), 2);
    }
}
