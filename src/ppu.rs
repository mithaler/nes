use crate::common::{Clocked, Shared, Addressable};
use crate::cpu::Cpu;
use crate::memory::PpuMem;

pub struct Ppu {
    mem: Shared<PpuMem>,
    cpu: Shared<Cpu>,

    tile: Option<Tile>,
    framebuffer_index: usize,
    framebuffer: [u8; (256 * 240 * 3)],
    scanline: i16,  // -1 - 261
    tick: u16  // 0 - 340
}

// R, G, B
type Color = (u8, u8, u8);

// http://www.firebrandx.com/nespalette.html
const COLORS: [Color; 64] = [
    // 0x
    (0x6a, 0x6d, 0x6a),
    (0x00, 0x13, 0x80),
    (0x1e, 0x00, 0x8a),
    (0x39, 0x00, 0x7a),
    (0x55, 0x00, 0x56),
    (0x5a, 0x00, 0x18),
    (0x4f, 0x10, 0x00),
    (0x3d, 0x1c, 0x00),
    (0x25, 0x32, 0x00),
    (0x00, 0x3d, 0x00),
    (0x00, 0x40, 0x00),
    (0x00, 0x39, 0x24),
    (0x00, 0x2e, 0x55),
    (0x00, 0x00, 0x00),
    (0x00, 0x00, 0x00),
    (0x00, 0x00, 0x00),

    // 1x
    (0xb9, 0xbc, 0xb9),
    (0x18, 0x50, 0xc7),
    (0x4b, 0x30, 0xe3),
    (0x73, 0x22, 0xd6),
    (0x95, 0x1f, 0xa9),
    (0x9d, 0x28, 0x5c),
    (0x98, 0x37, 0x00),
    (0x7f, 0x4c, 0x00),
    (0x5e, 0x64, 0x00),
    (0x22, 0x77, 0x00),
    (0x02, 0x7e, 0x02),
    (0x00, 0x76, 0x45),
    (0x00, 0x6e, 0x8a),
    (0x00, 0x00, 0x00),
    (0x00, 0x00, 0x00),
    (0x00, 0x00, 0x00),

    // 2x
    (0xff, 0xff, 0xff),
    (0x68, 0xa6, 0xff),
    (0x8c, 0x9c, 0xff),
    (0xb5, 0x86, 0xff),
    (0xd9, 0x75, 0xfd),
    (0xe3, 0x77, 0xb9),
    (0xe5, 0x8d, 0x68),
    (0xd4, 0x9d, 0x29),
    (0xb3, 0xaf, 0x0c),
    (0x7b, 0xc2, 0x11),
    (0x55, 0xca, 0x47),
    (0x46, 0xcb, 0x81),
    (0x47, 0xc1, 0xc5),
    (0x4a, 0x4d, 0x4a),
    (0x00, 0x00, 0x00),
    (0x00, 0x00, 0x00),

    // 3x
    (0xff, 0xff, 0xff),
    (0xcc, 0xea, 0xff),
    (0xdd, 0xde, 0xff),
    (0xec, 0xda, 0xff),
    (0xf8, 0xd7, 0xfe),
    (0xfc, 0xd6, 0xf5),
    (0xfd, 0xdb, 0xcf),
    (0xf9, 0xe7, 0xb5),
    (0xf1, 0xf0, 0xaa),
    (0xda, 0xfa, 0xa9),
    (0xc9, 0xff, 0xbc),
    (0xc3, 0xfb, 0xd7),
    (0xc4, 0xf6, 0xf6),
    (0xbe, 0xc1, 0xbe),
    (0x00, 0x00, 0x00),
    (0x00, 0x00, 0x00),
];

struct Tile {
    pattern: Vec<Vec<u8>>,
    palette: [&'static Color; 4],
    num: u8,
    x: u8,
    y: u8,
}

fn color(id: u8) -> &'static Color {
    &COLORS[id as usize]
}

impl Ppu {
    pub fn new(ppu_mem: Shared<PpuMem>, cpu: Shared<Cpu>) -> Ppu {
        // startup state: https://wiki.nesdev.com/w/index.php/PPU_power_up_state
        Ppu {
            mem: ppu_mem,
            cpu,
            tile: None,
            framebuffer_index: 0,
            framebuffer: [0; (256 * 240 * 3)],  // 3 bytes per pixel
            scanline: -1,
            tick: 0,
        }
    }

    pub fn frame(&self) -> &[u8] {
        &self.framebuffer
    }

    fn pattern(&self, num: u8) -> Vec<Vec<u8>> {
        let mem = self.mem.borrow();

        let offset = if (mem.get_ppuctrl() & 0b0001_0000) != 0 {
            0x1000
        } else {
            0
        };
        let pattern = self.mem.borrow().pattern(num, offset);
        let iter = pattern.0.iter().zip(pattern.1.iter());

        let mut ret: Vec<Vec<u8>> = Vec::with_capacity(8);
        for (left, right) in iter {
            let mut row = Vec::with_capacity(8);
            let mut bitmask = 0b1000_0000u8;
            for _ in (0u8..8u8).rev() {
                let mut out = 0;
                if (left & bitmask) != 0 {
                    out += 1;
                }
                if (right & bitmask) != 0 {
                    out += 2
                }
                row.push(out);
                bitmask >>= 1;
            }
            ret.push(row);
        }
        ret
    }

    /// Modifies the base memory address for the nametable the (x, y) coordinates belong in;
    /// returns (x, y, base), where all three coordinates are indexed by the specific nametable.
    fn nametable_base_addr(mut x: u8, mut y: u8, mut base: u16) -> (u16, u16, u16) {
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
        (x as u16, y as u16, base)
    }

    /// Returns the memory address of the tile at coordinates (x, y) in the nametable.
    fn nametable_addr(x: u8, y: u8) -> u16 {
        let (x_within, y_within, base) = Ppu::nametable_base_addr(x, y, 0x2000);
        base + (x_within as u16) + ((y_within * 0x20) as u16)
    }

    /// Returns the memory address of the attribute byte for the nametable tile at (x, y).
    fn tile_attr_addr(x: u8, y: u8) -> u16 {
        let (x_within, y_within, base) = Ppu::nametable_base_addr(x, y, 0x23C0);
        base + (x_within / 4) + ((y_within / 4) * 0x8)
    }

    /// Returns the memory address of a color by pixel value (0-3), palette number (0-3),
    /// and whether it's a background (true) or sprite (false) palette.
    fn color_addr(pixel_value: u8, palette_num: u8, background: bool) -> u16 {
        if pixel_value == 0 {
            return 0x3F00;
        }
        let mut out = 0;
        out |= pixel_value;
        out |= palette_num << 2;
        if !background {
            out |= 0b0001_0000;
        }
        0x3F00 | (out as u16)
    }

    /// Given the coordinates of a tile in the nametable, returns the byte representing the
    /// tile in the nametable.
    fn tile_pattern_num(&self, x: u8, y: u8) -> u8 {
        let addr = Ppu::nametable_addr(x, y);
        self.mem.borrow().get(addr)
    }

    fn tile_colorset(&self, x: u8, y: u8) -> [&'static Color; 4] {
        let palette_base_addr = self.tile_colorset_base_addr(x, y);
        let mem = self.mem.borrow();
        [color(mem.get(0x3F00)),  // BG color
         color(mem.get(palette_base_addr)),  // palette color 1
         color(mem.get(palette_base_addr + 1)),  // palette color 2
         color(mem.get(palette_base_addr + 2))]  // palette color 3
    }

    /// Given the coordinates of a tile in the nametable, returns the address of
    /// the first color in its palette.
    fn tile_colorset_base_addr(&self, x: u8, y: u8) -> u16 {
        let mut attrs = self.mem.borrow().get(Ppu::tile_attr_addr(x, y));
        let addr = Ppu::nametable_addr(x, y);

        // Which box inside the attribute byte is it?
        // on the right?
        if (addr & 0b0000_0000_0000_0011) > 1 {
            attrs >>= 2
        }
        // on the bottom?
        if (addr & 0b0000_0000_0111_1111) >= 0x40 {
            attrs >>= 4
        }
        0x3F01 | (((attrs & 0b0000_0011) as u16) << 2)
    }

    fn curr_tile_coordinates(&self) -> (u8, u8) {
        ((self.tick / 8) as u8, (self.scanline / 8) as u8)
    }

    fn tile(&self, x: u8, y: u8) -> Tile {
        let num = self.tile_pattern_num(x, y);
        let palette = self.tile_colorset(x, y);
        let pattern = self.pattern(num);
        Tile {x, y, num, pattern, palette}
    }

    fn dummy_scanline(&mut self) {
        // TODO even/odd frame
        if self.tick == 1 {
            println!("-- EXITING VBLANK --");
            self.mem.borrow_mut().set_vblank(false);  // TODO sprite 0 hit, overflow
        }
        self.framebuffer_index = 0;
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

    fn update_tile(&mut self) {
        let (x, y) = self.curr_tile_coordinates();
        match &self.tile {
            None => self.tile = Some(self.tile(x, y)),
            Some(t) => {
                if t.x != x || t.y != y {
                    self.tile = Some(self.tile(x, y))
                }
            }
        }
    }

    fn render_background_pixel(&mut self) {
        self.update_tile();
        let tile = self.tile.as_ref().unwrap();
        let pixel = tile.pattern[(self.scanline % 8) as usize][(self.tick % 8) as usize];
        let color = tile.palette[pixel as usize];
        self.framebuffer[self.framebuffer_index] = color.0;
        self.framebuffer[self.framebuffer_index + 1] = color.1;
        self.framebuffer[self.framebuffer_index + 2] = color.2;
        self.framebuffer_index += 3;
    }

    fn render_background(&mut self) {
        match self.tick {
            0 => {},
            1 ... 256 => self.render_background_pixel(),
            _ => {},

        }
    }

    fn visible_scanline(&mut self) {
        let ppumask = self.mem.borrow().get_ppumask();
        if (ppumask & 0b00001000) != 0 {
            self.render_background();
        }
        if (ppumask & 0b00010000) != 0 {
            //self.render_sprite();
        }
        // TODO sprite priority?
    }

    fn render(&mut self) {
        match self.scanline {
            -1 => self.dummy_scanline(),
            0 ... 239 => self.visible_scanline(),
            240 => {},  // post-render
            241 ... 260 => self.vblank_scanline(),
            _ => unreachable!()
        }
        self.tick = match self.tick {
            t @ 0 ... 339 => t + 1,
            340 => {
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
    use crate::controllers::Controllers;
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
        chr_rom.splice(0x10..0x18, LEFT.iter().cloned());
        chr_rom.splice(0x18..0x1F, RIGHT.iter().cloned());
        assert_eq!(&chr_rom[0x10..0x18], LEFT);
        assert_eq!(&chr_rom[0x18..0x20], RIGHT);

        Box::new(chr_rom)
    }

    fn test_ppu() -> (Shared<PpuMem>, Ppu) {
        let mapper = test_mapper(&[], test_pattern().as_slice());
        let ppu_mem = shared(PpuMem::new(mapper.clone()));
        let bus = Bus::new(ppu_mem.clone(), shared(Controllers::new()));
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
        let tile = test_ppu.pattern(1);
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
        let mut pattern_attr_addr = test_ppu.tile_colorset_base_addr(0, 0);
        assert_eq!(pattern_attr_addr, 0x3F0D);

        pattern_num = test_ppu.tile_pattern_num(40, 0);
        assert_eq!(pattern_num, 0xBC);
        pattern_attr_addr = test_ppu.tile_colorset_base_addr(40, 0);
        assert_eq!(pattern_attr_addr, 0x3F0D);

        pattern_num = test_ppu.tile_pattern_num(10, 39);
        assert_eq!(pattern_num, 0x1F);
        pattern_attr_addr = test_ppu.tile_colorset_base_addr(10, 39);
        assert_eq!(pattern_attr_addr, 0x3F05);

        // count through all the tiles under $A7 in (0, 0)
        assert_eq!(test_ppu.tile_colorset_base_addr(0, 0), 0x3F0D);
        assert_eq!(test_ppu.tile_colorset_base_addr(1, 0), 0x3F0D);
        assert_eq!(test_ppu.tile_colorset_base_addr(2, 0), 0x3F05);
        assert_eq!(test_ppu.tile_colorset_base_addr(3, 0), 0x3F05);
        assert_eq!(test_ppu.tile_colorset_base_addr(0, 1), 0x3F0D);
        assert_eq!(test_ppu.tile_colorset_base_addr(1, 1), 0x3F0D);
        assert_eq!(test_ppu.tile_colorset_base_addr(2, 1), 0x3F05);
        assert_eq!(test_ppu.tile_colorset_base_addr(3, 1), 0x3F05);
        assert_eq!(test_ppu.tile_colorset_base_addr(0, 2), 0x3F09);
        assert_eq!(test_ppu.tile_colorset_base_addr(1, 2), 0x3F09);
        assert_eq!(test_ppu.tile_colorset_base_addr(2, 2), 0x3F09);
        assert_eq!(test_ppu.tile_colorset_base_addr(3, 2), 0x3F09);
        assert_eq!(test_ppu.tile_colorset_base_addr(0, 3), 0x3F09);
        assert_eq!(test_ppu.tile_colorset_base_addr(1, 3), 0x3F09);
        assert_eq!(test_ppu.tile_colorset_base_addr(2, 3), 0x3F09);
        assert_eq!(test_ppu.tile_colorset_base_addr(3, 3), 0x3F09);
    }
}
