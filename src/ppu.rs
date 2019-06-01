use crate::mappers::Mapper;
use crate::memory::PpuMem;

pub struct Ppu {
    mem: PpuMem,
}

type Tile = Vec<Vec<u8>>;

impl Ppu {
    pub fn new(mapper: Mapper) -> Ppu {
        // startup state: https://wiki.nesdev.com/w/index.php/PPU_power_up_state
        Ppu {
            mem: PpuMem::new(mapper),
        }
    }

    fn pattern(&self, num: u16) -> Tile {
        let pattern = self.mem.pattern(num);
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
}

#[cfg(test)]
mod tests {
    use super::Ppu;
    use crate::mappers::test_mapper;

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

    #[test]
    fn pattern_overlay() {
        let test_ppu = Ppu::new(test_mapper(&[], test_pattern().as_slice()));
        let tile = test_ppu.pattern(1u16);
        assert_eq!(tile.len(), 8);
        for (idx, row) in tile.iter().enumerate() {
            assert_eq!(row.as_slice(), TILE[idx]);
        }
    }
}
