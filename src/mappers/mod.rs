mod nrom;

use crate::common::{shared, Shared};

use nrom::Nrom;

pub type Mapper = Shared<Mapping>;

pub trait Mapping {
    fn get_cpu_space(&self, addr: u16) -> u8;
    fn set_cpu_space(&mut self, addr: u16, value: u8);
    fn get_cpu_page(&self, start_addr: u16) -> &[u8];

    fn get_ppu_space(&self, addr: u16) -> u8;
    fn set_ppu_space(&mut self, addr: u16, value: u8);
}

#[derive(Debug)]
pub enum NametableMirror {
    Horizontal,
    Vertical
}

impl NametableMirror {
    fn mirrored_addr(&self, addr: u16) -> usize {
        usize::from(match self {
            NametableMirror::Horizontal => {
                match addr {
                    0x2000 ... 0x23FF | 0x2800 ... 0x2BFF => addr,
                    0x2400 ... 0x27FF | 0x2C00 ... 0x2EFF => addr & 0b1111_1011_1111_1111,
                    _ => unreachable!(),
                }
            },
            NametableMirror::Vertical => {
                match addr {
                    0x2000 ... 0x23FF | 0x2400 ... 0x27FF => addr,
                    0x2800 ... 0x2BFF | 0x2C00 ... 0x2EFF => addr & 0b1111_0111_1111_1111,
                    _ => unreachable!(),
                }
            }
        })
    }
}

pub fn mapper(header: &[u8], rom_sections: &[u8]) -> Mapper {
    let mapper_num = header[7] & 0b1111_0000 | ((header[6] & 0b1111_0000) >> 4);
    println!("Mapper number: {:?}", mapper_num);
    shared(match mapper_num {
        0 => Nrom::new(header, rom_sections),
        _ => unimplemented!("Mapper {:?}", mapper_num),
    })
}

#[cfg(test)]
pub fn test_mapper(prg_rom: &[u8], chr_rom: &[u8]) -> Mapper {
    shared(Nrom::test_mapper(prg_rom, chr_rom))
}

#[cfg(test)]
mod tests {
    use super::NametableMirror;

    #[test]
    fn test_horizontal_mirroring() {
        let mirror = NametableMirror::Horizontal;
        assert_eq!(mirror.mirrored_addr(0x2000), 0x2000);
        assert_eq!(mirror.mirrored_addr(0x2300), 0x2300);
        assert_eq!(mirror.mirrored_addr(0x24A0), 0x20A0);
        assert_eq!(mirror.mirrored_addr(0x284B), 0x284B);
        assert_eq!(mirror.mirrored_addr(0x2D20), 0x2920);
    }

    #[test]
    fn test_vertical_mirroring() {
        let mirror = NametableMirror::Vertical;
        assert_eq!(mirror.mirrored_addr(0x2000), 0x2000);
        assert_eq!(mirror.mirrored_addr(0x2300), 0x2300);
        assert_eq!(mirror.mirrored_addr(0x24A0), 0x24A0);
        assert_eq!(mirror.mirrored_addr(0x284B), 0x204B);
        assert_eq!(mirror.mirrored_addr(0x2D20), 0x2520);
    }
}
