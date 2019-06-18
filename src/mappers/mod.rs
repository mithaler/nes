use gxrom::Gxrom;
use nrom::Nrom;

use crate::common::Shared;
use crate::mappers::mmc1::{Mmc1, Uxrom};

mod nrom;  // 0
mod mmc1;  // 1
mod gxrom;  // 66

pub const fn kb(num: u16) -> usize {
    (num * 0x400) as usize
}

pub type Mapper = Shared<Mapping>;

pub struct HeaderAttributes {
    prg_rom_size: usize,  // in 16kb units
    chr_rom_size: usize,  // in 8kb units
    chr_ram_size: usize,  // in 8kb units
    nametable_mirror: NametableMirror,
    prg_ram: bool,
}

impl HeaderAttributes {
    pub fn from_headers(header: &[u8]) -> HeaderAttributes {
        let attrs = HeaderAttributes {
            prg_rom_size: header[4] as usize,
            chr_rom_size: header[5] as usize,
            chr_ram_size: ((64 << (header[11] & 0b0000_1111)) / 0x2000) as usize,
            prg_ram: (header[6] & 0b0000_0010) != 0,
            nametable_mirror: match (header[6] & 0b0000_0001) != 0 {
                true => NametableMirror::Vertical,
                false => NametableMirror::Horizontal
            }
        };
        info!(
            "PRG ROM: 0x{:X?}, CHR ROM: 0x{:X?}, CHR RAM: 0x{:X?}, contains PRG RAM: {:?}, nametable mirroring: {:?}",
            attrs.prg_rom_size * 0x4000,
            attrs.chr_rom_size * 0x2000,
            attrs.chr_ram_size * 0x2000,
            attrs.prg_ram,
            attrs.nametable_mirror
        );
        if (header[6] & 0b0000_1000) != 0 {
            unimplemented!("Trainer!?");
        }
        attrs
    }
}

pub trait Mapping {
    fn get_cpu_space(&self, addr: u16) -> u8;
    fn set_cpu_space(&mut self, addr: u16, value: u8);
    fn get_cpu_page(&self, start_addr: u16) -> &[u8];

    fn get_ppu_space(&self, addr: u16) -> u8;
    fn set_ppu_space(&mut self, addr: u16, value: u8);
}

pub trait Resolver {
    /// Takes an address directly from the way the CPU/PPU asked for it, and resolves it to
    /// an address within the memory actually stored by a mapper.
    fn resolve_addr(&self, addr: u16) -> usize;
}

#[derive(Debug)]
pub enum NametableMirror {
    Horizontal,
    Vertical,
    Single(u16)  // offset
}

impl Resolver for NametableMirror {
    fn resolve_addr(&self, addr: u16) -> usize {
        usize::from(match self {
            NametableMirror::Horizontal => {
                match addr {
                    0x2000 ... 0x23FF | 0x2800 ... 0x2BFF => addr,
                    0x2400 ... 0x27FF | 0x2C00 ... 0x2FFF => addr & 0b1111_1011_1111_1111,
                    _ => unreachable!("nametable addr {:0X?}", addr),
                }
            },
            NametableMirror::Vertical => {
                match addr {
                    0x2000 ... 0x23FF | 0x2400 ... 0x27FF => addr,
                    0x2800 ... 0x2BFF | 0x2C00 ... 0x2FFF => addr & 0b1111_0111_1111_1111,
                    _ => unreachable!("nametable addr {:0X?}", addr),
                }
            },
            NametableMirror::Single(offset) => {
                // we only care about position within the single nametable
                let position = addr & 0x03FF;
                position + 0x2000 + *offset
            }
        })
    }
}

pub fn mapper(header: &[u8], rom_sections: &[u8]) -> Mapper {
    let mapper_num = header[7] & 0b1111_0000 | ((header[6] & 0b1111_0000) >> 4);
    info!("Mapper number: {:?}", mapper_num);
    match mapper_num {
        0 => Nrom::new(header, rom_sections),
        1 => Mmc1::new(header, rom_sections),
        2 => Uxrom::new(header, rom_sections),
        66 => Gxrom::new(header, rom_sections),
        _ => unimplemented!("Mapper {:?}", mapper_num),
    }
}

#[cfg(test)]
pub fn test_mapper(prg_rom: &[u8], chr_rom: &[u8]) -> Mapper {
    Nrom::test_mapper(prg_rom, chr_rom)
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
