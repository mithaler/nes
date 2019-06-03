mod nrom;

use crate::common::{shared, Shared};

use nrom::Nrom;

pub type Mapper = Shared<Mapping>;

pub trait Mapping {
    fn get_cpu_space(&self, addr: u16) -> u8;
    fn set_cpu_space(&mut self, addr: u16, value: u8);

    fn get_ppu_space(&self, addr: u16) -> u8;
    fn set_ppu_space(&mut self, addr: u16, value: u8);
}

fn join_nibbles(contains_msb: u8, contains_lsb: u8) -> u8 {
    (contains_msb & 0xF0) | (contains_lsb >> 4)
}

pub fn mapper(header: &[u8], rom_sections: &[u8]) -> Mapper {
    let mapper_num = join_nibbles(header[5], header[4]);
    println!("Mapper number: {:?}", mapper_num);
    shared(match mapper_num {
        0 => Nrom::new(header, rom_sections),
        _ => unimplemented!("Mapper {:?} unimplemented", mapper_num),
    })
}

#[cfg(test)]
pub fn test_mapper(prg_rom: &[u8], chr_rom: &[u8]) -> Mapper {
    shared(Nrom::test_mapper(prg_rom, chr_rom))
}
