use std::env;
use std::fs;
use std::str;

mod cpu;
mod common;
mod memory;
mod ppu;

use crate::cpu::Cpu;
use crate::ppu::Ppu;
use crate::memory::{Mem, mem};
use crate::common::Clocked;

/// Returns the PRG_ROM and CHR_ROM sections of the ROM.
/// See the [INES file format](https://wiki.nesdev.com/w/index.php/INES).
/// (NES 2.0 not yet supported.)
fn rom_sections(rom: Mem) -> (Mem, Mem) {
    let (header, rom_sections) = rom.split_at(16);
    assert_eq!(str::from_utf8(&header[0..4]).unwrap(), "NES\u{1a}", "Not a NES ROM!");

    let prg_size = usize::from(header[4]);
    let chr_size = usize::from(header[5]);

    let flags6 = &header[6];
    if (flags6 & 4) != 0 {
        // first 512 bytes are a trainer, wtf
        // shift rom_sections forward 512 bytes, i guess?
        panic!("omg i have no idea what to do with a trainer")
    }

    let (prg_rom, rom_sections) = rom_sections.split_at(0x4000 * prg_size);
    let (chr_rom, _rest) = rom_sections.split_at(0x2000 * chr_size);

    (mem(prg_rom), mem(chr_rom))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    let rom: Mem = Box::new(fs::read(&args[1])?);

    let (prg_rom, chr_rom) = rom_sections(rom);

    let mut cpu = Cpu::new(prg_rom);
    let mut ppu = Ppu::new(chr_rom);
    // TODO figure out pausing
    loop {
        cpu.tick();
    }
}
