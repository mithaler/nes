use std::env;
use std::fs;
use std::str;

mod cpu;
mod common;
mod mappers;
mod memory;
mod ppu;

use crate::cpu::Cpu;
use crate::ppu::Ppu;
use crate::mappers::{mapper};
use crate::memory::{Mem};
use crate::common::Clocked;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    let rom: Mem = Box::new(fs::read(&args[1])?);

    let (header, rom_sections) = rom.split_at(16);
    assert_eq!(str::from_utf8(&header[0..4]).unwrap(), "NES\u{1a}", "Not a NES ROM!");

    let mapper = mapper(header, rom_sections);
    let mut cpu = Cpu::new(mapper.clone());
    let mut ppu = Ppu::new(mapper.clone());
    // TODO figure out pausing
    loop {
        cpu.tick();
    }
}
