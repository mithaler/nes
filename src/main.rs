use std::fs;
use std::str;

mod bus;
mod cpu;
mod common;
mod mappers;
mod memory;
mod ppu;

extern crate clap;

use clap::{Arg, App};

use crate::bus::{Bus};
use crate::common::{Clocked, shared};
use crate::cpu::Cpu;
use crate::mappers::mapper;
use crate::memory::{CpuMem, Mem, PpuMem};
use crate::ppu::Ppu;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let matches = App::new("nes")
        .version("0.1")
        .author("Michael Louis Thaler <michael.louis.thaler@gmail.com>")
        .about("Plays NES games")
        .arg(Arg::with_name("ROM_FILE")
            .help("Sets the ROM file to use")
            .required(true)
            .index(1))
        .arg(Arg::with_name("test_mode")
            .short("t")
            .help("Enables test mode"))
        .get_matches();

    let rom: Mem = Box::new(fs::read(matches.value_of("ROM_FILE").unwrap())?);

    let (header, rom_sections) = rom.split_at(16);
    assert_eq!(str::from_utf8(&header[0..4]).unwrap(), "NES\u{1a}", "Not a NES ROM!");

    let mapper = mapper(header, rom_sections);
    let ppu_mem = shared(PpuMem::new(mapper.clone()));
    let bus = Bus::new(ppu_mem.clone());
    let cpu_mem = Box::new(CpuMem::new(mapper, bus));

    let cpu = shared(Cpu::new(cpu_mem, matches.is_present("test_mode")));
    let mut ppu = Ppu::new(ppu_mem.clone(), cpu.clone());
    // TODO figure out pausing
    loop {
        for i in 0..=24 {
            if i % 12 == 0 {
                cpu.borrow_mut().tick();
            }
            if i % 4 == 0 {
                ppu.tick();
            }
        }
    }
}
