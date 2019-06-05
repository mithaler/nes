extern crate clap;
extern crate sdl2;

use std::error::Error;
use std::fs;
use std::str;

use clap::{App, Arg};
use sdl2::EventPump;
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::pixels::{Color, PixelFormatEnum};
use sdl2::render::{Canvas, Texture, TextureAccess};
use sdl2::video::Window;

use crate::bus::Bus;
use crate::common::{Clocked, shared, Shared};
use crate::controllers::Controllers;
use crate::cpu::Cpu;
use crate::mappers::mapper;
use crate::memory::{CpuMem, Mem, PpuMem};
use crate::ppu::Ppu;

mod bus;
mod cpu;
mod common;
mod controllers;
mod mappers;
mod memory;
mod ppu;

const WIDTH: u32 = 256;
const HEIGHT: u32 = 240;

struct Context<'a> {
    canvas: Canvas<Window>,
    cpu: Shared<Cpu>,
    controllers: Shared<Controllers>,
    texture: Texture<'a>,
    ppu: Ppu,
    event_pump: EventPump
}

fn main() -> Result<(), Box<Error>> {
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

    let controllers = shared(Controllers::new());
    let mapper = mapper(header, rom_sections);
    let ppu_mem = shared(PpuMem::new(mapper.clone()));
    let bus = Bus::new(ppu_mem.clone(), controllers.clone());
    let cpu_mem = Box::new(CpuMem::new(mapper, bus));

    let cpu = shared(Cpu::new(cpu_mem, matches.is_present("test_mode")));
    let ppu = Ppu::new(ppu_mem.clone(), cpu.clone());

    // Canvas setup
    let sdl_context = sdl2::init()?;
    let video_subsystem = sdl_context.video()?;
    let event_pump = sdl_context.event_pump()?;

    let window = video_subsystem.window("NES", WIDTH, HEIGHT)
        .position_centered()
        .build()
        .unwrap();

    let mut canvas = window.into_canvas().build()?;
    let creator = canvas.texture_creator();
    let texture = creator.create_texture(
        PixelFormatEnum::RGB24,
        TextureAccess::Streaming,
        WIDTH, HEIGHT
    )?;

    canvas.set_draw_color(Color::RGB(0, 255, 255));
    canvas.clear();

    let mut context = Context {event_pump, texture, cpu, ppu, controllers, canvas};

    let mut odd_frame = false;
    let mut running = true;
    while running {
        // number of PPU cycles
        let cycle_count = match odd_frame {
            true => 89340,
            false => 89340 // ??
        };
        running = render_frame(&mut context, cycle_count)?;
        odd_frame = !odd_frame;
    }
    Ok(())
}

fn render_frame(context: &mut Context, cycles: u32) -> Result<bool, Box<Error>> {
    for i in 0..=cycles {
        for event in context.event_pump.poll_iter() {
            match event {
                Event::Quit {..} |
                Event::KeyDown { keycode: Some(Keycode::Escape), .. } => return Ok(false),
                Event::KeyDown { keycode: Some(_), .. } => context.controllers.borrow_mut().event(event),
                Event::KeyUp { keycode: Some(_), .. } => context.controllers.borrow_mut().event(event),
                _ => {}
            }
        }

        if i % 12 == 0 {
            context.cpu.borrow_mut().tick();
        }
        if i % 4 == 0 {
            context.ppu.tick();
        }
    }

    context.texture.update(None, context.ppu.frame(), WIDTH as usize)?;
    context.canvas.copy(&context.texture, None, None)?;
    context.canvas.present();

    Ok(true)
}
