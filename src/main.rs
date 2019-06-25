#[macro_use] extern crate bitflags;
#[macro_use] extern crate log;
extern crate clap;
extern crate sdl2;
extern crate simplelog;

use std::error::Error;
use std::fs;
use std::str;
use std::thread::sleep;
use std::time::{Duration, Instant};

use clap::{App, Arg};
use log::LevelFilter;
use sdl2::audio::{AudioQueue, AudioSpecDesired};
use sdl2::event::Event;
use sdl2::EventPump;
use sdl2::keyboard::Keycode;
use sdl2::pixels::{Color, PixelFormatEnum};
use sdl2::render::{Canvas, Texture, TextureAccess};
use sdl2::video::Window;
use simplelog::{Config, TermLogger};

use crate::apu::Apu;
use crate::bus::Bus;
use crate::common::{Clocked, SAMPLES_PER_FRAME, shared, Shared, Irq};
use crate::controllers::Controllers;
use crate::cpu::Cpu;
use crate::mappers::mapper;
use crate::memory::{CpuMem, Mem, PpuMem};
use crate::ppu::Ppu;

mod apu;
mod bus;
mod cpu;
mod common;
mod controllers;
mod mappers;
mod memory;
mod ppu;

const WIDTH: u32 = 256;
const HEIGHT: u32 = 240;
const TARGET_DURATION: Duration = Duration::from_millis(1000 / 60);

struct Context<'a> {
    canvas: Canvas<Window>,
    cpu: Shared<Cpu>,
    controllers: Shared<Controllers>,
    texture: Texture<'a>,
    audio_queue: AudioQueue<f32>,
    ppu: Ppu,
    apu: Shared<Apu>,
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
        .arg(Arg::with_name("debug logging")
            .short("d")
            .help("Enables debug logging"))
        .arg(Arg::with_name("ui scale")
            .short("s")
            .takes_value(true)
            .help("UI scale factor (default 3)"))
        .get_matches();

    let loglevel = match matches.is_present("debug logging") {
        true => LevelFilter::Debug,
        false => LevelFilter::Info
    };
    TermLogger::init(loglevel, Config::default())?;

    let rom: Mem = Box::new(fs::read(matches.value_of("ROM_FILE").unwrap())?);

    let (header, rom_sections) = rom.split_at(16);
    assert_eq!(str::from_utf8(&header[0..4]).unwrap(), "NES\u{1a}", "Not a NES ROM!");

    let controllers = shared(Controllers::new());
    let mapper = mapper(header, rom_sections);
    let ppu_mem = shared(PpuMem::new(mapper.clone()));
    let apu = Apu::new(mapper.clone());
    let bus = Bus::new(apu.clone(), ppu_mem.clone(), controllers.clone());
    let cpu_mem = Box::new(CpuMem::new(mapper, bus));

    let cpu = shared(Cpu::new(cpu_mem, matches.is_present("test_mode")));
    let ppu = Ppu::new(ppu_mem.clone(), cpu.clone());

    // Canvas setup
    let sdl_context = sdl2::init()?;
    let video_subsystem = sdl_context.video()?;
    let event_pump = sdl_context.event_pump()?;

    let ui_scale_factor = matches.value_of("ui scale").unwrap_or("3").parse::<u32>()?;
    let window = video_subsystem.window("NES", WIDTH * ui_scale_factor, HEIGHT * ui_scale_factor)
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

    let audio_spec = AudioSpecDesired {
        samples: Some(SAMPLES_PER_FRAME as u16),
        channels: Some(1),
        freq: Some(44100) // Hz
    };
    let audio_queue = sdl_context.audio()?.open_queue(None, &audio_spec)?;
    audio_queue.resume();

    let mut context = Context {event_pump, texture, canvas, audio_queue, cpu, ppu, apu, controllers};
    frame_loop(&mut context)
}

fn frame_loop(mut context: &mut Context) -> Result<(), Box<Error>> {
    let mut odd_frame = false;
    let mut running = true;
    let mut turbo = false;
    while running {
        let before = Instant::now();
        for event in context.event_pump.poll_iter() {
            match event {
                Event::Quit {..} |
                Event::KeyDown { keycode: Some(Keycode::Escape), .. } => running = false,
                Event::KeyDown { keycode: Some(Keycode::F7), .. } => context.cpu.borrow_mut().reset(),
                Event::KeyDown { keycode: Some(Keycode::Backquote), .. } => turbo = true,
                Event::KeyUp { keycode: Some(Keycode::Backquote), .. } => turbo = false,
                Event::KeyDown { keycode: Some(_), .. } => context.controllers.borrow_mut().event(event),
                Event::KeyUp { keycode: Some(_), .. } => context.controllers.borrow_mut().event(event),
                _ => {}
            }
        }
        let ppu_cycle_count = match odd_frame {
            true => 89342,
            false => 89342 // ??
        };
        render_frame(&mut context, ppu_cycle_count)?;
        odd_frame = !odd_frame;

        if !turbo {
            let after = Instant::now();
            if let Some(to_sleep) = TARGET_DURATION.checked_sub(after - before) {
                sleep(to_sleep);
            }
        }
    }
    Ok(())
}

fn render_frame(context: &mut Context, ppu_cycles: u32) -> Result<(), Box<Error>> {
    for i in 0..ppu_cycles {
        if (i % 3) == 0 {
            context.cpu.borrow_mut().tick();
            context.apu.borrow_mut().tick();

            if context.apu.borrow().irq() {
                // I think this is wrong; really this should be setting a flag for next cycle
                context.cpu.borrow_mut().irq();
            }
        }
        context.ppu.tick();
    }

    {
        let mut apu = context.apu.borrow_mut();
        let samples = apu.samples();
        context.audio_queue.queue(samples);
        samples.clear();
    }
    context.texture.update(None, context.ppu.frame(), (WIDTH * 3) as usize)?;
    context.canvas.copy(&context.texture, None, None)?;
    context.canvas.present();

    Ok(())
}
