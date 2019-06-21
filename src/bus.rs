use crate::common::{Addressable, Shared, shared, join_bytes};
use crate::apu::Apu;
use crate::controllers::Controllers;
use crate::memory::PpuMem;

// TODO: this and PPUSCROLL status are somehow the same thing, but I'm really confused about how.
enum AddressLatchStatus {
    Empty,
    HoldingHighByte(u8),
}

pub type CpuBus = Shared<Bus>;

pub struct Bus {
    oamaddr: u8,  // $2003

    address_latch_status: AddressLatchStatus,
    last_written: u8,
    ppu_write_addr: u16,
    ppudata_read_buffer: u8,

    apu: Shared<Apu>,
    ppu_mem: Shared<PpuMem>,
    controllers: Shared<Controllers>
}

use AddressLatchStatus::*;

impl Bus {
    pub fn new(apu: Shared<Apu>, ppu_mem: Shared<PpuMem>, controllers: Shared<Controllers>) -> CpuBus {
        shared(Bus {
            oamaddr: 0,

            address_latch_status: Empty,
            last_written: 0,
            ppu_write_addr: 0,
            ppudata_read_buffer: 0,

            apu,
            ppu_mem,
            controllers
        })
    }

    fn advance_write_addr(&mut self) {
        match self.ppu_mem.borrow().get_ppuctrl().addr_increment_down {
            true => self.ppu_write_addr += 0x20,  // go down
            false => self.ppu_write_addr += 1  // go right
        }
    }

    fn get_ppustatus(&mut self) -> u8 {
        self.address_latch_status = Empty;
        let ppustatus = self.ppu_mem.borrow().get_ppustatus();
        ppustatus | self.last_written
    }

    // https://wiki.nesdev.com/w/index.php/PPU_registers#The_PPUDATA_read_buffer_.28post-fetch.29
    fn get_ppudata(&mut self) -> u8 {
        let (out, addr) = if (0x3F00..=0x3FFF).contains(&self.ppu_write_addr) {
            (self.ppu_mem.borrow().get(self.ppu_write_addr),
             if self.ppu_write_addr >= 0x3000 {
                 self.ppu_write_addr - 0x1000
             } else {
                 self.ppu_write_addr
             }
            )
        } else {
            (self.ppudata_read_buffer, self.ppu_write_addr)
        };
        self.ppudata_read_buffer = self.ppu_mem.borrow().get(addr);
        self.advance_write_addr();
        out
    }

    fn get_oamdata(&mut self) -> u8 {
        self.ppu_mem.borrow_mut().borrow_oam()[self.oamaddr as usize]
    }

    fn set_ppuctrl(&mut self, value: u8) {
        debug!("PPUCTRL set: {:#010b}", value);
        self.ppu_mem.borrow_mut().set_ppuctrl(value);
    }

    fn set_ppumask(&mut self, value: u8) {
        debug!("PPUMASK set: {:#010b}", value);
        self.ppu_mem.borrow_mut().set_ppumask(value);
    }

    fn set_oamaddr(&mut self, value: u8) {
        self.oamaddr = value;
    }

    fn set_oamdata(&mut self, value: u8) {
        self.ppu_mem.borrow_mut().set_oam(self.oamaddr, value);
        self.oamaddr = self.oamaddr.wrapping_add(1);
    }

    fn set_ppuscroll(&mut self, value: u8) {
        debug!("PPUSCROLL write: {:02X?}", value);
        // I am really not sure if this interaction between PPUADDR and PPUSCROLL is right :(
        self.address_latch_status = match self.address_latch_status {
            Empty => {self.ppu_mem.borrow_mut().scroll_x = value; HoldingHighByte(value)},
            HoldingHighByte(_) => {self.ppu_mem.borrow_mut().scroll_y = value; Empty}
        }
    }

    fn set_ppuaddr(&mut self, value: u8) {
        self.address_latch_status = match self.address_latch_status {
            Empty => HoldingHighByte(value),
            HoldingHighByte(hi) => { self.ppu_write_addr = join_bytes(hi, value); Empty },
        }
    }

    fn set_ppudata(&mut self, value: u8) {
        let mirrored = self.ppu_write_addr & 0b0011_1111_1111_1111;
        debug!("VRAM write: {:02X?} to {:04X?}", value, mirrored);
        self.ppu_mem.borrow_mut().set(mirrored, value);
        self.advance_write_addr();
    }

    /// Special CPU operation that writes directly to PPU OAM memory.
    pub fn set_oamdma(&mut self, oam_page: &[u8]) {
        self.ppu_mem.borrow_mut().set_oamdma(oam_page);
    }

    pub fn get(&mut self, register: u16) -> u8 {
        match register {
            0x2000 => panic!("PPUCTRL not readable by CPU!"),
            0x2001 => 0,  // I guess this should return the latch value, I dunno
            0x2002 => self.get_ppustatus(),
            0x2003 => self.oamaddr,
            0x2004 => self.get_oamdata(),
            0x2005 => panic!("PPUSCROLL not readable by CPU!"),
            0x2006 => panic!("PPUADDR not readable by CPU!"),
            0x2007 => self.get_ppudata(),

            0x4000 ... 0x4015 => panic!("APU registers not readable by CPU!"),

            0x4016 => self.controllers.borrow_mut().report_controller_1(),
            0x4017 => self.controllers.borrow_mut().report_controller_2(),

            _ => { warn!("Unimplemented register read: {:04X?}", register); 0},
        }
    }

    pub fn set(&mut self, register: u16, value: u8) {
        self.last_written = value & 0b0001_1111;
        match register {
            0x2000 => self.set_ppuctrl(value),
            0x2001 => self.set_ppumask(value),
            0x2002 => panic!("PPUSTATUS not writable by CPU!"),
            0x2003 => self.set_oamaddr(value),
            0x2004 => self.set_oamdata(value),
            0x2005 => self.set_ppuscroll(value),
            0x2006 => self.set_ppuaddr(value),
            0x2007 => self.set_ppudata(value),

            0x4000 ... 0x4013 | 0x4015 | 0x4017 => self.apu.borrow_mut().set_register(register, value),

            0x4014 => panic!("Don't write to $4014, call set_oamdma instead!"),
            0x4016 => self.controllers.borrow_mut().set_polling(if value != 0 {true} else {false}),

            _ => warn!("Unimplemented register write: {:04X?} -> {:02X?}", register, value),
        }
    }
}
