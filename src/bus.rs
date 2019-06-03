use crate::common::{Addressable, Shared, shared, join_bytes};
use crate::memory::PpuMem;

enum AddressLatchStatus {
    Empty,
    HoldingHighByte(u8),
}

pub type CpuBus = Shared<Bus>;

pub struct Bus {
    oamaddr: u8,  // $2003
    oamdata: u8,  // $2004
    ppuscroll: u8,  // $2005
    ppuaddr: u8,  // $2006
    oamdma: u8,  // $2008

    address_latch_status: AddressLatchStatus,
    last_written: u8,
    ppu_write_addr: u16,
    ppu_mem: Shared<PpuMem>
}

use AddressLatchStatus::*;

impl Bus {
    pub fn new(ppu_mem: Shared<PpuMem>) -> CpuBus {
        shared(Bus {
            oamaddr: 0,
            oamdata: 0,
            ppuscroll: 0,
            ppuaddr: 0,
            oamdma: 0,

            address_latch_status: Empty,
            last_written: 0,
            ppu_write_addr: 0,
            ppu_mem
        })
    }

    fn advance_write_addr(&mut self) {
        match (self.ppu_mem.borrow().get_ppuctrl() & 0b0000_0100) == 1 {
            true => self.ppu_write_addr += 0x20,  // go down
            false => self.ppu_write_addr += 1  // go right
        }
    }

    fn get_ppustatus(&mut self) -> u8 {
        self.address_latch_status = Empty;
        let ppustatus = self.ppu_mem.borrow().get_ppustatus();
        ppustatus | self.last_written
    }

    fn get_ppudata(&mut self) -> u8 {
        let out = self.ppu_mem.borrow().get(self.ppu_write_addr);
        self.advance_write_addr();
        out
    }

    fn set_ppuctrl(&mut self, value: u8) {
        self.ppu_mem.borrow_mut().set_ppuctrl(value);
    }

    fn set_ppumask(&mut self, value: u8) {
        self.ppu_mem.borrow_mut().set_ppumask(value);
    }

    fn set_oamaddr(&mut self, value: u8) {
        self.oamaddr = value;
    }

    fn set_oamdata(&mut self, value: u8) {
        self.oamdata = value;
    }

    fn set_ppuscroll(&mut self, value: u8) {
        self.ppuscroll = value;
    }

    fn set_ppuaddr(&mut self, value: u8) {
        self.ppuaddr = value;
        self.address_latch_status = match self.address_latch_status {
            Empty => HoldingHighByte(value),
            HoldingHighByte(hi) => { self.ppu_write_addr = join_bytes(hi, value); Empty },
        }
    }

    fn set_ppudata(&mut self, value: u8) {
        println!("VRAM write: {:02X?} to {:04X?}", value, self.ppu_write_addr);
        self.ppu_mem.borrow_mut().set(self.ppu_write_addr, value);
        self.advance_write_addr();
    }

    fn set_oamdma(&mut self, value: u8) {
        self.oamdma = value;
    }

    pub fn get(&mut self, register: u16) -> u8 {
        match register {
            0x2000 => panic!("PPUCTRL not readable by CPU!"),
            0x2001 => panic!("PPUMASK not readable by CPU!"),
            0x2002 => self.get_ppustatus(),
            0x2003 => self.oamaddr,
            0x2004 => self.oamdata,
            0x2005 => self.ppuscroll,
            0x2006 => self.ppuaddr,
            0x2007 => self.get_ppudata(),
            0x2008 => self.oamdma,
            _ => panic!("Bad bus addr: {:04X?}", register)
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
            0x2008 => self.set_oamdma(value),
            _ => panic!("Bad bus addr: {:04X?}", register)
        }
    }
}
