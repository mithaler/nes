// Mapper 066: https://wiki.nesdev.com/w/index.php/GxROM

use crate::mappers::{NametableMirror, HeaderAttributes, Mapping};
use crate::memory::{Mem, initialized_mem, mem};
use crate::common::{Shared, shared};

pub struct Gxrom {
    prg_bank: u16,
    chr_bank: u16,
    prg_ram: Option<Mem>,
    prg_rom: Mem,
    chr_rom: Mem,
    internal_vram: Mem,
    nametable_mirror: NametableMirror
}

impl Gxrom {
    pub fn new(header: &[u8], rom_sections: &[u8]) -> Shared<Gxrom> {
        let attrs = HeaderAttributes::from_headers(header);
        let prg_rom = mem(&rom_sections[0 .. attrs.prg_rom_size * 0x4000]);
        let chr_rom = mem(&rom_sections[attrs.prg_rom_size * 0x4000 .. (attrs.prg_rom_size * 0x4000) + attrs.chr_rom_size * 0x2000]);
        let prg_ram = match attrs.prg_ram {
            true => Some(initialized_mem(0x2000)),
            false => None
        };

        shared(Gxrom {
            prg_bank: 0,
            chr_bank: 0,
            prg_rom,
            chr_rom,
            prg_ram,
            internal_vram: initialized_mem(0x1000),
            nametable_mirror: attrs.nametable_mirror
        })
    }

    fn mirrored_addr(&self, addr: u16) -> usize {
        self.nametable_mirror.mirrored_addr(addr) - 0x2000
    }

    /// Given a requested address, returns the actual position in the ROM after bankswitching.
    fn prg_rom_addr(addr: u16, prg_bank: u16) -> usize {
        usize::from((addr - 0x8000) + (0x8000 * prg_bank))
    }

    fn chr_rom_addr(addr: u16, chr_bank: u16) -> usize {
        usize::from(addr + (0x2000 * chr_bank))
    }
}

impl Mapping for Gxrom {
    fn get_cpu_space(&self, addr: u16) -> u8 {
        match addr {
            0x0000...0x401F => panic!("Address {:X?} not handled by mappers!", addr),
            0x4020...0x5FFF => panic!("Address {:X?} unused by this mapper!", addr),
            0x6000...0x7FFF => self.prg_ram.as_ref().expect("ROM without RAM tried to read it!")[(addr - 0x6000) as usize],
            0x8000...0xFFFF => self.prg_rom[Gxrom::prg_rom_addr(addr, self.prg_bank)],
        }
    }

    fn set_cpu_space(&mut self, addr: u16, value: u8) {
        match addr {
            0x8000 ... 0xFFFF => {
                self.chr_bank = (value & 0b0000_0011) as u16;
                self.prg_bank = (value >> 4) as u16;
            },
            _ => panic!("CPU write to unexpected addr: {:X?} -> {:X?}", addr, value),
        }
    }

    fn get_cpu_page(&self, start_addr: u16) -> &[u8] {
        unimplemented!()
    }

    fn get_ppu_space(&self, addr: u16) -> u8 {
        match addr {
            0x0 ... 0x1FFF => self.chr_rom[Gxrom::chr_rom_addr(addr, self.chr_bank)],
            0x2000 ... 0x2FFF => self.internal_vram[self.mirrored_addr(addr)],
            0x3000 ... 0x3EFF => self.internal_vram[(addr - 0x3000) as usize],
            _ => unimplemented!()
        }
    }

    fn set_ppu_space(&mut self, addr: u16, value: u8) {
        match addr {
            0x0 ... 0x1FFF => self.chr_rom[Gxrom::chr_rom_addr(addr, self.chr_bank)] = value,
            0x2000 ... 0x2FFF => {
                let addr = self.mirrored_addr(addr);
                self.internal_vram[addr] = value
            },
            0x3000 ... 0x3EFF => self.internal_vram[(addr - 0x3000) as usize] = value,
            _ => unimplemented!()
        }
    }
}
