// Mapper 001: https://wiki.nesdev.com/w/index.php/MMC1
// Mapper 002: https://wiki.nesdev.com/w/index.php/UxROM

use crate::common::{Shared, shared};
use crate::memory::{Mem, mem, initialized_mem};
use crate::mappers::{NametableMirror, HeaderAttributes, Mapping, Resolver, kb};

const SHIFT_REGISTER_INITIAL: u8 = 0b0001_0000;

#[derive(Debug)]
enum PrgBankMode {
    Whole(usize),
    FirstFixed(usize),
    LastFixed(usize, usize)  // selected bank, total count of banks
}

impl Resolver for PrgBankMode {
    fn resolve_addr(&self, addr: u16) -> usize {
        let resolved = addr as usize - kb(32);
        match self {
            PrgBankMode::Whole(bank) => resolved + (kb(32) * *bank),
            PrgBankMode::FirstFixed(bank) => {
                if resolved < kb(16) {
                    resolved
                } else {
                    resolved + bank * kb(16)
                }
            },
            PrgBankMode::LastFixed(bank, count) => {
                if resolved < kb(16) {
                    resolved + bank * kb(16)
                } else {
                    resolved + (count - 1) * kb(16)
                }
            }
        }
    }
}

#[derive(Debug)]
enum ChrBankMode {
    Whole(usize),
    Separate(usize, usize)
}

impl Resolver for ChrBankMode {
    fn resolve_addr(&self, addr: u16) -> usize {
        let resolved = addr as usize;
        match self {
            ChrBankMode::Whole(bank) => resolved + (bank * kb(8)),
            ChrBankMode::Separate(bank0, bank1) => {
                match addr {
                    0x0 ... 0x0FFF => resolved + (bank0 * kb(8)),
                    0x1000 ... 0x1FFF => resolved + (bank1 * kb(8)),
                    _ => unreachable!()
                }
            }
        }
    }
}

pub struct Mmc1 {
    prg_bank_mode: PrgBankMode,
    chr_bank_mode: ChrBankMode,
    shift_register: u8,
    prg_ram: Option<Mem>,
    prg_rom: Mem,
    chr_rom: Mem,
    internal_vram: Mem,
    nametable_mirror: NametableMirror
}

impl Mmc1 {
    pub fn new(header: &[u8], rom_sections: &[u8]) -> Shared<Mmc1> {
        let attrs = HeaderAttributes::from_headers(header);
        let prg_rom = mem(&rom_sections[0 .. attrs.prg_rom_size * kb(16)]);

        let chr_rom = if attrs.chr_rom_size > 0 {
            mem(&rom_sections[attrs.prg_rom_size * kb(16) .. (attrs.prg_rom_size * kb(16)) + attrs.chr_rom_size * kb(8)])
        } else {
            // CHR RAM (assumes INES format!)
            initialized_mem(0x2000)
        };

        let prg_ram = match attrs.prg_ram {
            true => Some(initialized_mem(kb(8))),
            false => None
        };

        shared(Mmc1 {
            prg_bank_mode: PrgBankMode::LastFixed(0, attrs.prg_rom_size - 1),
            chr_bank_mode: ChrBankMode::Whole(0),
            shift_register: 0,
            prg_rom,
            chr_rom,
            prg_ram,
            internal_vram: initialized_mem(kb(4)),
            nametable_mirror: attrs.nametable_mirror
        })
    }

    fn mirrored_addr(&self, addr: u16) -> usize {
        self.nametable_mirror.resolve_addr(addr) - kb(8)
    }

    fn control_register(&mut self, value: u8) {
        self.nametable_mirror = match value & 0b0000_0011 {
            0 => NametableMirror::Single(0),
            1 => NametableMirror::Single(0x800),
            2 => NametableMirror::Vertical,
            3 => NametableMirror::Horizontal,
            _ => unreachable!()
        };
        self.prg_bank_mode = match (value >> 2) & 0b0000_0011 {
            0 | 1 => PrgBankMode::Whole(0),
            2 => PrgBankMode::FirstFixed(0),
            3 => PrgBankMode::LastFixed(0, (self.prg_rom.len() / kb(16)) - 1),
            _ => unreachable!()
        };
        self.chr_bank_mode = match (value >> 4) & 0b0000_0001 {
            0 => ChrBankMode::Whole(0),
            1 => ChrBankMode::Separate(0, 0),
            _ => unreachable!()
        };
        debug!("MMC1 control: PRG {:?}, CHR {:?}, Nametable {:?}",
               self.prg_bank_mode, self.chr_bank_mode, self.nametable_mirror);
    }

    fn chr_bank_0(&mut self, value: usize) {
        self.chr_bank_mode = match self.chr_bank_mode {
            ChrBankMode::Whole(_) => ChrBankMode::Whole(value & 0b1111_1110),
            ChrBankMode::Separate(_, bank1) => ChrBankMode::Separate(value, bank1)
        };
        debug!("Set MMC1 CHR mode: {:?}", self.chr_bank_mode);
    }

    fn chr_bank_1(&mut self, value: usize) {
        match self.chr_bank_mode {
            ChrBankMode::Whole(_) => {},
            ChrBankMode::Separate(bank0, _) => self.chr_bank_mode = ChrBankMode::Separate(bank0, value)
        };
        debug!("Set MMC1 CHR mode: {:?}", self.chr_bank_mode);
    }

    fn prg_bank(&mut self, value: usize) {
        self.prg_bank_mode = match self.prg_bank_mode {
            PrgBankMode::Whole(_) => PrgBankMode::Whole(value & 0b1111_1110),
            PrgBankMode::FirstFixed(_) => PrgBankMode::FirstFixed(value),
            PrgBankMode::LastFixed(_, count) => PrgBankMode::LastFixed(value, count)
        };
        debug!("Set MMC1 PRG mode: {:?}", self.prg_bank_mode);
    }

    fn modeswitch(&mut self, addr: u16, value: u8) {
        let val = value as usize;
        match addr {
            0x8000 ... 0x9FFF => self.control_register(value),
            0xA000 ... 0xBFFF => self.chr_bank_0(val),
            0xC000 ... 0xDFFF => self.chr_bank_1(val),
            0xE000 ... 0xFFFF => self.prg_bank(val),
            _ => unimplemented!("MMC1 modeswitch addr {:04X} -> {:#010b}", addr, value),
        }
    }

    fn write_shift_register(&mut self, addr: u16, value: u8) {
        if (value & 0b1000_0000) != 0 {
            self.shift_register = SHIFT_REGISTER_INITIAL;
        } else {
            let modeswitching = (self.shift_register & 0b0000_0001) != 0;
            self.shift_register >>= 1;
            self.shift_register |= (value << 4) & 0b0001_0000;
            if modeswitching {
                self.modeswitch(addr, self.shift_register);
                self.shift_register = SHIFT_REGISTER_INITIAL;
            }
        }
    }
}

impl Mapping for Mmc1 {
    fn get_cpu_space(&self, addr: u16) -> u8 {
        match addr {
            0x0000...0x401F => panic!("Address {:X?} not handled by mappers!", addr),
            0x4020...0x5FFF => panic!("Address {:X?} unused by this mapper!", addr),
            0x6000...0x7FFF => self.prg_ram.as_ref().expect("ROM without RAM tried to write it!")[(addr - 0x6000) as usize],
            0x8000...0xFFFF => self.prg_rom[self.prg_bank_mode.resolve_addr(addr)]
        }
    }

    fn set_cpu_space(&mut self, addr: u16, value: u8) {
        match addr {
            0x0000...0x401F => panic!("Address {:X?} not handled by mappers!", addr),
            0x4020...0x5FFF => panic!("Address {:X?} unused by this mapper!", addr),
            0x6000...0x7FFF => self.prg_ram.as_mut().expect("ROM without RAM tried to write it!")[(addr - 0x6000) as usize] = value,
            0x8000...0xFFFF => self.write_shift_register(addr, value)
        }
    }

    fn get_cpu_page(&self, _start_addr: u16) -> &[u8] {
        unimplemented!()
    }

    fn get_ppu_space(&self, addr: u16) -> u8 {
        match addr {
            0x0 ... 0x1FFF => self.chr_rom[self.chr_bank_mode.resolve_addr(addr)],
            0x2000 ... 0x2FFF => self.internal_vram[self.mirrored_addr(addr)],
            0x3000 ... 0x3EFF => self.internal_vram[(addr - 0x3000) as usize],
            _ => unimplemented!()
        }
    }

    fn set_ppu_space(&mut self, addr: u16, value: u8) {
        match addr {
            0x0 ... 0x1FFF => self.chr_rom[self.chr_bank_mode.resolve_addr(addr)] = value,
            0x2000 ... 0x2FFF => {
                let addr = self.mirrored_addr(addr);
                self.internal_vram[addr] = value;
            },
            0x3000 ... 0x3EFF => self.internal_vram[(addr - 0x3000) as usize] = value,
            _ => unimplemented!()
        }
    }
}

pub struct Uxrom {
    prg_bank_mode: PrgBankMode,  // always LastFixed!
    prg_ram: Option<Mem>,
    prg_rom: Mem,
    chr_rom: Mem,
    internal_vram: Mem,
    nametable_mirror: NametableMirror
}

impl Uxrom {
    pub fn new(header: &[u8], rom_sections: &[u8]) -> Shared<Uxrom> {
        let attrs = HeaderAttributes::from_headers(header);
        let prg_rom = mem(&rom_sections[0..attrs.prg_rom_size * kb(16)]);

        let chr_rom = if attrs.chr_rom_size > 0 {
            mem(&rom_sections[attrs.prg_rom_size * kb(16)..(attrs.prg_rom_size * kb(16)) + attrs.chr_rom_size * kb(8)])
        } else {
            // CHR RAM (assumes INES format!)
            initialized_mem(0x2000)
        };

        let prg_ram = match attrs.prg_ram {
            true => Some(initialized_mem(kb(8))),
            false => None
        };

        shared(Uxrom {
            prg_bank_mode: PrgBankMode::LastFixed(0, attrs.prg_rom_size - 1),
            prg_rom,
            chr_rom,
            prg_ram,
            internal_vram: initialized_mem(kb(4)),
            nametable_mirror: attrs.nametable_mirror
        })
    }

    fn set_bank_mode(&mut self, value: u8) {
        if let PrgBankMode::LastFixed(_, count) = self.prg_bank_mode {
            self.prg_bank_mode = PrgBankMode::LastFixed(value as usize, count);
        }
    }

    fn mirrored_addr(&self, addr: u16) -> usize {
        self.nametable_mirror.resolve_addr(addr) - kb(8)
    }
}

impl Mapping for Uxrom {
    fn get_cpu_space(&self, addr: u16) -> u8 {
        match addr {
            0x0000...0x401F => panic!("Address {:X?} not handled by mappers!", addr),
            0x4020...0x5FFF => panic!("Address {:X?} unused by this mapper!", addr),
            0x6000...0x7FFF => self.prg_ram.as_ref().expect("ROM without RAM tried to write it!")[(addr - 0x6000) as usize],
            0x8000...0xFFFF => self.prg_rom[self.prg_bank_mode.resolve_addr(addr)]
        }
    }

    fn set_cpu_space(&mut self, addr: u16, value: u8) {
        match addr {
            0x0000...0x401F => panic!("Address {:X?} not handled by mappers!", addr),
            0x4020...0x5FFF => panic!("Address {:X?} unused by this mapper!", addr),
            0x6000...0x7FFF => self.prg_ram.as_mut().expect("ROM without RAM tried to write it!")[(addr - 0x6000) as usize] = value,
            0x8000...0xFFFF => self.set_bank_mode(value)
        }
    }

    fn get_cpu_page(&self, _start_addr: u16) -> &[u8] {
        unimplemented!()
    }

    fn get_ppu_space(&self, addr: u16) -> u8 {
        match addr {
            0x0 ... 0x1FFF => self.chr_rom[addr as usize],
            0x2000 ... 0x2FFF => self.internal_vram[self.mirrored_addr(addr)],
            0x3000 ... 0x3EFF => self.internal_vram[(addr - 0x3000) as usize],
            _ => unimplemented!()
        }
    }

    fn set_ppu_space(&mut self, addr: u16, value: u8) {
        match addr {
            0x0 ... 0x1FFF => self.chr_rom[addr as usize] = value,
            0x2000 ... 0x2FFF => {
                let addr = self.mirrored_addr(addr);
                self.internal_vram[addr] = value;
            },
            0x3000 ... 0x3EFF => self.internal_vram[(addr - 0x3000) as usize] = value,
            _ => unimplemented!()
        }
    }
}
