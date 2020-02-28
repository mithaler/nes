// Mapper 001: https://wiki.nesdev.com/w/index.php/MMC1
// Mapper 002: https://wiki.nesdev.com/w/index.php/UxROM

use crate::common::{Shared, shared, OPEN_BUS_VALUE};
use crate::memory::{Mem, mem, initialized_mem};
use crate::mappers::{NametableMirror, HeaderAttributes, Mapping, Resolver, kb};

const SHIFT_REGISTER_INITIAL: u8 = 0b0001_0000;

#[derive(Debug)]
enum PrgBankMode {
    Whole,
    FirstFixed,
    LastFixed(usize)  // total count of banks
}

impl PrgBankMode {
    fn resolve_addr(&self, bank: usize, addr: u16) -> usize {
        let resolved = addr as usize - kb(32);
        match self {
            PrgBankMode::Whole => resolved + (kb(32) * (bank & 0b1111_1110)),
            PrgBankMode::FirstFixed => {
                if resolved < kb(16) {
                    resolved
                } else {
                    resolved + bank * kb(16)
                }
            },
            PrgBankMode::LastFixed(count) => {
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
    Whole,
    Separate
}

impl ChrBankMode {
    fn resolve_addr(&self, bank0: usize, bank1: usize, addr: u16) -> usize {
        let resolved = addr as usize;
        match self {
            ChrBankMode::Whole => resolved + ((bank0 & 0b1111_1110) * kb(8)),
            ChrBankMode::Separate => {
                match addr {
                    0x0 ..= 0x0FFF => resolved + (bank0 * kb(4)),
                    0x1000 ..= 0x1FFF => resolved - 0x1000 + (bank1 * kb(4)),
                    _ => unreachable!()
                }
            }
        }
    }
}

pub struct Mmc1 {
    selected_prg_bank: usize,
    prg_bank_mode: PrgBankMode,
    selected_chr_bank_0: usize,
    selected_chr_bank_1: usize,
    chr_bank_mode: ChrBankMode,
    shift_register: u8,
    prg_ram: Mem,
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

        let prg_ram = initialized_mem(kb(8));

        shared(Mmc1 {
            selected_prg_bank: 0,
            prg_bank_mode: PrgBankMode::LastFixed(attrs.prg_rom_size - 1),
            selected_chr_bank_0: 0,
            selected_chr_bank_1: 0,
            chr_bank_mode: ChrBankMode::Whole,
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
            0 | 1 => PrgBankMode::Whole,
            2 => PrgBankMode::FirstFixed,
            3 => PrgBankMode::LastFixed((self.prg_rom.len() / kb(16)) - 1),
            _ => unreachable!()
        };
        self.chr_bank_mode = match (value >> 4) & 0b0000_0001 {
            0 => ChrBankMode::Whole,
            1 => ChrBankMode::Separate,
            _ => unreachable!()
        };
        debug!("MMC1 control: PRG {:?}, CHR {:?}, Nametable {:?}",
               self.prg_bank_mode, self.chr_bank_mode, self.nametable_mirror);
    }

    fn chr_bank_0(&mut self, value: usize) {
        self.selected_chr_bank_0 = value & 0b0001_1111;
        debug!("Set MMC1 CHR bank 0: {:?} {:?}", self.chr_bank_mode, self.selected_chr_bank_0);
    }

    fn chr_bank_1(&mut self, value: usize) {
        self.selected_chr_bank_1 = value & 0b0001_1111;
        debug!("Set MMC1 CHR bank 1: {:?}", self.selected_chr_bank_1);
    }

    fn prg_bank(&mut self, value: usize) {
        self.selected_prg_bank = value;
        self.prg_bank_mode = match self.prg_bank_mode {
            PrgBankMode::Whole => PrgBankMode::Whole,
            PrgBankMode::FirstFixed => PrgBankMode::FirstFixed,
            PrgBankMode::LastFixed(count) => PrgBankMode::LastFixed(count)
        };
        debug!("Set MMC1 PRG mode: {:?}", self.prg_bank_mode);
    }

    fn modeswitch(&mut self, addr: u16, value: u8) {
        let val = value as usize;
        match addr {
            0x8000 ..= 0x9FFF => self.control_register(value),
            0xA000 ..= 0xBFFF => self.chr_bank_0(val),
            0xC000 ..= 0xDFFF => self.chr_bank_1(val),
            0xE000 ..= 0xFFFF => self.prg_bank(val),
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
            0x0000..=0x401F => panic!("Address {:X?} not handled by mappers!", addr),
            0x4020..=0x5FFF => OPEN_BUS_VALUE,
            0x6000..=0x7FFF => self.prg_ram[(addr - 0x6000) as usize],
            0x8000..=0xFFFF => self.prg_rom[self.prg_bank_mode.resolve_addr(self.selected_prg_bank, addr)]
        }
    }

    fn set_cpu_space(&mut self, addr: u16, value: u8) {
        match addr {
            0x0000..=0x401F => panic!("Address {:X?} not handled by mappers!", addr),
            0x4020..=0x5FFF => {},
            0x6000..=0x7FFF => self.prg_ram[(addr - 0x6000) as usize] = value,
            0x8000..=0xFFFF => self.write_shift_register(addr, value)
        }
    }

    fn get_ppu_space(&self, addr: u16) -> u8 {
        match addr {
            0x0 ..= 0x1FFF => self.chr_rom[self.chr_bank_mode.resolve_addr(self.selected_chr_bank_0, self.selected_chr_bank_1, addr)],
            0x2000 ..= 0x2FFF => self.internal_vram[self.mirrored_addr(addr)],
            0x3000 ..= 0x3EFF => self.internal_vram[(addr - 0x3000) as usize],
            _ => unimplemented!()
        }
    }

    fn set_ppu_space(&mut self, addr: u16, value: u8) {
        match addr {
            0x0 ..= 0x1FFF => self.chr_rom[self.chr_bank_mode.resolve_addr(self.selected_chr_bank_0, self.selected_chr_bank_1, addr)] = value,
            0x2000 ..= 0x2FFF => {
                let addr = self.mirrored_addr(addr);
                self.internal_vram[addr] = value;
            },
            0x3000 ..= 0x3EFF => self.internal_vram[(addr - 0x3000) as usize] = value,
            _ => unimplemented!()
        }
    }
}

pub struct Uxrom {
    selected_prg_bank: usize,
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
            initialized_mem(kb(8))
        };

        let prg_ram = match attrs.prg_ram {
            true => Some(initialized_mem(kb(8))),
            false => None
        };

        shared(Uxrom {
            selected_prg_bank: 0,
            prg_bank_mode: PrgBankMode::LastFixed(attrs.prg_rom_size - 1),
            prg_rom,
            chr_rom,
            prg_ram,
            internal_vram: initialized_mem(kb(4)),
            nametable_mirror: attrs.nametable_mirror
        })
    }

    fn set_bank_mode(&mut self, value: u8) {
        self.selected_prg_bank = value as usize;
    }

    fn mirrored_addr(&self, addr: u16) -> usize {
        self.nametable_mirror.resolve_addr(addr) - kb(8)
    }
}

impl Mapping for Uxrom {
    fn get_cpu_space(&self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x401F => panic!("Address {:X?} not handled by mappers!", addr),
            0x4020..=0x5FFF => OPEN_BUS_VALUE,
            0x6000..=0x7FFF => self.prg_ram.as_ref().map_or(0, |ram| ram[(addr - 0x6000) as usize]),
            0x8000..=0xFFFF => self.prg_rom[self.prg_bank_mode.resolve_addr(self.selected_prg_bank, addr)]
        }
    }

    fn set_cpu_space(&mut self, addr: u16, value: u8) {
        match addr {
            0x0000..=0x401F => panic!("Address {:X?} not handled by mappers!", addr),
            0x4020..=0x5FFF => {},
            0x6000..=0x7FFF => self.prg_ram.as_mut().expect("ROM without RAM tried to write it!")[(addr - 0x6000) as usize] = value,
            0x8000..=0xFFFF => self.set_bank_mode(value)
        }
    }

    fn get_ppu_space(&self, addr: u16) -> u8 {
        match addr {
            0x0 ..= 0x1FFF => self.chr_rom[addr as usize],
            0x2000 ..= 0x2FFF => self.internal_vram[self.mirrored_addr(addr)],
            0x3000 ..= 0x3EFF => self.internal_vram[(addr - 0x3000) as usize],
            _ => unimplemented!()
        }
    }

    fn set_ppu_space(&mut self, addr: u16, value: u8) {
        match addr {
            0x0 ..= 0x1FFF => self.chr_rom[addr as usize] = value,
            0x2000 ..= 0x2FFF => {
                let addr = self.mirrored_addr(addr);
                self.internal_vram[addr] = value;
            },
            0x3000 ..= 0x3EFF => self.internal_vram[(addr - 0x3000) as usize] = value,
            _ => unimplemented!()
        }
    }
}
