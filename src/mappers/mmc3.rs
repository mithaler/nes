// https://wiki.nesdev.com/w/index.php/MMC3
use crate::mappers::{Mapping, kb, NametableMirror, Resolver, HeaderAttributes};
use crate::memory::{Mem, mem, initialized_mem};
use crate::common::{Shared, shared, Clocked, OPEN_BUS_VALUE};

// I wish this were in the stdlib
fn div_rem(a: usize, b: usize) -> (usize, usize) {
    (a / b, a % b)
}

#[derive(Default, Debug)]
struct IrqCounter {
    enabled: bool,
    reload: bool,
    latch: u8,
    counter: u8,
    triggered: bool
}

impl Clocked for IrqCounter {
    fn tick(&mut self) {
        if self.reload {
            self.counter = self.latch;
            self.reload = false;
        } else if self.counter == 0 {
            self.counter = self.latch;
            if self.enabled {
                self.triggered = true;
            }
        } else {
            self.counter -= 1;
        }
    }
}

// See the wiki page for an explanation of the many registers
#[derive(Debug)]
pub struct Mmc3 {
    prg_rom: Mem,
    prg_ram: Mem,
    prg_bank_count: usize,
    prg_r6: usize, // swappable bank
    prg_r7: usize, // middle bank
    prg_first_bank_switchable: bool,

    chr_rom: Mem,
    chr_first_bank_fine: bool,
    chr_course_bank_registers: [usize; 2],
    chr_fine_bank_registers: [usize; 4],
    internal_vram: Mem,

    bank_selector: usize,

    ram_write_protected: bool,
    ram_enabled: bool,

    nametable_mirror: NametableMirror,
    irq: IrqCounter,
}

impl Mmc3 {
    pub fn new(header: &[u8], rom_sections: &[u8]) -> Shared<Mmc3> {
        let attrs = HeaderAttributes::from_headers(header);
        let prg_rom = mem(&rom_sections[0 .. attrs.prg_rom_size * kb(16)]);

        let chr_rom = if attrs.chr_rom_size > 0 {
            mem(&rom_sections[attrs.prg_rom_size * kb(16) .. (attrs.prg_rom_size * kb(16)) + attrs.chr_rom_size * kb(8)])
        } else {
            // CHR RAM (assumes INES format!)
            initialized_mem(0x2000)
        };

        let prg_ram = initialized_mem(kb(8));
        let prg_bank_count = prg_rom.len() / kb(8);

        shared(Mmc3 {
            prg_rom,
            prg_ram,
            chr_rom,

            prg_bank_count,
            prg_r6: 0,
            prg_r7: 0,
            prg_first_bank_switchable: false,

            chr_first_bank_fine: false,
            chr_course_bank_registers: [0, 0],
            chr_fine_bank_registers: [0, 0, 0, 0],
            internal_vram: initialized_mem(kb(4)),

            bank_selector: 0,

            ram_write_protected: false,
            ram_enabled: false,

            nametable_mirror: NametableMirror::Horizontal,
            irq: Default::default()
        })
    }

    fn bank_select(&mut self, value: u8) {
        self.bank_selector = (value & 0b0000_0111) as usize;
        self.prg_first_bank_switchable = (value & 0b0100_0000) == 0;
        self.chr_first_bank_fine = (value & 0b1000_0000) != 0;
    }

    fn set_bank_register(&mut self, value: u8) {
        match self.bank_selector {
            r @ 0 ... 1 => self.chr_course_bank_registers[r as usize] = (value & 0b1111_1110) as usize,
            r @ 2 ... 5 => self.chr_fine_bank_registers[r as usize - 2] = value as usize,
            6 => self.prg_r6 = (value & 0b0011_1111) as usize,
            7 => self.prg_r7 = (value & 0b0011_1111) as usize,
            _ => unreachable!()
        }
        debug!("MMC3 bank register change: R{:?} -> {:?}", self.bank_selector, value);
    }

    fn set_nametable_mirror(&mut self, value: u8) {
        self.nametable_mirror = match value & 1 {
            0 => NametableMirror::Vertical,
            1 => NametableMirror::Horizontal,
            _ => unreachable!()
        }
    }

    fn set_ram_protect(&mut self, value: u8) {
        self.ram_write_protected = (value & 0b0100_0000) != 0;
        self.ram_enabled = (value & 0b1000_0000) != 0;
    }

    fn resolve_swappable_prg_bank(&self, addr: usize, base: usize) -> usize {
        let resolved = addr - base;
        if (base == 0x8000 && self.prg_first_bank_switchable) ||
           (base == 0xC000 && !self.prg_first_bank_switchable) {
            resolved + (self.prg_r6 * kb(8))
        } else {
            resolved + (self.prg_bank_count - 2) * kb(8)
        }
    }

    fn read_chr_rom(&self, addr: usize) -> u8 {
        // https://wiki.nesdev.com/w/index.php/MMC3#CHR_Banks
        let resolved_addr = if self.chr_first_bank_fine {
            match addr {
                0x0000...0x0FFF => {
                    let (bank, position) = div_rem(addr, kb(1));
                    self.chr_fine_bank_registers[bank] * kb(1) + position
                }
                0x1000...0x1FFF => {
                    let (bank, position) = div_rem(addr - 0x1000, kb(2));
                    self.chr_course_bank_registers[bank] * kb(1) + position
                },
                _ => unreachable!()
            }
        } else {
            match addr {
                0x0000...0x0FFF => {
                    let (bank, position) = div_rem(addr, kb(2));
                    self.chr_course_bank_registers[bank] * kb(1) + position
                },
                0x1000...0x1FFF => {
                    let (bank, position) = div_rem(addr - 0x1000, kb(1));
                    self.chr_fine_bank_registers[bank] * kb(1) + position
                },
                _ => unreachable!()
            }
        };
        self.chr_rom[resolved_addr]
    }

    fn mirrored_addr(&self, addr: u16) -> usize {
        self.nametable_mirror.resolve_addr(addr) - kb(8)
    }
}

impl Mapping for Mmc3 {
    fn get_cpu_space(&self, addr: u16) -> u8 {
        let resolved = addr as usize;
        match addr {
            0x0000...0x401F => panic!("Address {:X?} not handled by mappers!", addr),
            0x4020...0x5FFF => panic!("Address {:X?} unused by this mapper!", addr),
            0x6000...0x7FFF => {
                match self.ram_enabled {
                    true => self.prg_ram[resolved - 0x6000],
                    false => OPEN_BUS_VALUE
                }
            },
            0x8000...0x9FFF => self.prg_rom[self.resolve_swappable_prg_bank(resolved, 0x8000)],
            0xA000...0xBFFF => self.prg_rom[resolved - 0xA000 + (self.prg_r7 * kb(8))],
            0xC000...0xDFFF => self.prg_rom[self.resolve_swappable_prg_bank(resolved, 0xC000)],
            0xE000...0xFFFF => self.prg_rom[resolved - 0xE000 + ((self.prg_bank_count - 1) * kb(8))]
        }
    }

    fn set_cpu_space(&mut self, addr: u16, value: u8) {
        let resolved = addr as usize;
        match addr {
            0x6000...0x7FFF =>
                if self.ram_enabled && !self.ram_write_protected {
                    self.prg_ram[resolved - 0x6000] = value
                },
            0x8000...0x9FFE if addr & 1 == 0 => self.bank_select(value),
            0x8001...0x9FFF if addr & 1 == 1 => self.set_bank_register(value),
            0xA000...0xBFFE if addr & 1 == 0 => self.set_nametable_mirror(value),
            0xA001...0xBFFF if addr & 1 == 1 => self.set_ram_protect(value),
            0xC000...0xDFFE if addr & 1 == 0 => self.irq.latch = value,
            0xC001...0xDFFF if addr & 1 == 1 => self.irq.reload = true,
            0xE000...0xFFFE if addr & 1 == 0 => {
                self.irq.enabled = false;
                self.irq.counter = self.irq.latch;
                self.irq.triggered = false;
            },
            0xE001...0xFFFF if addr & 1 == 1 => self.irq.enabled = true,
            _ => unimplemented!("MMC3 write: {:04X?} -> {:02X?}", addr, value)
        }
    }

    fn get_ppu_space(&self, addr: u16) -> u8 {
        match addr {
            0x0000...0x1FFF => self.read_chr_rom(addr as usize),
            0x2000...0x2FFF => self.internal_vram[self.mirrored_addr(addr)],
            0x3000...0x3EFF => self.internal_vram[(addr - 0x3000) as usize],
            _ => unimplemented!()
        }
    }

    fn set_ppu_space(&mut self, addr: u16, value: u8) {
        match addr {
            0x2000...0x2FFF => {
                let addr = self.mirrored_addr(addr);
                self.internal_vram[addr] = value;
            },
            0x3000...0x3EFF => self.internal_vram[(addr - 0x3000) as usize] = value,
            _ => unimplemented!("Bad MMC3 write: {:04X?} -> {:02X?}", addr, value)
        }
    }

    fn clock_scanline(&mut self) {
        self.irq.tick();
    }

    fn irq(&mut self) -> bool {
        if self.irq.triggered {
            self.irq.triggered = false;
            true
        } else {
            false
        }
    }
}
