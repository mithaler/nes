use crate::common::{Clocked, Addressable, join_bytes};
use crate::memory::{CpuMem, Mem};

mod opcodes {
    #[derive(Debug)]
    pub enum Operation {
        ADC, AND, ASL, BCC, BCS, BEQ, BIT, BMI,
        BNE, BPL, BRK, BVC, BVS, CLC, CLD, CLI,
        CLV, CMP, CPX, CPY, DEC, DEX, DEY, EOR,
        INC, INX, INY, JMP, JSR, LDA, LDX, LDY,
        LSR, NOP, ORA, PHA, PHP, PLA, PLP, ROL,
        ROR, RTI, RTS, SBC, SEC, SED, SEI, STA,
        STX, STY, TAX, TAY, TSX, TXA, TXS, TYA,

        // Unofficial
        KIL, ISC, DCP, AXS, LAS, LAX, AHX, SAX,
        XAA, SHX, RRA, TAS, SHY, ARR, SRE, ALR,
        RLA, ANC, SLO,
    }

    #[derive(Debug, PartialEq)]
    pub enum AddressMode {
        Implicit,
        Accumulator,
        Immediate,
        ZeroPage,
        ZeroPageX,
        ZeroPageY,
        Relative,
        Absolute,
        AbsoluteX,
        AbsoluteY,
        Indirect,
        IndirectX,
        IndirectY
    }

    impl AddressMode {
        pub fn byte_count(&self) -> u16 {
            match self {
                // no arg
                Accumulator | Implicit => 1,

                // 1 byte arg
                ZeroPage | ZeroPageX | ZeroPageY | Relative => 2,
                Immediate | Indirect | IndirectX | IndirectY => 2,

                // 2 byte arg
                Absolute | AbsoluteX | AbsoluteY => 3,
            }
        }
    }

    use Operation::*;
    use AddressMode::*;

    type Cycles = u8;
    type AddIfPageBoundaryCrossed = bool;

    pub type Opcode = (Operation, AddressMode, Cycles, AddIfPageBoundaryCrossed);

    // http://www.oxyron.de/html/opcodes02.html
    const TABLE: [Opcode; 256] = [
        // 0x
        (BRK, Implicit, 7, false),
        (ORA, IndirectX, 6, false),
        (KIL, Implicit, 0, false),
        (SLO, IndirectX, 8, false),
        (NOP, ZeroPage, 3, false),
        (ORA, ZeroPage, 3, false),
        (ASL, ZeroPage, 5, false),
        (SLO, ZeroPage, 5, false),
        (PHP, Implicit, 3, false),
        (ORA, Immediate, 2, false),
        (ASL, Accumulator, 2, false),
        (ANC, Immediate, 2, false),
        (NOP, Absolute, 4, false),
        (ORA, Absolute, 4, false),
        (ASL, Absolute, 6, false),
        (SLO, Absolute, 6, false),

        // 1x
        (BPL, Relative, 2, true),
        (ORA, IndirectY, 5, true),
        (KIL, Implicit, 0, false),
        (SLO, IndirectY, 8, false),
        (NOP, ZeroPageX, 4, false),
        (ORA, ZeroPageX, 4, false),
        (ASL, ZeroPageX, 6, false),
        (SLO, ZeroPageX, 6, false),
        (CLC, Implicit, 2, false),
        (ORA, AbsoluteY, 4, true),
        (NOP, Implicit, 2, false),
        (SLO, AbsoluteY, 7, false),
        (NOP, AbsoluteX, 4, true),
        (ORA, AbsoluteX, 4, true),
        (ASL, AbsoluteX, 7, false),
        (SLO, AbsoluteX, 7, false),

        // 2x
        (JSR, Absolute, 6, false),
        (AND, IndirectX, 6, false),
        (KIL, Implicit, 0, false),
        (RLA, IndirectX, 8, false),
        (BIT, ZeroPage, 3, false),
        (AND, ZeroPage, 3, false),
        (ROL, ZeroPage, 5, false),
        (RLA, ZeroPage, 5, false),
        (PLP, Implicit, 4, false),
        (AND, Immediate, 2, false),
        (ROL, Accumulator, 2, false),
        (ANC, Immediate, 2, false),
        (BIT, Absolute, 4, false),
        (AND, Absolute, 4, false),
        (ROL, Absolute, 6, false),
        (RLA, Absolute, 6, false),

        // 3x
        (BMI, Relative, 2, true),
        (AND, IndirectY, 5, true),
        (KIL, Implicit, 0, false),
        (RLA, IndirectY, 8, false),
        (NOP, ZeroPageX, 4, false),
        (AND, ZeroPageX, 4, false),
        (ROL, ZeroPageX, 6, false),
        (RLA, ZeroPageX, 6, false),
        (SEC, Implicit, 2, false),
        (AND, AbsoluteY, 4, true),
        (NOP, Implicit, 2, false),
        (RLA, AbsoluteY, 7, false),
        (NOP, AbsoluteX, 4, true),
        (AND, AbsoluteX, 4, true),
        (ROL, AbsoluteX, 7, false),
        (RLA, AbsoluteX, 7, false),

        // 4x
        (RTI, Implicit, 6, false),
        (EOR, IndirectX, 6, false),
        (KIL, Implicit, 0, false),
        (SRE, IndirectX, 8, false),
        (NOP, ZeroPage, 3, false),
        (EOR, ZeroPage, 3, false),
        (LSR, ZeroPage, 5, false),
        (SRE, ZeroPage, 5, false),
        (PHA, Implicit, 3, false),
        (EOR, Immediate, 2, false),
        (LSR, Accumulator, 2, false),
        (ALR, Immediate, 2, false),
        (JMP, Absolute, 3, false),
        (EOR, Absolute, 4, false),
        (LSR, Absolute, 6, false),
        (SRE, Absolute, 6, false),

        // 5x
        (BVC, Relative, 2, true),
        (EOR, IndirectY, 5, true),
        (KIL, Implicit, 0, false),
        (SRE, IndirectY, 8, false),
        (NOP, ZeroPageX, 4, false),
        (EOR, ZeroPageX, 4, false),
        (LSR, ZeroPageX, 6, false),
        (SRE, ZeroPageX, 6, false),
        (CLI, Implicit, 2, false),
        (EOR, AbsoluteY, 4, true),
        (NOP, Implicit, 2, false),
        (SRE, AbsoluteY, 7, false),
        (NOP, AbsoluteX, 4, true),
        (EOR, AbsoluteX, 4, true),
        (LSR, AbsoluteX, 7, false),
        (SRE, AbsoluteX, 7, false),

        // 6x
        (RTS, Implicit, 6, false),
        (ADC, IndirectX, 6, false),
        (KIL, Implicit, 0, false),
        (RRA, IndirectX, 8, false),
        (NOP, ZeroPage, 3, false),
        (ADC, ZeroPage, 3, false),
        (ROR, ZeroPage, 5, false),
        (RRA, ZeroPage, 5, false),
        (PLA, Implicit, 4, false),
        (ADC, Immediate, 2, false),
        (ROR, Accumulator, 2, false),
        (ARR, Immediate, 2, false),
        (JMP, Indirect, 5, false),
        (ADC, Absolute, 4, false),
        (ROR, Absolute, 6, false),
        (RRA, Absolute, 6, false),

        // 7x
        (BVS, Relative, 2, true),
        (ADC, IndirectY, 5, true),
        (KIL, Implicit, 0, false),
        (RRA, IndirectY, 8, false),
        (NOP, ZeroPageX, 4, false),
        (ADC, ZeroPageX, 4, false),
        (ROR, ZeroPageX, 6, false),
        (RRA, ZeroPageX, 6, false),
        (SEI, Implicit, 2, false),
        (ADC, AbsoluteY, 4, true),
        (NOP, Implicit, 2, false),
        (RRA, AbsoluteY, 7, false),
        (NOP, AbsoluteX, 4, true),
        (ADC, AbsoluteX, 4, true),
        (ROR, AbsoluteX, 7, false),
        (RRA, AbsoluteX, 7, false),

        // 8x
        (NOP, Immediate, 2, false),
        (STA, IndirectX, 6, false),
        (NOP, Immediate, 2, false),
        (SAX, IndirectX, 6, false),
        (STY, ZeroPage, 3, false),
        (STA, ZeroPage, 3, false),
        (STX, ZeroPage, 3, false),
        (SAX, ZeroPage, 3, false),
        (DEY, Implicit, 2, false),
        (NOP, Immediate, 2, false),
        (TXA, Implicit, 2, false),
        (XAA, Immediate, 2, false),
        (STY, Absolute, 4, false),
        (STA, Absolute, 4, false),
        (STX, Absolute, 4, false),
        (SAX, Absolute, 4, false),

        // 9x
        (BCC, Relative, 2, true),
        (STA, IndirectY, 6, false),
        (KIL, Implicit, 0, false),
        (AHX, IndirectY, 6, false),
        (STY, ZeroPageX, 4, false),
        (STA, ZeroPageX, 4, false),
        (STX, ZeroPageY, 4, false),
        (SAX, ZeroPageY, 4, false),
        (TYA, Implicit, 2, false),
        (STA, AbsoluteY, 5, false),
        (TXS, Implicit, 2, false),
        (TAS, AbsoluteY, 5, false),
        (SHY, AbsoluteX, 5, false),
        (STA, AbsoluteX, 5, false),
        (SHX, AbsoluteY, 5, false),
        (AHX, AbsoluteY, 5, false),

        // Ax
        (LDY, Immediate, 2, false),
        (LDA, IndirectX, 6, false),
        (LDX, Immediate, 2, false),
        (LAX, IndirectX, 6, false),
        (LDY, ZeroPage, 3, false),
        (LDA, ZeroPage, 3, false),
        (LDX, ZeroPage, 3, false),
        (LAX, ZeroPage, 3, false),
        (TAY, Implicit, 2, false),
        (LDA, Immediate, 2, false),
        (TAX, Implicit, 2, false),
        (LAX, Immediate, 2, false),
        (LDY, Absolute, 4, false),
        (LDA, Absolute, 4, false),
        (LDX, Absolute, 4, false),
        (LAX, Absolute, 4, false),

        // Bx
        (BCS, Relative, 2, true),
        (LDA, IndirectY, 5, true),
        (KIL, Implicit, 0, false),
        (LAX, IndirectY, 5, true),
        (LDY, ZeroPageX, 4, false),
        (LDA, ZeroPageX, 4, false),
        (LDX, ZeroPageY, 4, false),
        (LAX, ZeroPageY, 4, false),
        (CLV, Implicit, 2, false),
        (LDA, AbsoluteY, 4, true),
        (TSX, Implicit, 2, false),
        (LAS, AbsoluteY, 4, true),
        (LDY, AbsoluteX, 4, true),
        (LDA, AbsoluteX, 4, true),
        (LDX, AbsoluteY, 4, true),
        (LAX, AbsoluteY, 4, true),

        // Cx
        (CPY, Immediate, 2, false),
        (CMP, IndirectX, 6, false),
        (NOP, Immediate, 2, false),
        (DCP, IndirectX, 8, false),
        (CPY, ZeroPage, 3, false),
        (CMP, ZeroPage, 3, false),
        (DEC, ZeroPage, 5, false),
        (DCP, ZeroPage, 5, false),
        (INY, Implicit, 2, false),
        (CMP, Immediate, 2, false),
        (DEX, Implicit, 2, false),
        (AXS, Immediate, 2, false),
        (CPY, Absolute, 4, false),
        (CMP, Absolute, 4, false),
        (DEC, Absolute, 6, false),
        (DCP, Absolute, 6, false),

        // Dx
        (BNE, Relative, 2, true),
        (CMP, IndirectY, 5, true),
        (KIL, Implicit, 0, false),
        (DCP, IndirectY, 8, false),
        (NOP, ZeroPageX, 4, false),
        (CMP, ZeroPageX, 4, false),
        (DEC, ZeroPageX, 6, false),
        (DCP, ZeroPageX, 6, false),
        (CLD, Implicit, 2, false),
        (CMP, AbsoluteY, 4, true),
        (NOP, Implicit, 2, false),
        (DCP, AbsoluteY, 7, false),
        (NOP, AbsoluteX, 4, true),
        (CMP, AbsoluteX, 4, true),
        (DEC, AbsoluteX, 7, false),
        (DCP, AbsoluteX, 7, false),

        // Ex
        (CPX, Immediate, 2, false),
        (SBC, IndirectX, 6, false),
        (NOP, Immediate, 2, false),
        (ISC, IndirectX, 8, false),
        (CPX, ZeroPage, 3, false),
        (SBC, ZeroPage, 3, false),
        (INC, ZeroPage, 5, false),
        (ISC, ZeroPage, 5, false),
        (INX, Implicit, 2, false),
        (SBC, Immediate, 2, false),
        (NOP, Implicit, 2, false),
        (SBC, Immediate, 2, false),
        (CPX, Absolute, 4, false),
        (SBC, Absolute, 4, false),
        (INC, Absolute, 6, false),
        (ISC, Absolute, 6, false),

        // Fx
        (BEQ, Relative, 2, true),
        (SBC, IndirectY, 5, true),
        (KIL, Implicit, 0, false),
        (ISC, IndirectY, 8, false),
        (NOP, ZeroPageX, 4, false),
        (SBC, ZeroPageX, 4, false),
        (INC, ZeroPageX, 6, false),
        (ISC, ZeroPageX, 6, false),
        (SED, Implicit, 2, false),
        (SBC, AbsoluteY, 4, true),
        (NOP, Implicit, 2, false),
        (ISC, AbsoluteY, 7, false),
        (NOP, AbsoluteX, 4, true),
        (SBC, AbsoluteX, 4, true),
        (INC, AbsoluteX, 7, false),
        (ISC, AbsoluteX, 7, false),
    ];

    pub fn resolve(code: u8) -> &'static Opcode {
        &TABLE[code as usize]
    }
}


pub struct Cpu {
    // address space
    mem: Box<CpuMem>,

    // registers
    a: u8, // accumulator
    x: u8, // index X
    y: u8, // index Y
    pc: u16, // program counter
    s: u8, // stack
    p: u8, // flags

    remaining_pause: u8,
    instruction_counter: u64,
}

use opcodes::Opcode;
use opcodes::Operation::*;
use opcodes::AddressMode::*;

const SIGN_BIT: u8 = 0b1000_0000;

const CARRY_FLAG: u8 = 0b0000_0001;
const ZERO_FLAG: u8 = 0b0000_0010;
const OVERFLOW_FLAG: u8 = 0b0100_0000;
const NEGATIVE_FLAG: u8 = SIGN_BIT;

// The mask of bits that get turned on when the P register is represented on the stack.
const PHP_MASK: u8 = 0b0011_0000;

impl Cpu {
    pub fn new(prg_rom: Mem) -> Cpu {
        // startup state: https://wiki.nesdev.com/w/index.php/CPU_power_up_state
        Cpu {
            mem: Box::new(CpuMem::new(prg_rom)),
            a: 0,
            x: 0,
            y: 0,
            pc: 0xC000,  // something something test program?
            s: 0xfd,
            p: 0x24,
            remaining_pause: 0,
            instruction_counter: 0,
        }
    }

    fn absolute_addr(&self) -> u16 {
        join_bytes(self.mem.get(self.pc + 2), self.mem.get(self.pc + 1))
    }

    fn next_byte(&self) -> u8 {
        self.mem.get(self.pc + 1)
    }

    /// Returns whether the addresses are on different 256-bit pages.
    fn different_pages(addr1: u16, addr2: u16) -> bool {
        (addr1 & 0xFF00) != (addr2 & 0xFF00)
    }

    /// Returns the address in memory that the opcode points to, based on the current
    /// position of `PC` and the opcode's address mode. Panics if the opcode's address mode is
    /// `ACCUMULATOR` or `IMPLICIT`, because in both cases the handler needs to do something
    /// other than resolve an address in memory.
    fn resolve_addr(&self, op: &Opcode) -> u16 {
        match op.1 {
            Accumulator => 0,
            Implicit => 0,
            Immediate => self.pc + 1,
            Absolute => self.absolute_addr(),
            AbsoluteX => self.absolute_addr().wrapping_add(self.x as u16),
            AbsoluteY => self.absolute_addr().wrapping_add(self.y as u16),
            ZeroPage => join_bytes(0x0, self.next_byte()),
            ZeroPageX => join_bytes(0x0, self.next_byte().wrapping_add(self.x)),
            ZeroPageY => join_bytes(0x0, self.next_byte().wrapping_add(self.y)),
            Relative => self.pc.wrapping_add((self.next_byte() as i8) as u16),
            Indirect => {
                let addr = join_bytes(self.mem.get(self.pc + 2), self.mem.get(self.pc + 1));
                let high_byte_addr = if (addr & 0x00FF) == 0x00FF {
                    // crazy 6502 bug!
                    addr & 0xFF00
                } else {
                    addr + 1
                };
                join_bytes(self.mem.get(high_byte_addr), self.mem.get(addr))
            }
            IndirectX => {
                let arg = self.next_byte().wrapping_add(self.x);
                let low = self.mem.get(join_bytes(0x0, arg));
                let high = self.mem.get(join_bytes(0x0, arg.wrapping_add(1)));
                join_bytes(high, low)
            },
            IndirectY => {
                let arg = self.next_byte();
                let low = self.mem.get(join_bytes(0x0, arg));
                let high = self.mem.get(join_bytes(0x0, arg.wrapping_add(1)));
                join_bytes(high, low).wrapping_add(self.y as u16)
            }
        }
    }

    fn stack_push(&mut self, datum: u8) {
        self.mem.set(join_bytes(0x01, self.s), datum);
        self.s -= 1;
    }

    fn stack_pop(&mut self) -> u8 {
        self.s += 1;
        self.mem.get(join_bytes(0x01, self.s))
        // should this clear the byte it's pointing to? it'll be overwritten on next push
    }

    /// Sets the remaining cycle pauses appropriately, and returns the number of bytes
    /// the program counter should advance. The cycle pause count should be _one lower than
    /// the documentation says_, because we're using up the first cycle executing the
    /// instruction in the first place.
    fn set_pause_and_return_shift(&mut self, pause: u8, op: &Opcode) -> u16 {
        self.remaining_pause = pause;
        op.1.byte_count()
    }

    fn _group_1_pause_and_shift(&mut self, op: &Opcode) -> u16 {
        // ZeroPageY isn't actually part of these, but LDX allows it and is otherwise identical
        match op.1 {
            Immediate => self.set_pause_and_return_shift(1, op),
            ZeroPage => self.set_pause_and_return_shift(2, op),
            ZeroPageX | ZeroPageY | Absolute => self.set_pause_and_return_shift(3, op),
            AbsoluteX | AbsoluteY => self.set_pause_and_return_shift(3, op),  // TODO page crossing???
            IndirectX => self.set_pause_and_return_shift(5, op),
            IndirectY => self.set_pause_and_return_shift(4, op),  // TODO page crossing???
            _ => unreachable!()
        }
    }

    fn _illegal_opcodes_pause_and_shift(&mut self, op: &Opcode) -> u16 {
        match op.1 {
            Absolute => self.set_pause_and_return_shift(5, op),
            AbsoluteX | AbsoluteY => self.set_pause_and_return_shift(6, op),
            ZeroPage => self.set_pause_and_return_shift(4, op),
            ZeroPageX => self.set_pause_and_return_shift(5, op),
            IndirectX | IndirectY => self.set_pause_and_return_shift(7, op),
            _ => unreachable!()
        }
    }

    /// Executes the opcode, updating all registers appropriately.
    fn execute_opcode(&mut self, op: &Opcode) {
        self.pc += match op.0 {
            ADC => self.adc(op),
            AND => self.and(op),
            ASL => self.asl(op),
            BIT => self.bit(op),
            EOR => self.eor(op),
            DEC => self.dec(op),
            DEX => self.dex(op),
            DEY => self.dey(op),
            INC => self.inc(op),
            INX => self.inx(op),
            INY => self.iny(op),
            JMP => self.jmp(op),
            JSR => self.jsr(op),
            LDA => self.lda(op),
            LDX => self.ldx(op),
            LDY => self.ldy(op),
            LSR => self.lsr(op),
            NOP => self.nop(op),
            ORA => self.ora(op),
            PHP => self.php(op),
            PHA => self.pha(op),
            PLA => self.pla(op),
            PLP => self.plp(op),
            ROL => self.rol(op),
            ROR => self.ror(op),
            RTI => self.rti(op),
            RTS => self.rts(op),
            SBC => self.sbc(op),
            STA => self.sta(op),
            STX => self.stx(op),
            STY => self.sty(op),

            // "illegal"
            DCP => self.dcp(op),
            ISC => self.isc(op),
            LAX => self.lax(op),
            SAX => self.sax(op),
            SLO => self.slo(op),
            SRE => self.sre(op),
            RRA => self.rra(op),
            RLA => self.rla(op),

            // comparisons
            CMP => self.compare(op, self.a),
            CPX => self.compare(op, self.x),
            CPY => self.compare(op, self.y),

            // branches
            BCS => self.branch(op, self.carry()),
            BCC => self.branch(op, !self.carry()),
            BEQ => self.branch(op, self.zero()),
            BNE => self.branch(op, !self.zero()),
            BVS => self.branch(op, self.overflow()),
            BVC => self.branch(op, !self.overflow()),
            BMI => self.branch(op, self.negative()),
            BPL => self.branch(op, !self.negative()),

            // simple flag settings
            // TODO handle cycle length for these (they're all 2)
            SEC => { self.set_carry(true); 1} ,
            SED => { self.set_decimal(true); 1},
            SEI => { self.set_interrupt_disable(true); 1},
            CLC => { self.set_carry(false); 1},
            CLD => { self.set_decimal(false); 1},
            CLI => { self.set_interrupt_disable(false); 1},
            CLV => { self.set_overflow(false); 1},

            // transfers
            TAX => self.transfer_op(|cpu| { cpu.x = cpu.a; (cpu.x, true) }),
            TAY => self.transfer_op(|cpu| { cpu.y = cpu.a; (cpu.y, true) }),
            TXS => self.transfer_op(|cpu| { cpu.s = cpu.x; (cpu.s, false) }),
            TSX => self.transfer_op(|cpu| { cpu.x = cpu.s; (cpu.x, true) }),
            TXA => self.transfer_op(|cpu| { cpu.a = cpu.x; (cpu.a, true) }),
            TYA => self.transfer_op(|cpu| { cpu.a = cpu.y; (cpu.a, true) }),

            _ => unimplemented!("{:?}", op)
        }
    }

    fn set_flag(&mut self, mask: u8, set_to: bool) {
        match set_to {
            true => self.p |= mask,
            false => self.p &= !mask
        }
    }

    fn carry(&self) -> bool {
        (self.p & CARRY_FLAG) != 0
    }

    fn zero(&self) -> bool {
        (self.p & ZERO_FLAG) != 0
    }

    fn overflow(&self) -> bool {
        (self.p & OVERFLOW_FLAG) != 0
    }

    fn negative(&self) -> bool {
        (self.p & NEGATIVE_FLAG) != 0
    }

    fn set_carry(&mut self, carry: bool) {
        self.set_flag(CARRY_FLAG, carry);
    }

    fn set_zero(&mut self, zero: bool) {
        self.set_flag(ZERO_FLAG, zero);
    }

    fn set_interrupt_disable(&mut self, interrupt_disable: bool) {
        self.set_flag(0b0000_0100, interrupt_disable);
    }

    fn set_decimal(&mut self, decimal: bool) {
        self.set_flag(0b0000_1000, decimal);
    }

    fn set_overflow(&mut self, overflow: bool) {
        self.set_flag(OVERFLOW_FLAG, overflow);
    }

    fn set_negative(&mut self, negative: bool) {
        self.set_flag(NEGATIVE_FLAG, negative);
    }

    fn set_value_flags(&mut self, val: u8) {
        self.set_negative((val & SIGN_BIT) != 0);
        self.set_zero(val == 0);
    }

    // Opcodes!

    fn adc(&mut self, op: &Opcode) -> u16 {
        let addr = self.resolve_addr(op);
        let value = self.mem.get(addr);
        let signed_sum = (value as i8 as i16) + (self.a as i8 as i16) + (self.carry() as i16);
        let (first_add, overflowing1) = self.a.overflowing_add(value);
        let (second_add, overflowing2) = first_add.overflowing_add(if self.carry() { 1 } else { 0 });
        self.a = second_add;
        self.set_carry(overflowing1 || overflowing2);
        self.set_value_flags(self.a);
        self.set_overflow(signed_sum < -128 || signed_sum > 127);
        self._group_1_pause_and_shift(op)
    }

    fn and(&mut self, op: &Opcode) -> u16 {
        let operand = self.mem.get(self.resolve_addr(op));
        self.a &= operand;
        self.set_value_flags(self.a);
        self._group_1_pause_and_shift(op)
    }

    fn asl(&mut self, op: &Opcode) -> u16 {
        if op.1 == Accumulator {
            let bit_7 = (self.a & 0b1000_0000) != 0;
            self.a <<= 1;
            self.set_carry(bit_7 as bool);
            self.set_value_flags(self.a);
        } else {
            let addr = self.resolve_addr(op);
            let mut value = self.mem.get(addr);
            let bit_7 = (value & 0b1000_0000) != 0;
            value <<= 1;
            self.set_carry(bit_7 as bool);
            self.set_value_flags(value);
            self.mem.set(addr, value);
        }
        match op.1 {
            Accumulator => self.set_pause_and_return_shift(1, op),
            ZeroPage => self.set_pause_and_return_shift(4, op),
            ZeroPageX | Absolute => self.set_pause_and_return_shift(5, op),
            AbsoluteX => self.set_pause_and_return_shift(6, op),

            // used by SLO, overridden by it
            AbsoluteY | IndirectX | IndirectY => 0,
            _ => unreachable!()
        }
    }

    fn bit(&mut self, op: &Opcode) -> u16 {
        let value = self.mem.get(self.resolve_addr(op));
        self.set_negative((value & SIGN_BIT) != 0);
        self.set_overflow((value & 0b0100_0000) != 0);
        self.set_zero((value & self.a) == 0);
        match op.1 {
            ZeroPage => self.set_pause_and_return_shift(2, op),
            Absolute => self.set_pause_and_return_shift(3, op),
            _ => unreachable!()
        }
    }

    fn branch(&mut self, op: &Opcode, branch: bool) -> u16 {
        self.remaining_pause = 2;
        if branch {
            self.pc = self.resolve_addr(op);
            self.remaining_pause += 1;
            // TODO +2 if page crossed???
        }
        2 // account for the 2 bytes this instruction used
    }

    fn compare(&mut self, op: &Opcode, to: u8) -> u16 {
        let value = self.mem.get(self.resolve_addr(op));
        self.set_carry(to >= value);
        self.set_zero(to == value);
        let result = to.wrapping_sub(value);
        self.set_negative(result >= 128);
        self._group_1_pause_and_shift(op)
    }

    fn dcp(&mut self, op: &Opcode) -> u16 {
        self.dec(op);
        self.compare(op, self.a);
        self._illegal_opcodes_pause_and_shift(op)
    }

    fn dec(&mut self, op: &Opcode) -> u16 {
        let addr = self.resolve_addr(op);
        let (new_val, shift) = self.increment(self.mem.get(addr), op, true);
        self.mem.set(addr, new_val);
        shift
    }

    fn dex(&mut self, op: &Opcode) -> u16 {
        let (new_val, shift) = self.increment(self.x, op, true);
        self.x = new_val;
        shift
    }

    fn dey(&mut self, op: &Opcode) -> u16 {
        let (new_val, shift) = self.increment(self.y, op, true);
        self.y = new_val;
        shift
    }

    fn eor(&mut self, op: &Opcode) -> u16 {
        self.a ^= self.mem.get(self.resolve_addr(op));
        self.set_value_flags(self.a);
        self._group_1_pause_and_shift(op)
    }

    fn increment(&mut self, mut val: u8, op: &Opcode, decrement: bool) -> (u8, u16) {
        if decrement { val = val.wrapping_sub(1); } else { val = val.wrapping_add(1); }
        self.set_value_flags(val);
        (val, match op.1 {
            Implicit => self.set_pause_and_return_shift(1, op),
            ZeroPage => self.set_pause_and_return_shift(4, op),
            ZeroPageX | Absolute => self.set_pause_and_return_shift(5, op),
            AbsoluteX => self.set_pause_and_return_shift(6, op),
            _ => 0 // reachable only via illegal opcodes, which overwrite this anyway
        })
    }

    fn inc(&mut self, op: &Opcode) -> u16 {
        let addr = self.resolve_addr(op);
        let (new_val, shift) = self.increment(self.mem.get(addr), op, false);
        self.mem.set(addr, new_val);
        shift
    }

    fn inx(&mut self, op: &Opcode) -> u16 {
        let (new_val, shift) = self.increment(self.x, op, false);
        self.x = new_val;
        shift
    }

    fn iny(&mut self, op: &Opcode) -> u16 {
        let (new_val, shift) = self.increment(self.y, op, false);
        self.y = new_val;
        shift
    }

    fn isc(&mut self, op: &Opcode) -> u16 {
        self.inc(op);
        self.sbc(op);
        self._illegal_opcodes_pause_and_shift(op)
    }

    fn jmp(&mut self, op: &Opcode) -> u16 {
        self.pc = self.resolve_addr(op);
        match op.1 {
            Absolute => self.remaining_pause = 2,
            Indirect => self.remaining_pause = 4,
            _ => unreachable!()
        }
        0 // this is a jump, we don't advance normally
    }

    fn jsr(&mut self, op: &Opcode) -> u16 {
        let addr = self.resolve_addr(op);
        let bytes = (self.pc + 2).to_be_bytes();
        self.stack_push(bytes[0]);
        self.stack_push(bytes[1]);
        self.pc = addr;
        self.remaining_pause = 5;
        0 // this is a jump, we don't advance normally
    }

    fn lax(&mut self, op: &Opcode) -> u16 {
        self.lda(op);
        self.transfer_op(|cpu| { cpu.x = cpu.a; (cpu.x, true) });
        self.set_value_flags(self.x);
        match op.1 {
            IndirectX => self.set_pause_and_return_shift(5, op),
            ZeroPage => self.set_pause_and_return_shift(2, op),
            Absolute | ZeroPageY => self.set_pause_and_return_shift(3, op),
            IndirectY => self.set_pause_and_return_shift(3, op),
            AbsoluteY => self.set_pause_and_return_shift(3, op),
            _ => unreachable!()
        }
    }

    // TODO these only differ by register; is there some way to make them one func?

    fn lda(&mut self, op: &Opcode) -> u16 {
        let value = self.mem.get(self.resolve_addr(op));
        self.a = value;
        self.set_value_flags(value);
        self._group_1_pause_and_shift(op)
    }

    fn ldx(&mut self, op: &Opcode) -> u16 {
        let value = self.mem.get(self.resolve_addr(op));
        self.x = value;
        self.set_value_flags(value);
        self._group_1_pause_and_shift(op)
    }

    fn ldy(&mut self, op: &Opcode) -> u16 {
        let value = self.mem.get(self.resolve_addr(op));
        self.y = value;
        self.set_value_flags(value);
        self._group_1_pause_and_shift(op)
    }

    fn lsr(&mut self, op: &Opcode) -> u16 {
        if op.1 == Accumulator {
            let bit_1 = (self.a & 0b1) != 0;
            self.a >>= 1;
            self.set_carry(bit_1 as bool);
            self.set_value_flags(self.a);
        } else {
            let addr = self.resolve_addr(op);
            let mut value = self.mem.get(addr);
            let bit_1 = (value & 0b1) != 0;
            value >>= 1;
            self.set_carry(bit_1 as bool);
            self.set_value_flags(value);
            self.mem.set(addr, value);
        }
        match op.1 {
            Accumulator => self.set_pause_and_return_shift(1, op),
            ZeroPage => self.set_pause_and_return_shift(4, op),
            ZeroPageX | Absolute => self.set_pause_and_return_shift(5, op),
            AbsoluteX => self.set_pause_and_return_shift(6, op),

            // used by SRE and overridden by it
            AbsoluteY | IndirectX | IndirectY => 0,
            _ => unreachable!()
        }
    }

    fn nop(&mut self, op: &Opcode) -> u16 {
        self.remaining_pause = 2;
        op.1.byte_count()
    }

    fn ora(&mut self, op: &Opcode) -> u16 {
        self.a |= self.mem.get(self.resolve_addr(op));
        self.set_value_flags(self.a);
        self._group_1_pause_and_shift(op)
    }

    fn php(&mut self, _op: &Opcode) -> u16 {
        self.stack_push(self.p | PHP_MASK);
        self.remaining_pause = 2;
        1
    }

    fn pha(&mut self, _op: &Opcode) -> u16 {
        self.stack_push(self.a);
        self.remaining_pause = 2;
        1
    }

    fn pla(&mut self, _op: &Opcode) -> u16 {
        self.a = self.stack_pop();
        self.set_value_flags(self.a);
        self.remaining_pause = 3;
        1
    }

    fn plp(&mut self, _op: &Opcode) -> u16 {
        self.p = self.stack_pop() & !PHP_MASK;
        self.remaining_pause = 3;
        1
    }

    fn rol_internal(&mut self, value: u8) -> u8 {
        let old_carry = self.carry();
        self.set_carry((value & 0b1000_0000) != 0);
        let mut out = value << 1;
        if old_carry {
            out ^= 0b0000_0001;
        };
        self.set_value_flags(out);
        out
    }

    fn rol(&mut self, op: &Opcode) -> u16 {
        if op.1 == Accumulator {
            self.a = self.rol_internal(self.a);
        } else {
            let addr = self.resolve_addr(op);
            let new_val = self.rol_internal(self.mem.get(addr));
            self.mem.set(addr, new_val);
        }
        match op.1 {
            Accumulator => self.set_pause_and_return_shift(1, op),
            ZeroPage => self.set_pause_and_return_shift(4, op),
            ZeroPageX | Absolute => self.set_pause_and_return_shift(5, op),
            AbsoluteX => self.set_pause_and_return_shift(6, op),

            // used by RLA, overridden by it
            AbsoluteY | IndirectX | IndirectY => 0,
            _ => unreachable!()
        }
    }

    fn ror_internal(&mut self, value: u8) -> u8 {
        let old_carry = self.carry();
        self.set_carry((value & 1) != 0);
        let mut out = value >> 1;
        if old_carry {
            out ^= 0b1000_0000;
        };
        self.set_value_flags(out);
        out
    }

    fn ror(&mut self, op: &Opcode) -> u16 {
        if op.1 == Accumulator {
            self.a = self.ror_internal(self.a);
        } else {
            let addr = self.resolve_addr(op);
            let new_val = self.ror_internal(self.mem.get(addr));
            self.mem.set(addr, new_val);
        }
        match op.1 {
            Accumulator => self.set_pause_and_return_shift(1, op),
            ZeroPage => self.set_pause_and_return_shift(4, op),
            ZeroPageX | Absolute => self.set_pause_and_return_shift(5, op),
            AbsoluteX => self.set_pause_and_return_shift(6, op),

            // used by RRA, overridden by it
            AbsoluteY | IndirectX | IndirectY => 0,
            _ => unreachable!()
        }
    }

    fn rra(&mut self, op: &Opcode) -> u16 {
        self.ror(op);
        self.adc(op);
        self._illegal_opcodes_pause_and_shift(op)
    }

    fn rla(&mut self, op: &Opcode) -> u16 {
        self.rol(op);
        self.and(op);
        self._illegal_opcodes_pause_and_shift(op)
    }

    fn rti(&mut self, op: &Opcode) -> u16 {
        self.p = self.stack_pop();
        self.remaining_pause += 1;
        self.rts(op);
        0  // do not advance one byte!
    }

    fn rts(&mut self, _op: &Opcode) -> u16 {
        let low = self.stack_pop();
        let high = self.stack_pop();
        self.pc = join_bytes(high, low);
        self.remaining_pause += 5;  // increment so this can be called from #rti
        1  // advance once byte!
    }

    fn sax(&mut self, op: &Opcode) -> u16 {
        self.mem.set(self.resolve_addr(op), self.a & self.x);
        match op.1 {
            ZeroPage => self.set_pause_and_return_shift(2, op),
            Absolute | ZeroPageY => self.set_pause_and_return_shift(3, op),
            IndirectX => self.set_pause_and_return_shift(5, op),
            _ => unreachable!()
        }
    }

    fn sbc(&mut self, op: &Opcode) -> u16 {
        let value = self.mem.get(self.resolve_addr(op));
        let signed_sum = (value as i8 as i16) - (self.a as i8 as i16) - (1 - (self.carry() as i16));
        let (first_sub, overflowing1) = self.a.overflowing_sub(value);
        let (second_sub, overflowing2) = first_sub.overflowing_sub(1 - (self.carry() as u8));
        self.a = second_sub;
        self.set_carry(!(overflowing1 || overflowing2));
        self.set_value_flags(self.a);
        self.set_overflow(signed_sum < -128 || signed_sum > 127);
        self._group_1_pause_and_shift(op)
    }

    fn slo(&mut self, op: &Opcode) -> u16 {
        self.asl(op);
        self.ora(op);
        self._illegal_opcodes_pause_and_shift(op)
    }

    fn sre(&mut self, op: &Opcode) -> u16 {
        self.lsr(op);
        self.eor(op);
        self._illegal_opcodes_pause_and_shift(op)
    }

    fn store(&mut self, op: &Opcode, value: u8) -> u16 {
        self.mem.set(self.resolve_addr(op), value);
        match op.1 {
            ZeroPage => self.set_pause_and_return_shift(2, op),
            ZeroPageX | ZeroPageY | Absolute => self.set_pause_and_return_shift(3, op),
            AbsoluteX | AbsoluteY => self.set_pause_and_return_shift(4, op),
            IndirectX | IndirectY => self.set_pause_and_return_shift(5, op),
            _ => unreachable!()
        }
    }

    fn sta(&mut self, op: &Opcode) -> u16 {
        self.store(op, self.a)
    }

    fn stx(&mut self, op: &Opcode) -> u16 {
        self.store(op, self.x)
    }

    fn sty(&mut self, op: &Opcode) -> u16 {
        self.store(op, self.y)
    }

    fn transfer_op(&mut self, func: fn(&mut Cpu) -> (u8, bool)) -> u16 {
        let (new_val, update_flags) = func(self);
        if update_flags {
            self.set_value_flags(new_val);
        }
        self.remaining_pause = 1;
        1
    }

}

impl Clocked for Cpu {
    fn tick(&mut self) {
        if self.remaining_pause > 0 {
            self.remaining_pause -= 1;
            return
        }

        self.instruction_counter += 1;
        let op = opcodes::resolve(self.mem.get(self.pc));
        println!("{:?} @ {:04X?} (A:{:02X?} X:{:02X?} Y:{:02X?} P:{:02X?} SP:{:02X?}): {:?}: {:04X?}",
                 self.instruction_counter, self.pc, self.a, self.x, self.y, self.p, self.s,
                 op, self.resolve_addr(op));
        self.execute_opcode(op);
    }
}
