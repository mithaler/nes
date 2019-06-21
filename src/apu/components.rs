use crate::common::Clocked;
use crate::apu::Channel;

pub const LENGTH_COUNTER_TABLE: [u8; 32] = [
    10, 254, 20, 2,  40, 4,  80, 6,  160, 8,  60, 10, 14, 12, 26, 14,  // 00 - 0F
    12, 16,  24, 18, 48, 20, 96, 22, 192, 24, 72, 26, 16, 28, 32, 30   // 10 - 1F
];

pub trait Silencer {
    fn silenced(&self) -> bool;
}

#[derive(Default, Debug)]
pub struct LengthCounter {
    pub length: u8,
    pub halt: bool
}

impl Clocked for LengthCounter {
    fn tick(&mut self) {
        if !self.halt && self.length > 0 {
            self.length -= 1;
        }
    }
}

impl LengthCounter {
    pub fn update_length(&mut self, index: u8) {
        self.length = LENGTH_COUNTER_TABLE[usize::from(index)];
    }
}

impl Silencer for LengthCounter {
    fn silenced(&self) -> bool {
        self.halt && self.length == 0
    }
}

#[derive(Default)]
pub struct Envelope {
    pub start: bool,
    looping: bool,
    constant: bool,
    decay: u8,
    value: u8,
    current: u8
}

impl Clocked for Envelope {
    fn tick(&mut self) {
        // https://wiki.nesdev.com/w/index.php/APU_Envelope
        if self.start {
            self.start = false;
            self.current = 0xF;
            self.decay = self.value;
        } else if self.decay > 0 {
            self.decay -= 1;
        } else {
            if self.current > 0 {
                self.current -= 1;
            } else if self.looping {
                self.current = 0xF;
            }
            self.decay = self.value;
        }
    }
}

impl Channel for Envelope {
    fn set_register(&mut self, _addr: u16, value: u8) {
        self.value = value & 0b0000_1111;
        self.current = self.value;
        self.looping = (value & 0b0010_0000) != 0;
        self.constant = (value & 0b0001_0000) != 0;
    }

    fn sample(&mut self) -> Option<f32> {
        if self.constant {
            Some(self.value as f32)
        } else {
            Some(self.current as f32)
        }
    }
}

#[derive(Debug)]
pub enum SweepNegator {
    Pulse1,
    Pulse2
}

impl SweepNegator {
    fn add(&self, current_period: u16, shifted: u16) -> u16 {
        match self {
            SweepNegator::Pulse1 => current_period.wrapping_sub(shifted).wrapping_sub(1),
            SweepNegator::Pulse2 => current_period.wrapping_sub(shifted),
        }
    }
}

#[derive(Debug)]
pub struct Sweep {
    enabled: bool,
    negate: bool,
    reload: bool,
    negator: SweepNegator,
    divider_period: u8,
    shift_count: u16,
    divider: u8,

    current_period: u16,
    pub target_period: u16
}

impl Sweep {
    pub fn new(negator: SweepNegator) -> Sweep {
        Sweep {
            negator,

            enabled: false,
            negate: false,
            reload: false,
            divider_period: 0,
            shift_count: 0,
            divider: 0,

            current_period: 0,
            target_period: 0
        }
    }

    pub fn set_register(&mut self, value: u8) {
        self.reload = true;
        self.enabled = (value & 0b1000_0000) != 0;
        self.negate = (value & 0b0000_1000) != 0;
        self.divider_period = ((value & 0b0111_0000) >> 4) + 1;
        self.shift_count = (value & 0b0000_0111) as u16;
    }

    pub fn update_target_period(&mut self, value: u16) {
        self.current_period = value;
        let shifted = value >> self.shift_count;
        self.target_period = if self.negate {
            self.negator.add(value, shifted)
        } else {
            value + shifted
        }
    }

    pub fn period(&self) -> u16 {
        match self.enabled {
            true => self.target_period,
            false => self.current_period
        }
    }
}

impl Silencer for Sweep {
    fn silenced(&self) -> bool {
        self.target_period > 0x7FF || self.current_period < 8
    }
}

impl Clocked for Sweep {
    fn tick(&mut self) {
        if self.divider == 0 &&
            self.enabled &&
            !self.silenced() &&
            self.shift_count != 0 {
            self.update_target_period(self.current_period);
        }
        if self.divider == 0 || self.reload {
            self.divider = self.divider_period;
            self.reload = false;
        } else {
            self.divider -= 1;
        }
    }
}
