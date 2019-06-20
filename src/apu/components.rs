use crate::common::Clocked;
use crate::apu::Channel;

pub const LENGTH_COUNTER_TABLE: [u8; 32] = [
    10, 254, 20, 2,  40, 4,  80, 6,  160, 8,  60, 10, 14, 12, 26, 14,  // 00 - 0F
    12, 16,  24, 18, 48, 20, 96, 22, 192, 24, 72, 26, 16, 28, 32, 30   // 10 - 1F
];

#[derive(Default)]
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
    pub fn silenced(&self) -> bool {
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
