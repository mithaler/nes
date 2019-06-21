use crate::apu::components::{Envelope, LengthCounter, Silencer};
use crate::apu::Channel;
use crate::common::Clocked;

// https://wiki.nesdev.com/w/index.php/APU_Noise
const PERIOD_TABLE: [u16; 16] = [
    4, 8, 16, 32, 64, 96, 128, 160, 202, 254, 380, 508, 762, 1016, 2034, 4068
];

#[derive(Default, Debug)]
pub struct Noise {
    shift_register: u16,
    mode: bool, // if true, xor with bit 6 instead of 1
    period: u16,
    period_position: u16,

    pub(crate) envelope: Envelope,
    pub(crate) length_counter: LengthCounter
}

impl Noise {
    pub fn new() -> Noise {
        Noise {
            shift_register: 1,
            ..Default::default()
        }
    }

    fn update_shift_register(&mut self) {
        let feedback = (self.shift_register & 1) ^
            if self.mode { (self.shift_register >> 6) & 1}
            else         { (self.shift_register >> 1) & 1};
        self.shift_register >>= 1;
        if feedback > 0 {
            self.shift_register |= 0b0100_0000_0000_0000;
        }
    }
}

impl Channel for Noise {
    fn set_register(&mut self, addr: u16, value: u8) {
        match addr {
            0x400C => {
                self.length_counter.halt = (value & 0b0010_0000) == 0;
                self.envelope.set_register(addr, value);
            },
            0x400E => {
                self.mode = (value & 0b1000_0000) != 0;
                self.period = PERIOD_TABLE[(value & 0b0000_1111) as usize]
            },
            0x400F => {
                self.length_counter.update_length(value >> 3);
                self.envelope.start = true;
            },
            _ => {}
        }
    }

    fn sample(&mut self) -> Option<f32> {
        if (self.shift_register & 1) == 1 || self.length_counter.silenced() {
            None
        } else {
            self.envelope.sample()
        }
    }
}

impl Clocked for Noise {
    fn tick(&mut self) {
        if self.period_position == 0 {
            self.update_shift_register();
            self.period_position = self.period;
        } else {
            self.period_position -= 1;
        }
    }
}
