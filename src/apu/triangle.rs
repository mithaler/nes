use crate::apu::Channel;
use crate::apu::components::{LengthCounter, Silencer};
use crate::common::Clocked;

const SEQUENCE: [f32; 32] = [
    15f32, 14f32, 13f32, 12f32, 11f32, 10f32,  9f32,  8f32,
     7f32,  6f32,  5f32,  4f32,  3f32,  2f32,  1f32,  0f32,
     0f32,  1f32,  2f32,  3f32,  4f32,  5f32,  6f32,  7f32,
     8f32,  9f32, 10f32, 11f32, 12f32, 13f32,  14f32, 15f32
];

#[derive(Default, Debug)]
pub struct Triangle {
    period: u16,
    period_position: u16,
    sequence_step: usize,
    control: bool,
    linear_counter_reload: bool,
    linear_counter_reload_value: u8,
    linear_counter_value: u8,

    pub(crate) length_counter: LengthCounter,
}

impl Triangle {
    pub fn clock_quarter_frame(&mut self) {
        if self.linear_counter_reload {
            self.linear_counter_value = self.linear_counter_reload_value;
        } else if self.linear_counter_value > 0 {
            self.linear_counter_value -= 1;
        }

        if !self.control {
            self.linear_counter_reload = false;
        }
    }
}

impl Silencer for Triangle {
    fn silenced(&self) -> bool {
        self.linear_counter_value == 0 || self.length_counter.silenced()
    }
}

impl Clocked for Triangle {
    fn tick(&mut self) {
        if self.period_position == 0 {
            self.period_position = self.period + 1;
            if !self.silenced() {
                self.sequence_step += 1;
                if self.sequence_step > 31 {
                    self.sequence_step = 0;
                }
            }
        } else {
            self.period_position -= 1;
        }
    }
}

impl Channel for Triangle {
    fn set_register(&mut self, addr: u16, value: u8) {
        match addr {
            0x4008 => {
                self.control = (value & 0b1000_0000) != 0;
                self.length_counter.halt = self.control;
                self.linear_counter_reload_value = value & 0b0111_1111;
                self.linear_counter_reload = true;
            },
            0x400A => self.period = self.period & 0xFF00 | value as u16,
            0x400B => {
                self.period = self.period & 0x00FF | (((value & 0b0000_0111) as u16) << 8);
                self.length_counter.update_length(value >> 3);
                self.linear_counter_reload = true;
            }
            _ => {}
        }
    }

    fn sample(&mut self) -> Option<f32> {
        Some(SEQUENCE[self.sequence_step])
    }
}
