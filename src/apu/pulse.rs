use crate::apu::Channel;
use crate::apu::components::{Envelope, LengthCounter, LENGTH_COUNTER_TABLE, Sweep, SweepNegator};
use crate::common::Clocked;

enum Duty {
    Eighth,
    Fourth,
    Half,
    ThreeFourths
}

impl Duty {
    fn waveform(&self) -> u8 {
        match self {
            Duty::Eighth =>       0b0100_0000,
            Duty::Fourth =>       0b0110_0000,
            Duty::Half =>         0b0111_1000,
            Duty::ThreeFourths => 0b1001_1111,
        }
    }

    fn active(&self, step: u8) -> bool {
        self.waveform() >> (7 - (step & 0b0000_0111)) & 1 != 0
    }
}

impl Default for Duty {
    fn default() -> Self {
        Duty::Eighth
    }
}

pub struct Pulse {
    duty: Duty,
    step: u8,
    timer: u16,
    period: u16,

    pub envelope: Envelope,
    pub sweep: Sweep,
    pub length_counter: LengthCounter,
}

impl Pulse {
    pub fn new(negator: SweepNegator) -> Pulse {
        Pulse {
            sweep: Sweep::new(negator),

            duty: Default::default(),
            step: 0,
            timer: 0,
            period: 0,
            envelope: Default::default(),
            length_counter: Default::default()
        }
    }

    pub fn clock_half_frame(&mut self) {
        self.length_counter.tick();
        self.sweep.tick();
    }
}

impl Channel for Pulse {
    fn set_register(&mut self, addr: u16, value: u8) {
        match addr & 0b0000_0011 {
            0 => {
                self.duty = match value >> 6 {
                    0 => Duty::Eighth,
                    1 => Duty::Fourth,
                    2 => Duty::Half,
                    3 => Duty::ThreeFourths,
                    _ => unreachable!()
                };
                self.length_counter.halt = (value & 0b0010_0000) == 0;
                self.envelope.set_register(addr, value);
            },
            1 => self.sweep.set_register(value),
            2 => {
                self.period = self.period & 0xFF00 | value as u16;
                self.sweep.update_target_period(self.period);
            },
            3 => {
                self.period = self.period & 0x00FF | (((value & 0b0000_0111) as u16) << 8);
                self.sweep.update_target_period(self.period);
                self.length_counter.length = LENGTH_COUNTER_TABLE[usize::from(value >> 3)];
                self.envelope.start = true;
                self.step = 0;
            },
            _ => unreachable!()
        }
    }

    fn sample(&mut self) -> Option<f32> {
        if !self.length_counter.silenced() && !self.sweep.silenced() && self.duty.active(self.step) {
            self.envelope.sample()
        } else {
            None
        }
    }
}

impl Clocked for Pulse {
    fn tick(&mut self) {
        if self.timer == 0 {
            self.step = self.step.wrapping_sub(1) & 0b0000_0111;
            self.timer = self.sweep.period();
        } else {
            self.timer -= 1;
        }
    }
}
