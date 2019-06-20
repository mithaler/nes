use crate::common::{Shared, shared, Clocked, CLOCKS_PER_FRAME, SAMPLES_PER_FRAME};

const SAMPLE_RATE: u16 = CLOCKS_PER_FRAME / SAMPLES_PER_FRAME / 2;

bitflags! {
    struct EnabledChannels: u8 {
        const PULSE_1 = 0b0000_0001;
        const PULSE_2 = 0b0000_0010;
        const TRIANGLE = 0b0000_0100;
        const NOISE = 0b0000_1000;
        const DMC = 0b0001_0000;
    }
}
bitflags! {
    struct FrameCounter: u8 {
        const FIVE_STEP = 0b1000_0000;
        const IRQ_INHIBIT = 0b0100_0000;
    }
}

trait Channel: Clocked {
    fn set_register(&mut self, addr: u16, value: u8);
    fn sample(&mut self) -> Option<f32>;
}

struct Envelope {
    start: bool,
    looping: bool,
    constant: bool,
    decay: u8,
    value: u8,
    current: u8
}

impl Envelope {
    fn new() -> Envelope {
        Envelope {
            start: false,
            looping: false,
            constant: false,
            decay: 0,
            value: 0,
            current: 0,
        }
    }
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

struct Pulse {
    duty: Duty,
    envelope: Envelope,
    step: u8,
    period: u16,
}

impl Pulse {
    fn new() -> Pulse {
        Pulse {
            duty: Duty::Eighth,
            envelope: Envelope::new(),
            step: 0,
            period: 0
        }
    }
}

impl Channel for Pulse {
    fn set_register(&mut self, addr: u16, value: u8) {
        match addr & 0b0000_0011 {
            0 => {
                self.duty = match (value & 0b1100_0000).rotate_left(2) {
                    0 => Duty::Eighth,
                    1 => Duty::Fourth,
                    2 => Duty::Half,
                    3 => Duty::ThreeFourths,
                    _ => unreachable!()
                };
                self.envelope.set_register(addr, value);
            },
            1 => {},
            2 => {self.period = self.period & 0xFF00 | value as u16},
            3 => {self.period = self.period & 0x00FF | (((value & 0b0000_0111) as u16) << 8)},
            _ => unreachable!()
        }
    }

    fn sample(&mut self) -> Option<f32> {
        if self.duty.active(self.step) {
            self.envelope.sample()
        } else {
            None
        }
    }
}

impl Clocked for Pulse {
    fn tick(&mut self) {
        self.step = self.step.wrapping_sub(1) & 0b0000_0111;
    }
}

pub struct Apu {
    cycle: u16,
    sample_step: u16,
    samples: Vec<f32>,
    pulse1: Pulse,
    pulse2: Pulse,
    enabled: EnabledChannels,
    frame_counter: FrameCounter,
}

impl Apu {
    pub fn new() -> Shared<Apu> {
        shared(Apu {
            cycle: 0,
            sample_step: 0,
            samples: Vec::with_capacity(SAMPLES_PER_FRAME as usize),
            pulse1: Pulse::new(),
            pulse2: Pulse::new(),
            enabled: EnabledChannels::empty(),
            frame_counter: FrameCounter::empty(),
        })
    }

    pub fn set_register(&mut self, addr: u16, value: u8) {
        match addr {
            0x4000 ... 0x4003 => self.pulse1.set_register(addr, value),
            0x4004 ... 0x4007 => self.pulse2.set_register(addr, value),
            0x4015 => self.enabled = EnabledChannels::from_bits_truncate(value),
            0x4017 => self.frame_counter = FrameCounter::from_bits_truncate(value),
            _ => warn!("Unimplemented APU register: {:04X} -> {:02X}", addr, value)
        }
    }

    fn sample(&mut self) {
        // https://wiki.nesdev.com/w/index.php/APU_Mixer
        let pulse_1 = match self.enabled.contains(EnabledChannels::PULSE_1) {
            true => self.pulse1.sample(),
            false => None
        }.unwrap_or(0f32);
        let pulse_2 = match self.enabled.contains(EnabledChannels::PULSE_2) {
            true => self.pulse2.sample(),
            false => None
        }.unwrap_or(0f32);
        let triangle = 0f32;
        let noise = 0f32;
        let dmc = 0f32;

        // TODO triangle, noise, dmc
        let pulse = 0.00752 * (pulse_1 + pulse_2);
        let tri_noise_dmc = 0.00851 * triangle + 0.00494 * noise + 0.00335 * dmc;
        self.samples.push(pulse + tri_noise_dmc)
    }

    pub fn samples(&mut self) -> &mut Vec<f32> {
        &mut self.samples
    }

    fn clock_channels(&mut self, half_frame: bool) {
        if half_frame {
            // clock length counters
            // clock sweep units
        }
        self.pulse1.envelope.tick();
        self.pulse2.envelope.tick();
        // clock triangle
    }
}

impl Clocked for Apu {
    fn tick(&mut self) {
        // https://wiki.nesdev.com/w/index.php/APU_Frame_Counter
        // I am treating CPU and APU cycles as equivalent, so these are multiplied by 2!
        if (self.cycle & 1) == 0 {
            self.pulse1.tick();
            self.pulse2.tick();
        }
        match self.cycle {
            7457 => self.clock_channels(false),
            14913 => self.clock_channels(true),
            22371 => self.clock_channels(false),
            29828 => {
                if self.frame_counter.bits() == 0 {
                    // irq
                }
            },
            29829 => {
                if !self.frame_counter.contains(FrameCounter::FIVE_STEP) {
                    self.clock_channels(true);
                    self.cycle = 0;
                }
            }
            37281 => {
                if self.frame_counter.contains(FrameCounter::FIVE_STEP) {
                    self.clock_channels(true);
                    self.cycle = 0;
                }
            }
            _ => {}
        }
        if (self.cycle & 1) == 0 {
            if self.sample_step == 0 {
                self.sample();
                self.sample_step += SAMPLE_RATE;
            } else {
                self.sample_step -= 1;
            }
        }
        self.cycle += 1;
    }
}
