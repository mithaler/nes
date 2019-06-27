use crate::apu::Channel;
use crate::common::Clocked;
use crate::apu::components::Silencer;
use crate::mappers::Mapper;

const PERIOD_TABLE: [u16; 16] = [
    428, 380, 340, 320, 286, 254, 226, 214, 190, 160, 142, 128, 106,  84,  72,  54
];

pub struct Dmc {
    pub (crate) irq: bool,
    looping: bool,
    silence: bool,

    period: u16,
    period_position: u16,
    bit_counter: u8,
    load_counter: u8,
    shift_register: u8,

    // Kludge: this really should be CpuMem, but fixing the circular dependency would require
    // redesigning everything. This still works fine because it only uses memory >= 0x8000.
    mapper: Mapper,
    sample_addr: u16,
    current_sample_addr: u16,
    sample_length: u16,
    pub (crate) bytes_remaining: u16,
    sample_buffer: Option<u8>,
}

impl Dmc {
    pub fn new(mapper: Mapper) -> Dmc {
        Dmc {
            mapper,

            irq: false,
            looping: false,
            silence: false,

            period: 0,
            period_position: 0,
            bit_counter: 0,
            load_counter: 0,
            shift_register: 0,

            sample_addr: 0xC000,
            current_sample_addr: 0xC000,
            sample_length: 0,
            bytes_remaining: 0,
            sample_buffer: None
        }
    }

    fn update_sample_buffer(&mut self) {
        if self.bytes_remaining == 0 {
            return;
        }
        self.sample_buffer = Some(self.mapper.borrow().get_cpu_space(self.current_sample_addr));
        // TODO: pause CPU for 4 cycles :(
        self.current_sample_addr = match self.current_sample_addr.checked_add(1) {
            Some(addr) => addr,
            None => 0x8000
        };
        self.bytes_remaining -= 1;
        if self.bytes_remaining == 0 && self.looping {
            self.bytes_remaining = self.sample_length;
            self.current_sample_addr = self.sample_addr;
        }
    }

    fn start_output_cycle(&mut self) {
        self.bit_counter = 8;
        match self.sample_buffer {
            None => self.silence = true,
            Some(sample) => {
                self.silence = false;
                self.shift_register = sample;
                self.sample_buffer = None;
            }
        }
    }

    fn update_load_counter(&mut self) {
        match self.shift_register & 1 {
            1 => {
                self.load_counter += 2;
                if self.load_counter >= 0x7F {
                    self.load_counter = 0x7F;
                }
            },
            0 => {
                self.load_counter = self.load_counter.checked_sub(2).unwrap_or(0);
            },
            _ => unreachable!()
        }
        self.shift_register >>= 1;
        self.bit_counter -= 1;
    }
}

impl Silencer for Dmc {
    fn silenced(&self) -> bool {
        self.silence
    }
}

impl Channel for Dmc {
    fn set_register(&mut self, addr: u16, value: u8) {
        match addr {
            0x4010 => {
                self.irq = (value & 0b1000_0000) != 0;
                self.looping = (value & 0b0100_0000) != 0;
                self.period = PERIOD_TABLE[(value & 0b0000_1111) as usize];
            },
            0x4011 => self.load_counter = value & 0b0111_1111,
            0x4012 => {
                self.sample_addr = 0xC000 | ((value as u16) << 6);
                self.current_sample_addr = self.sample_addr;
            },
            0x4013 => {
                self.sample_length = ((value as u16) << 4) + 1;
                self.bytes_remaining = self.sample_length;
            },
            _ => {}
        }
    }

    fn sample(&mut self) -> Option<f32> {
        match self.silenced() {
            true => None,
            false => Some(self.load_counter as f32)
        }
    }
}

impl Clocked for Dmc {
    fn tick(&mut self) {
        if self.period_position == 0 {
            if let None = self.sample_buffer {
                self.update_sample_buffer();
            }
            match self.bit_counter == 0 {
                true => self.start_output_cycle(),
                false => self.update_load_counter(),
            };
            self.period_position = self.period;
        } else {
            self.period_position -= 1;
        }
    }
}
