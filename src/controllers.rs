use sdl2::event::Event;
use sdl2::keyboard::Keycode;

pub enum Button {
    A,
    B,
    Select,
    Start,
    Up,
    Down,
    Left,
    Right
}

pub enum ControllerEvent {
    JoyDown,
    JoyUp
}

use Button::*;
use ControllerEvent::*;

impl Button {
    fn index(&self) -> u8 {
        match self {
            A =>      0b0000_0001,
            B =>      0b0000_0010,
            Select => 0b0000_0100,
            Start =>  0b0000_1000,
            Up =>     0b0001_0000,
            Down =>   0b0010_0000,
            Left =>   0b0100_0000,
            Right =>  0b1000_0000
        }
    }
}

pub struct Controllers {
    controller_1_active_buttons: u8,
    controller_2_active_buttons: u8,
    polling_requested: bool,
    controller_1_report_index: u8,
    controller_2_report_index: u8,
}

impl Controllers {
    pub fn new() -> Controllers {
        Controllers {
            controller_1_active_buttons: 0,
            controller_2_active_buttons: 0,
            polling_requested: false,
            controller_1_report_index: 1,
            controller_2_report_index: 1
        }
    }

    pub fn set_polling(&mut self, polling_requested: bool) {
        match polling_requested {
            true => if !self.polling_requested {
                        self.polling_requested = true;
                        self.controller_1_report_index = 1;
                        self.controller_2_report_index = 1;
                    },
            false => self.polling_requested = false
        }
    }

    fn controller_1_event(&mut self, event: ControllerEvent, button: Button) {
        match event {
            ControllerEvent::JoyDown => self.controller_1_active_buttons |= button.index(),
            ControllerEvent::JoyUp => self.controller_1_active_buttons &= !button.index(),
        };
    }

    #[cfg(test)]
    fn controller_2_event(&mut self, event: ControllerEvent, button: Button) {
        match event {
            ControllerEvent::JoyDown => self.controller_2_active_buttons |= button.index(),
            ControllerEvent::JoyUp => self.controller_2_active_buttons &= !button.index(),
        }
    }

    pub fn event(&mut self, event: Event) {
        match event {
            Event::KeyDown { keycode: Some(Keycode::L), .. } => self.controller_1_event(JoyDown, A),
            Event::KeyDown { keycode: Some(Keycode::K), .. } => self.controller_1_event(JoyDown, B),
            Event::KeyDown { keycode: Some(Keycode::Return), .. } => self.controller_1_event(JoyDown, Start),
            Event::KeyDown { keycode: Some(Keycode::Space), .. } => self.controller_1_event(JoyDown, Select),
            Event::KeyDown { keycode: Some(Keycode::W), .. } => self.controller_1_event(JoyDown, Up),
            Event::KeyDown { keycode: Some(Keycode::S), .. } => self.controller_1_event(JoyDown, Down),
            Event::KeyDown { keycode: Some(Keycode::A), .. } => self.controller_1_event(JoyDown, Left),
            Event::KeyDown { keycode: Some(Keycode::D), .. } => self.controller_1_event(JoyDown, Right),

            Event::KeyUp { keycode: Some(Keycode::L), .. } => self.controller_1_event(JoyUp, A),
            Event::KeyUp { keycode: Some(Keycode::K), .. } => self.controller_1_event(JoyUp, B),
            Event::KeyUp { keycode: Some(Keycode::Return), .. } => self.controller_1_event(JoyUp, Start),
            Event::KeyUp { keycode: Some(Keycode::Space), .. } => self.controller_1_event(JoyUp, Select),
            Event::KeyUp { keycode: Some(Keycode::W), .. } => self.controller_1_event(JoyUp, Up),
            Event::KeyUp { keycode: Some(Keycode::S), .. } => self.controller_1_event(JoyUp, Down),
            Event::KeyUp { keycode: Some(Keycode::A), .. } => self.controller_1_event(JoyUp, Left),
            Event::KeyUp { keycode: Some(Keycode::D), .. } => self.controller_1_event(JoyUp, Right),

            _ => {}
        }
    }

    fn report(&self, active_buttons: u8, report_index: u8) -> (u8, u8) {
        let report = if report_index == 0 || (active_buttons & report_index) != 0 {
            1
        } else {
            0
        };
        (report, if self.polling_requested {report_index} else {report_index << 1})
    }

    pub fn report_controller_1(&mut self) -> u8 {
        let (report, report_index) = self.report(self.controller_1_active_buttons, self.controller_1_report_index);
        self.controller_1_report_index = report_index;
        report
    }

    pub fn report_controller_2(&mut self) -> u8 {
        let (report, report_index) = self.report(self.controller_2_active_buttons, self.controller_2_report_index);
        self.controller_2_report_index = report_index;
        report
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_polling_loop() {
        let mut c = Controllers::new();
        c.controller_1_event(ControllerEvent::JoyDown, A);
        c.controller_1_event(ControllerEvent::JoyDown, Up);
        c.controller_2_event(ControllerEvent::JoyDown, Start);

        c.set_polling(true);
        c.set_polling(false);

        assert_eq!(c.report_controller_1(), 1);
        assert_eq!(c.report_controller_1(), 0);
        assert_eq!(c.report_controller_1(), 0);
        assert_eq!(c.report_controller_1(), 0);
        assert_eq!(c.report_controller_1(), 1);
        assert_eq!(c.report_controller_1(), 0);
        assert_eq!(c.report_controller_1(), 0);
        assert_eq!(c.report_controller_1(), 0);

        assert_eq!(c.report_controller_2(), 0);
        assert_eq!(c.report_controller_2(), 0);
        assert_eq!(c.report_controller_2(), 0);
        assert_eq!(c.report_controller_2(), 1);
        assert_eq!(c.report_controller_2(), 0);
        assert_eq!(c.report_controller_2(), 0);
        assert_eq!(c.report_controller_2(), 0);
        assert_eq!(c.report_controller_2(), 0);
    }

    #[test]
    fn test_polling_reset() {
        let mut c = Controllers::new();
        c.controller_1_event(ControllerEvent::JoyDown, A);
        c.controller_1_event(ControllerEvent::JoyDown, Up);

        c.set_polling(true);
        c.set_polling(false);

        assert_eq!(c.report_controller_1(), 1);
        assert_eq!(c.report_controller_1(), 0);
        assert_eq!(c.report_controller_1(), 0);
        assert_eq!(c.report_controller_1(), 0);
        assert_eq!(c.report_controller_1(), 1);
        assert_eq!(c.report_controller_1(), 0);
        assert_eq!(c.report_controller_1(), 0);
        assert_eq!(c.report_controller_1(), 0);

        // after 8 we should keep getting 1s
        assert_eq!(c.report_controller_1(), 1);
        assert_eq!(c.report_controller_1(), 1);
        assert_eq!(c.report_controller_1(), 1);
        assert_eq!(c.report_controller_1(), 1);

        // reset the loop
        c.set_polling(true);
        c.set_polling(false);

        assert_eq!(c.report_controller_1(), 1);
        assert_eq!(c.report_controller_1(), 0);
        assert_eq!(c.report_controller_1(), 0);
        assert_eq!(c.report_controller_1(), 0);
        assert_eq!(c.report_controller_1(), 1);
        assert_eq!(c.report_controller_1(), 0);
        assert_eq!(c.report_controller_1(), 0);
        assert_eq!(c.report_controller_1(), 0);
    }

    #[test]
    fn test_joydown() {
        let mut c = Controllers::new();
        c.controller_1_event(ControllerEvent::JoyDown, B);
        c.controller_1_event(ControllerEvent::JoyDown, Select);
        c.controller_1_event(ControllerEvent::JoyDown, Left);

        c.set_polling(true);
        c.set_polling(false);

        assert_eq!(c.report_controller_1(), 0);
        assert_eq!(c.report_controller_1(), 1);
        assert_eq!(c.report_controller_1(), 1);
        assert_eq!(c.report_controller_1(), 0);
        assert_eq!(c.report_controller_1(), 0);
        assert_eq!(c.report_controller_1(), 0);
        assert_eq!(c.report_controller_1(), 1);
        assert_eq!(c.report_controller_1(), 0);

        c.set_polling(true);
        c.set_polling(false);

        c.controller_1_event(ControllerEvent::JoyUp, B);
        c.controller_1_event(ControllerEvent::JoyUp, Left);
        c.controller_1_event(ControllerEvent::JoyDown, A);
        c.controller_1_event(ControllerEvent::JoyDown, Up);

        assert_eq!(c.report_controller_1(), 1);
        assert_eq!(c.report_controller_1(), 0);
        assert_eq!(c.report_controller_1(), 1);
        assert_eq!(c.report_controller_1(), 0);
        assert_eq!(c.report_controller_1(), 1);
        assert_eq!(c.report_controller_1(), 0);
        assert_eq!(c.report_controller_1(), 0);
        assert_eq!(c.report_controller_1(), 0);
    }

    #[test]
    fn test_continual_poll() {
        let mut c = Controllers::new();
        c.controller_1_event(ControllerEvent::JoyDown, A);

        // if this is never set back to false, keep returning the value for A
        c.set_polling(true);

        assert_eq!(c.report_controller_1(), 1);
        assert_eq!(c.report_controller_1(), 1);
        assert_eq!(c.report_controller_1(), 1);
        assert_eq!(c.report_controller_1(), 1);
        assert_eq!(c.report_controller_1(), 1);
        assert_eq!(c.report_controller_1(), 1);
        assert_eq!(c.report_controller_1(), 1);
        assert_eq!(c.report_controller_1(), 1);
    }
}
