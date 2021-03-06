use std::cell::RefCell;
use std::rc::Rc;

pub const CLOCKS_PER_FRAME: f32 = 29780f32;
pub const SAMPLES_PER_FRAME: f32 = 735f32; // 44100 Hz / 60 FPS

/// Buses often return this if there's nothing connected to them.
pub const OPEN_BUS_VALUE: u8 = 0x40;

/// A heap-allocated, mutably shared object. Currently assumes the application is
/// single-threaded; the underlying implementation of how this is shared will change
/// if we ever make it multi-threaded.
pub type Shared<T> = Rc<RefCell<T>>;

pub fn shared<T>(t: T) -> Shared<T> {
    Rc::new(RefCell::new(t))
}

pub trait Addressable {
    fn get(&self, addr: u16) -> u8;
    fn set(&mut self, addr: u16, value: u8);

    fn update(&mut self, addr: u16, func: &dyn Fn(u8) -> u8) -> u8 {
        let curr = self.get(addr);
        let new = func(curr);
        self.set(addr, new);
        new
    }
}

pub trait Irq {
    fn irq(&self) -> bool;
}

pub trait Clocked {
    fn tick(&mut self);
}

pub fn join_bytes(high: u8, low: u8) -> u16 {
    (high as u16) << 8 | low as u16
}

#[cfg(test)]
mod tests {
    use super::join_bytes;

    #[test]
    fn it_joins_bytes() {
        assert_eq!(join_bytes(0xfc, 0xb3), 0xfcb3);
    }
}
