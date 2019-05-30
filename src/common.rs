pub trait Addressable {
    fn get(&self, addr: u16) -> u8;
    fn set(&mut self, addr: u16, value: u8);

    fn update(&mut self, addr: u16, func: &Fn (u8) -> u8) -> u8 {
        let curr = self.get(addr);
        let new = func(curr);
        self.set(addr, new);
        new
    }
}

pub trait Clocked {
    fn tick(&mut self);
}

pub fn join_bytes(high: u8, low: u8) -> u16 {
    (high as u16) << 8 | low as u16
}

mod tests {
    use super::join_bytes;

    #[test]
    fn it_joins_bytes() {
        assert_eq!(join_bytes(0xfc, 0xb3), 0xfcb3);
    }
}
