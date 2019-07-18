A NES emulator in Rust. Written as a learning project, to learn a) Rust and b) how emulators work.

### How to use

Build with `cargo build --release` and then run the `nes` binary with a ROM as the first argument, or simply run with `cargo run -- my/nes/rom.nes`.

#### Controls

Hard-coded at the moment.

Up: W  
Left: A  
Down: S  
Right: D  
A: L  
B: K  
Start: Enter  
Select: Space  

### Should I play games on this?

Oh god no. This is a toy project that I wrote for my edification. If you actually want to play games, use something that people actually maintain.

### Milestones

All of these things _mostly_ work. :)

- [x] CPU (`nestest` matches golden log)
- [x] PPU backgrounds (Donkey Kong title screen)
- [x] PPU sprites (Donkey Kong title screen sprite)
- [x] Keyboard controller input (Donkey Kong playable)
- [x] PPU scrolling (Super Mario Bros. playable)
- [x] INES Mapper 001 (Final Fantasy playable)
- [x] INES Mapper 002 (Megaman playable)
- [x] 8x16 sprites (Castlevania playable)
- [x] APU audio
- [x] INES Mapper 004 (Super Mario Bros. 3 playable)
