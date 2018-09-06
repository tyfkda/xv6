#![feature(lang_items)]
#![feature(start)]
#![no_std]
#![feature(panic_handler)]
#![feature(core_panic_info)]
#![feature(asm)]

use core::panic::PanicInfo;

fn hlt() {
  unsafe {
    asm!("hlt");
  }
}

static HELLO: &[u8] = b"Hello World!";

#[no_mangle]
pub extern "C" fn main() {
  let vga_buffer = 0xb8000 as *mut u16;

  for (i, &byte) in HELLO.iter().enumerate() {
    unsafe {
      *vga_buffer.offset(i as isize) = (byte as u16) | 0x0f00;
    }
  }

  loop {
    hlt();
  }
}

#[no_mangle]
pub extern fn mpenter() {
  loop {
    hlt();
  }
}

#[lang = "eh_personality"] extern fn eh_personality() {}
#[panic_handler]
pub fn panic_handler(_panic_info: &PanicInfo) -> ! {
  unimplemented!()
}
