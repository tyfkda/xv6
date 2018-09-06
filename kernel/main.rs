#![feature(lang_items)]
#![feature(start)]
#![no_std]
#![feature(panic_handler)]
#![feature(core_panic_info)]
#![feature(asm)]

use core::panic::PanicInfo;

//const CRTPORT: u16 = 0x03d4;

//fn outb(port: u16, data: u8) {
//  unsafe {
//    asm!("out $0,$1" : : "0" (data), "1" (port));
//  }
//}

fn hlt() {
  unsafe {
    asm!("hlt");
  }
}

#[no_mangle]
pub extern fn main() {
  //outb(CRTPORT, 14);
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
