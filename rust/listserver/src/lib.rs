#![feature(proc_macro_hygiene)]
#![feature(decl_macro)]
#[macro_use] extern crate serde_derive;
#[macro_use] extern crate rocket_contrib;

pub mod base;
pub mod rocketeer;
