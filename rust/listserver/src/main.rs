#![allow(warnings)]
#![feature(proc_macro_hygiene, decl_macro, cell_update)]

use listserver::rocketeer::service::list_service::{BareList, ListService};
use listserver::rocketeer::service::list_service_in_memory;
use listserver::rocketeer::rocketeer::ignite;

fn main() {
    println!("starting");
    let list_service: Box<dyn ListService> = list_service_in_memory::boxed_service_factory();
    ignite(list_service).launch();
}
