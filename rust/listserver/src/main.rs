#![allow(warnings)]
#![feature(proc_macro_hygiene, decl_macro, cell_update)]

use std::sync::Mutex;
use std::collections::HashMap;

extern crate rocket;
extern crate rocket_contrib;
#[macro_use] extern crate serde_derive;

use serde::{Serialize, Deserialize};

use rocket::*;
use rocket::State;
use rocket::response::status;
use rocket::http::{Status, ContentType};
use rocket::response::Responder;
use rocket::response::ResponseBuilder;
use rocket_contrib::json::{Json};

mod base;
mod service;

use service::list_service::ListService;
use service::list_service_in_memory::ListServiceInMemory;
use base::base::{ID, Element};
use base::base::ListResult;
use base::base::ListError;
use crate::service::list_service::BareList;

/*
 * Anything given to "manage" must be thread-safe and transferable to another thread.
 * Also have a static lifetime. Not sure how static is enforced for us.
 * pub fn manage<T: Send + Sync + 'static>(self, state: T) -> Self
*/

// TODO. Document response json for each call.

type ServiceState<'r> = State<'r, Box<dyn ListService>>;
type JsonResult<T> = Json<ListResult<T>>;

fn main() {
    println!("hello");
    let list_service: Box<dyn ListService> = Box::new(ListServiceInMemory {store: Mutex::new(HashMap::new())});

    rocket::ignite()
        // .mount("/stacks", routes![add, get, push, pop, is_empty])
        .mount("/lists", routes![create, get, add_element, lookup])
        .manage(list_service)
        .launch();
}

/**
 * Example - http://localhost:8000/lists/create/drinks
 * Use header: key: Content-Type value: application/json
 * Response: { "Ok": null }
 */
#[post("/create/<id>", format = "json")]
fn create(id: ID, state: ServiceState) -> JsonResult<()> {
    let service: &Box<dyn ListService> = state.inner();
    let result = service.create(id);
    Json(result)
}

/**
 * Example: http://localhost:8000/lists/get/drinks
 * Use header: key: Content-Type value: application/json
 * Response: { "Ok": ["tea", "coffee"] }
 */
#[get("/get/<id>", format = "json")]
fn get(id: ID, state: ServiceState) -> JsonResult<BareList> {
    let service: &Box<dyn ListService> = state.inner();
    Json(service.get(&id))
}

/**
 * Example: http://localhost:8000/lists/add/drinks/tea/0
 * Use header: key: Content-Type value: application/json
 * Response: { "Ok": null }
 */
#[post("/add/<id>/<element>/<index>", format ="json")]
fn add_element(id: ID, element: Element, index: usize, state: ServiceState) -> JsonResult<()> {
    let service: &Box<dyn ListService> = state.inner();
    Json(service.add_element(&id, element, index))
}

/**
 * Example: http://localhost:8000/lists/lookup/drinks/0
 * Use header: key: Content-Type value: application/json
 * Response: { "Ok": "tea" }
 */
#[get("/lookup/<id>/<index>", format = "json")]
fn lookup(id: ID, index: usize, state: ServiceState) -> JsonResult<Option<Element>> {
    let service: &Box<dyn ListService> = state.inner();
    Json(service.get_element(&id, index))
}



