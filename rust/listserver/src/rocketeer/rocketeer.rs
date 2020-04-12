#![allow(warnings)]
#![feature(proc_macro_hygiene, decl_macro, cell_update)]

use std::fmt;
use std::collections::HashMap;
use std::sync::Mutex;

use serde::{Deserialize, Serialize};

use rocket::http::{ContentType, Status};
use rocket::response::status;
use rocket::response::{Responder, ResponseBuilder};
use rocket::{get, post, routes, State, Rocket};
use rocket_contrib::json::{Json, JsonValue};

use crate::base::{ID, Element, ListError, ListResult, BareList};
use crate::rocketeer::service::list_service::{ListService};
// use crate::rocketeer::service::list_service_in_memory::ListServiceInMemory;
use crate::rocketeer::service::list_service_in_memory;

/*
 * The controller layer of the listserver application using rocket.
 */

/*
 * Anything given to rocket's "manage" function must be thread-safe and
 * transferable to another thread, and have a static lifetime:
 *
 *      pub fn manage<T: Send + Sync + 'static>(self, state: T) -> Self
 *
 * TODO. Understand static. For now just copy pasting.
*/

type ServiceState<'r> = State<'r, Box<dyn ListService>>;
type JsonResult<T> = Json<ListResult<T>>;

const PATH_PREFIX: &str = "/lists";
const CREATE: &str = "create";
const ADD: &str = "add";
const GET: &str = "get";

pub fn ignite(list_service: Box<dyn ListService>) -> Rocket {
    // let list_service: Box<dyn ListService> = list_service_in_memory::boxed_service_factory();
    println!("igniting rocket");
    rocket::ignite()
        .mount(PATH_PREFIX, routes![create, get, add_element, lookup])
        .manage(list_service)
}

pub fn mk_create_url(id: &str) -> String {
    format!("{}/{}/{}", PATH_PREFIX, CREATE, id)
}

pub fn mk_add_element_url(id: &str, element: &str, index: usize) -> String {
    format!("/{}/{}/{}/{}/{}", PATH_PREFIX, ADD, id, element, index)
}

pub fn mk_get_url(id: &str) -> String {
    format!("/{}/{}/{}", PATH_PREFIX, GET, id)
}

// TODO. Use constants to construct URL in macros.

/**
 * Example - http://localhost:8000/lists/create/drinks
 * Use header: key: Content-Type value: application/json
 * Response: { "Ok": null }
 */
#[post("/create/<id>", format = "json")]
fn create(id: ID, state: ServiceState) -> JsonResult<()> {
    let service: &Box<dyn ListService> = state.inner();
    let result = service.create(&id);
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
#[post("/add/<id>/<element>/<index>", format = "json")]
fn add_element(id: ID, element: Element, index: usize, state: ServiceState) -> JsonResult<()> {
    let service: &Box<dyn ListService> = state.inner();
    Json(service.add_element(&id, &element, index))
}

/**
 * Example: http://localhost:8000/lists/lookup/drinks/0
 * Use header: key: Content-Type value: application/json
 * Response: { "Ok": "tea" }
 */
#[get("/lookup/<id>/<index>", format = "json")]
fn lookup(id: ID, index: usize, state: ServiceState) -> JsonResult<Element> {
    let service: &Box<dyn ListService> = state.inner();
    Json(service.get_element(&id, index))
}
