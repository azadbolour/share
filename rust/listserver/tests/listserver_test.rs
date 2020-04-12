use listserver;

use listserver::rocketeer::rocketeer::{ignite, mk_create_url, mk_add_element_url, mk_get_url};

use rocket::local::Client;
use rocket::http::ContentType;

use serde_json::from_str;
use listserver::rocketeer::service::list_service::{ListService};

#[allow(unused_imports)]
use listserver::base::{ID, Element, ListResult, BareList};
use listserver::rocketeer::service::list_service_in_memory;
use listserver::rocketeer::service::list_service_db::ListServiceDb;
use listserver::dbbase::{ConnectionProvider, get_database_url};

const ENV_FILE: &str = ".env.test";

#[test]
fn listserver_in_memory_test() {
    let list_service: Box<dyn ListService> = list_service_in_memory::boxed_service_factory();
    listserver_test(list_service)
}

#[test]
fn listserver_sqlite_test() {
    let db_url = get_database_url(ENV_FILE).unwrap();
    let connection_provider = ConnectionProvider::new(&db_url).unwrap();
    let list_service: Box<dyn ListService> = Box::new(ListServiceDb::new(connection_provider));
    listserver_test(list_service)
}

const DRINKS: &str = "drinks";

// TODO. URGENT. Use expect rather than unwrap in tests to get information about failure.
fn listserver_test(service: Box<dyn ListService>) {
    // TODO. Taking liberties here. Delete should go through the client.
    service.delete(&DRINKS.to_string()).unwrap();
    let rocket = ignite(service);
    let client = Client::new(rocket).expect("valid rocket");
    post_create(&client);
    post_add_element(&client, DRINKS, "tea", 0);
    post_get(&client, DRINKS, Ok(vec!["tea".to_string()]));

    // TODO. Add other service functions as they are implemented.
}

fn post_add_element(client: &Client, id: &str, element: &str, index: usize) {
    let url = mk_add_element_url(id, element, index);
    let mut response = client
        .post(url)
        .header(ContentType::JSON)
        .dispatch();
    let body = response.body_string().unwrap();
    println!("{}", body);
    let result: ListResult<()> = from_str(&body).unwrap();
    assert_eq!(result.unwrap(), ());
}

fn post_create(client: &Client) {
    let url = mk_create_url("drinks");
    let mut response = client
        .post(url)
        .header(ContentType::JSON)
        .dispatch();
    let body = response.body_string().unwrap();
    println!("{}", body);
    let result: ListResult<()> = from_str(&body).unwrap();
    assert_eq!(result.unwrap(), ());
}

fn post_get(client: &Client, id: &str, expected: ListResult<BareList>) {
    let url = mk_get_url(&id);
    let mut response = client
        .get(url)
        .header(ContentType::JSON)
        .dispatch();
    let body = response.body_string().unwrap();
    println!("{}", body);
    let result: ListResult<BareList> = from_str(&body).unwrap();
    let equals = result == expected;
    assert!(equals);
}