use listserver;

use listserver::rocketeer::rocketeer::{ignite, mk_create_url, mk_add_element_url, mk_get_url};

use rocket::local::Client;
use rocket::http::ContentType;

use serde_json::from_str;
use listserver::rocketeer::service::list_service::{BareList, ListService};

#[allow(unused_imports)]
use listserver::base::{ID, Element, ListResult};
use listserver::rocketeer::service::list_service_in_memory;

#[test]
fn listserver_test() {
    let list_service: Box<dyn ListService> = list_service_in_memory::boxed_service_factory();
    let rocket = ignite(list_service);
    let client = Client::new(rocket).expect("valid rocket");
    post_create(&client);
    post_add_element(&client, "drinks", "tea", 0);
    post_get(&client, "drinks", Ok(vec!["tea".to_string()]));

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
    let url = mk_get_url("drinks");
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