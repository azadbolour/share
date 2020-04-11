
use listserver::base::BareList;
use listserver::rocketeer::service::list_service::{ListService};
use listserver::rocketeer::service::list_service_in_memory;
use listserver::rocketeer::service::list_service_db::ListServiceDb;
use listserver::dbbase::ConnectionProvider;

// TODO. URGENT. Use expect rather than unwrap in tests to get information about failure.

#[test]
fn service_in_memory_test() {
    let list_service: Box<dyn ListService> = list_service_in_memory::boxed_service_factory();
    service_test(list_service)
}

const DB_URL: &str = "database/lists.db";

#[test]
fn service_sqlite_test() {
    let connection_provider = ConnectionProvider::new(DB_URL).unwrap();
    let list_service: Box<dyn ListService> = Box::new(ListServiceDb::new(connection_provider));
    service_test(list_service)
}

fn service_test(service: Box<dyn ListService>) {
    let id = "drinks".to_string();
    let tea = "tea".to_string();
    service.delete(&id).unwrap();
    service.create(&id).unwrap();
    service.add_element(&id, &tea, 0).unwrap();
    let bare_list = service.get(&id).unwrap();
    let expected: BareList = vec![tea];
    assert_eq!(bare_list, expected);

    // TODO. Add other service functions as they are implemented.
}