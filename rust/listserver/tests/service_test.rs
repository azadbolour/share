
use listserver::base::BareList;
use listserver::base::vec_map;
use listserver::rocketeer::service::list_service::{ListService};
use listserver::rocketeer::service::list_service_in_memory;
use listserver::rocketeer::service::list_service_db::ListServiceDb;
use listserver::dbbase::{ConnectionProvider, get_database_url};

// TODO. URGENT. Use expect rather than unwrap in tests to get information about failure.

#[test]
fn service_in_memory_test() {
    let list_service: Box<dyn ListService> = list_service_in_memory::boxed_service_factory();
    service_test(list_service)
}

const ENV_FILE: &str = ".env.test";

#[test]
fn service_sqlite_test() {
    let db_url = get_database_url(ENV_FILE).unwrap();
    let connection_provider = ConnectionProvider::new(&db_url).unwrap();
    let list_service: Box<dyn ListService> = Box::new(ListServiceDb::new(connection_provider));
    service_test(list_service)
}

fn service_test(service: Box<dyn ListService>) {
    let id = "drinks".to_string();
    let tea = "tea".to_string();
    service.delete(&id).expect("service delete list");
    service.create(&id).expect("service create list");
    service.add_element(&id, &tea, 0).expect("service add element");
    let bare_list = service.get(&id).expect("service get list");
    let expected: BareList = vec![tea];
    assert_eq!(bare_list, expected);

    let new_list: BareList =
        vec_map(&vec!["tea", "coffee", "wine", "bear"], |el| el.to_string());

    service.update(&id, &new_list).expect("service update");
    let updated_list = service.get(&id).expect("service get list");
    assert_eq!(updated_list, new_list);

    // TODO. Add other service functions as they are implemented.
}