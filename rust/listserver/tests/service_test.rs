
use listserver::base::{Element, ListResult, ListError, ID};
use listserver::rocketeer::service::list_service::{BareList, ListService};
use listserver::rocketeer::service::list_service_in_memory;

#[test]
fn service_in_memory_test() {
    let list_service: Box<dyn ListService> = list_service_in_memory::boxed_service_factory();
    service_test(list_service)
}

fn service_test(service: Box<dyn ListService>) {
    let id = "drinks".to_string();
    let tea = "tea".to_string();
    service.create(&id);
    service.add_element(&id, &tea, 0);
    let result = service.get(&id);
    let expected: ListResult<BareList> = Ok(vec![tea]);
    assert_eq!(result, expected);

    // TODO. Add other service functions as they are implemented.
}