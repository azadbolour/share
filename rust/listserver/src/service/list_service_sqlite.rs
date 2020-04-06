use super::list_service::{BareList, ListService};
use listserver::base::{Element, ListError, ID};

struct ListServiceSqlite {
    // TODO. Use sqlite connection.
}

impl ListService for ListServiceSqlite {
    fn create(&self, id: ID) -> Result<(), ListError> {
        unimplemented!()
    }

    fn get(&self, id: &ID) -> Result<BareList, ListError> {
        unimplemented!()
    }

    fn update(&self, id: &ID, list: BareList) -> Result<(), ListError> {
        unimplemented!()
    }

    fn delete(&self, id: &ID) -> Result<(), ListError> {
        unimplemented!()
    }

    fn add_element(&self, id: &ID, element: Element, index: usize) -> Result<(), ListError> {
        unimplemented!()
    }

    fn get_element(&self, id: &ID, index: usize) -> Result<Option<Element>, ListError> {
        unimplemented!()
    }

    fn update_element(&self, id: &ID, element: Element, index: usize) -> Result<(), ListError> {
        unimplemented!()
    }

    fn remove_element(&self, id: &ID, index: usize) {
        unimplemented!()
    }
}
