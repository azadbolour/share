// use super::list_service::{BareList, ListService};
// use listserver::base::{Element, ListError, ID};

use crate::base::{Element, ListResult, ID};
use crate::rocketeer::service::list_service::{BareList, ListService};

struct ListServiceSqlite {
    // TODO. Use sqlite connection.
}

impl ListService for ListServiceSqlite {
    #[allow(unused)]
    fn create(&self, id: &ID) -> ListResult<()> {
        unimplemented!()
    }

    #[allow(unused)]
    fn get(&self, id: &ID) -> ListResult<BareList> {
        unimplemented!()
    }

    #[allow(unused)]
    fn update(&self, id: &ID, list: BareList) -> ListResult<()> {
        unimplemented!()
    }

    #[allow(unused)]
    fn delete(&self, id: &ID) -> ListResult<()> {
        unimplemented!()
    }

    #[allow(unused)]
    fn add_element(&self, id: &ID, element: &Element, index: usize) -> ListResult<()> {
        unimplemented!()
    }

    #[allow(unused)]
    fn get_element(&self, id: &ID, index: usize) -> ListResult<Option<Element>> {
        unimplemented!()
    }

    #[allow(unused)]
    fn update_element(&self, id: &ID, element: &Element, index: usize) -> ListResult<()> {
        unimplemented!()
    }

    #[allow(unused)]
    fn remove_element(&self, id: &ID, index: usize) -> ListResult<()> {
        unimplemented!()
    }
}
