
use crate::base::{ID, Element, ListResult, BareList};
use crate::dbbase::ConnectionProvider;
use crate::rocketeer::db::listdao;
use crate::rocketeer::service::list_service::{ListService};

/**
 * Database implementation of the list service interface.
 *
 * Note. Deceptively pretends to be database-agnostic! But
 * ConnectionProvider is defined in terms of Sqlite.
 * See dbbase for details.
 */
pub struct ListServiceDb {
    connection_provider: ConnectionProvider
}

impl ListServiceDb {
    pub fn new(connection_provider: ConnectionProvider) -> ListServiceDb {
        ListServiceDb { connection_provider }
    }
}

impl ListService for ListServiceDb {
    fn create(&self, id: &ID) -> ListResult<()> {
        let conn = self.connection_provider.get_connection()?;
        listdao::create(&conn, id)
    }

    fn get(&self, id: &ID) -> ListResult<BareList> {
        let conn = self.connection_provider.get_connection()?;
         listdao::get(&conn, id)
    }

    fn update(&self, id: &ID, list: &BareList) -> ListResult<()> {
        let conn = self.connection_provider.get_connection()?;
        listdao::update(&conn, id, list.clone())
    }

    fn delete(&self, id: &ID) -> ListResult<()> {
        let conn = self.connection_provider.get_connection()?;
        listdao::delete(&conn, id)
            .map(|_| ())

    }

    fn add_element(&self, id: &ID, element: &Element, index: usize) -> ListResult<()> {
        let conn = self.connection_provider.get_connection()?;
        listdao::add_element(&conn, id, element, index)
    }

    fn get_element(&self, id: &ID, index: usize) -> ListResult<Element> {
        let conn = self.connection_provider.get_connection()?;
        listdao::get_element(&conn, id, index)
    }

    #[allow(unused)]
    fn update_element(&self, id: &ID, element: &Element, index: usize) -> ListResult<()> {
        // TODO. URGENT. Implement me. Delegate to transactional dao function.
        unimplemented!()
    }

    #[allow(unused)]
    fn remove_element(&self, id: &ID, index: usize) -> ListResult<()> {
        // TODO. URGENT. Implement me. Delegate to transactional dao function.
        unimplemented!()
    }
}
