
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
        self.connection_provider.get_connection()
            .and_then(|connection|
                listdao::create(&connection, id)
            )
    }

    #[allow(unused)]
    fn get(&self, id: &ID) -> ListResult<BareList> {
        self.connection_provider.get_connection()
            .and_then( |connection|
                listdao::get(&connection, id)
            )
    }

    #[allow(unused)]
    fn update(&self, id: &ID, list: BareList) -> ListResult<()> {
        unimplemented!()
    }

    #[allow(unused)]
    fn delete(&self, id: &ID) -> ListResult<()> {
        self.connection_provider.get_connection()
            .and_then(|connection|
                listdao::delete(&connection, id)
                    .map(|_| ())
            )
    }

    #[allow(unused)]
    fn add_element(&self, id: &ID, element: &Element, index: usize) -> ListResult<()> {
        self.connection_provider.get_connection()
            .and_then(|connection|
                listdao::add_element(&connection, id, element, index)
            )
    }

    #[allow(unused)]
    fn get_element(&self, id: &ID, index: usize) -> ListResult<Element> {
        // TODO. URGENT. Implement me.
        unimplemented!()
    }

    #[allow(unused)]
    fn update_element(&self, id: &ID, element: &Element, index: usize) -> ListResult<()> {
        // TODO. URGENT. Implement me.
        unimplemented!()
    }

    #[allow(unused)]
    fn remove_element(&self, id: &ID, index: usize) -> ListResult<()> {
        // TODO. URGENT. Implement me.
        unimplemented!()
    }
}
