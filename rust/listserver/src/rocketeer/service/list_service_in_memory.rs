use std::collections::HashMap;
use std::sync::{Mutex, MutexGuard};

use crate::base::{ID, Element, ListError, ListResult, BareList};
use crate::base::{to_locking_error, vec_head_option};
use crate::rocketeer::service::list_service::{ListService};

type SafeList = Mutex<BareList>;
type UnsafeListStore = HashMap<ID, SafeList>;

/**
 * In-memory implementation of the service interface.
 */
pub struct ListServiceInMemory {
    pub store: Mutex<UnsafeListStore>,
}

pub fn boxed_service_factory() -> Box<dyn ListService> {
    Box::new(ListServiceInMemory {
        store: Mutex::new(HashMap::new()),
    })
}

impl ListServiceInMemory {
    fn guarded_store(&self) -> ListResult<MutexGuard<UnsafeListStore>> {
        let store = &self.store;
        store.lock().map_err(to_locking_error)
    }

    fn create_internal(&self, id: &String) -> ListResult<()> {
        let mut guarded_store = self.guarded_store()?;
        match guarded_store.get(&id.clone()) {
            Some(_) => Err(ListError::DuplicateListIdError(id.clone())),
            None => self.do_insert(id, &mut guarded_store),
        }
    }

    fn do_insert(&self, id: &ID, lock: &mut MutexGuard<UnsafeListStore>) -> ListResult<()> {
        let safe_list = self.new_empty_safe_list();
        // Insert for HashMap means insert_or_update (returning old value).
        match lock.insert(id.clone(), safe_list) {
            None => Ok(()),
            Some(_) => Err(ListError::ListIdNotFoundError(id.clone()))
        }
    }

    fn new_empty_safe_list(&self) -> Mutex<BareList> {
        let unsafe_list: Vec<Element> = Vec::new();
        let safe_list = Mutex::new(unsafe_list);
        safe_list
    }

    fn get_internal(&self, id: &ID) -> ListResult<BareList> {
        let guarded_store = self.guarded_store()?;
        let safe_list = guarded_store.get(&id.clone())
            .ok_or(ListError::ListIdNotFoundError(id.clone()))?;

        let guarded_list = safe_list.lock()
            .map_err(to_locking_error)?;
        let bare_list: BareList = guarded_list.to_vec();
        Ok(bare_list)
    }

    fn insert_into_safe_list(&self, safe_list: &SafeList, id: &ID, element: &Element, index: usize)
                             -> ListResult<SafeList> {
        let guarded_safe_list = safe_list.lock()
            .map_err(to_locking_error)?;
        let mut bare_list: BareList = guarded_safe_list.to_vec();
        if bare_list.len() < index {
            Err(ListError::ListIndexOutOfRangeError(id.clone(), index))
        } else {
            bare_list.insert(index, element.clone());
            Ok(Mutex::new(bare_list))
        }
    }
}

impl ListService for ListServiceInMemory {
    fn create(&self, id: &ID) -> ListResult<()> {
        self.validate_id(&id).and(self.create_internal(&id))
    }

    fn get(&self, id: &ID) -> ListResult<BareList> {
        self.validate_id(&id).and(self.get_internal(&id))
    }

    #[allow(unused)]
    fn update(&self, id: &ID, list: BareList) -> ListResult<()> {
        // TODO. URGENT. Implement me.
        unimplemented!()
    }

    #[allow(unused)]
    fn delete(&self, id: &ID) -> ListResult<()> {
        let store = &self.store;
        let mut lock = store.lock()
            .map_err(to_locking_error)?;
        lock.remove(id);
        Ok(())
    }

    fn add_element(&self, id: &ID, element: &Element, index: usize) -> ListResult<()> {
        let store = &self.store;
        let mut lock = store.lock()
            .map_err(to_locking_error)?;
        let safe_list: &SafeList = lock.get(&id.clone())
            .ok_or(ListError::ListIdNotFoundError(id.clone()))?;
        let inserted_safe_list: SafeList = self.insert_into_safe_list(safe_list, id, element, index)?;
        lock.insert(id.clone(), inserted_safe_list);
         Ok(())
    }

    fn get_element(&self, id: &ID, index: usize) -> ListResult<Element> {
        let bare_list = self.get(id)?;
        vec_head_option(&bare_list)
            .ok_or(ListError::ListIndexOutOfRangeError(id.clone(), index))
    }

    #[allow(unused)]
    fn update_element(&self, id: &ID, element: &Element, index: usize) -> ListResult<()> {
        // TODO. URGENT. Implement me.
        unimplemented!()
    }

    #[allow(unused)]
    fn remove_element(&self, id: &ID, index: usize) -> ListResult<()>{
        // TODO. URGENT. Implement me.
        unimplemented!()
    }
}


