use std::collections::HashMap;
use std::sync::{Mutex, MutexGuard};

use crate::base::{ID, Element, ListError, ListResult, BareList};
use crate::base::{to_locking_error};
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

    fn update(&self, id: &ID, list: &BareList) -> ListResult<()> {
        let mut guarded_store= self.guarded_store()?;
        if !guarded_store.contains_key(id) {
            Err(ListError::ListIdNotFoundError(id.clone()))
        }
        else {
            let safe_list = Mutex::new(list.clone());
            guarded_store.insert(id.clone(), safe_list);
            Ok(())
        }
    }

    fn delete(&self, id: &ID) -> ListResult<()> {
        let store = &self.store;
        let mut guarded_unsafe_store = store.lock()
            .map_err(to_locking_error)?;
        guarded_unsafe_store.remove(id);
        Ok(())
    }

    fn add_element(&self, id: &ID, element: &Element, index: usize) -> ListResult<()> {
        let store = &self.store;
        let mut guarded_unsafe_store = store.lock()
            .map_err(to_locking_error)?;
        let safe_list: &SafeList = guarded_unsafe_store.get(&id.clone())
            .ok_or(ListError::ListIdNotFoundError(id.clone()))?;
        let inserted_safe_list: SafeList = self.insert_into_safe_list(safe_list, id, element, index)?;
        guarded_unsafe_store.insert(id.clone(), inserted_safe_list);
         Ok(())
    }

    fn get_element(&self, id: &ID, index: usize) -> ListResult<Element> {
        let bare_list = self.get(id)?;
        let element = bare_list.get(index)
            .ok_or(ListError::ListIndexOutOfRangeError(id.clone(), index))?;
        Ok(element.clone())
    }

    fn update_element(&self, id: &ID, element: &Element, index: usize) -> ListResult<()> {
        let mut bare_list = self.get(id)?;
        if index > bare_list.len() {
            Err(ListError::ListIndexOutOfRangeError(id.clone(), index))
        }
        else {
            bare_list.insert(index, element.clone());
            self.update(id, &bare_list)
        }
    }

    #[allow(unused)]
    fn remove_element(&self, id: &ID, index: usize) -> ListResult<()>{
        // TODO. URGENT. Implement me.
        unimplemented!()
    }
}


