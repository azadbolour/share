use std::collections::HashMap;
use std::sync::{Mutex, MutexGuard};

use listserver::base::{Element, ListError, ListResult, ID, MAX_ID_LENGTH};

use crate::{BareList, ListService};

type SafeList = Mutex<BareList>;
type UnsafeListStore = HashMap<ID, SafeList>;

pub struct ListServiceInMemory {
    pub store: Mutex<UnsafeListStore>,
}

impl ListService for ListServiceInMemory {
    fn create(&self, id: ID) -> Result<(), ListError> {
        self.validate_id(&id).and(self.create_internal(&id))
    }

    fn get(&self, id: &ID) -> Result<BareList, ListError> {
        self.validate_id(&id).and(self.get_internal(&id))
    }

    fn update(&self, id: &ID, list: BareList) -> Result<(), ListError> {
        unimplemented!()
    }

    fn delete(&self, id: &ID) -> Result<(), ListError> {
        unimplemented!()
    }

    fn add_element(&self, id: &ID, element: Element, index: usize) -> Result<(), ListError> {
        let store = &self.store;
        let mut lock = store.lock().expect("lock list store");
        match lock.get(&id.clone()) {
            Some(safe_list) => self
                .insert_into_safe_list(safe_list, id, element, index)
                .map(|list| {
                    lock.insert(id.clone(), list);
                    ()
                }),
            None => Err(ListError::ListIdNotFoundError(id.clone())),
        }
    }

    fn get_element(&self, id: &ID, index: usize) -> Result<Option<Element>, ListError> {
        self.get(id)
            .and_then(|bare_list| Ok(bare_list.get(index).map(|el| el.clone())))
    }

    fn update_element(&self, id: &ID, element: Element, index: usize) -> Result<(), ListError> {
        unimplemented!()
    }

    fn remove_element(&self, id: &ID, index: usize) {
        unimplemented!()
    }
}

impl ListServiceInMemory {
    fn create_internal(&self, id: &String) -> Result<(), ListError> {
        let store = &self.store;
        let mut lock = store.lock().expect("lock list store");
        match lock.get(&id.clone()) {
            Some(_) => Err(ListError::DuplicateListIdError(id.clone())),
            None => self.do_insert(id, &mut lock),
        }
    }

    fn do_insert(
        &self,
        id: &String,
        lock: &mut MutexGuard<UnsafeListStore>,
    ) -> Result<(), ListError> {
        let safe_list = self.new_empty_safe_list();
        // Insert for HashMap means insert_or_update (returning old value).
        match lock.insert(id.clone(), safe_list) {
            None => Ok(()),
            Some(_) => Err(ListError::ListInternalError(
                "unable to insert list into list store".to_string(),
            )),
        }
    }

    fn new_empty_safe_list(&self) -> Mutex<BareList> {
        let unsafe_list: Vec<Element> = Vec::new();
        let safe_list = Mutex::new(unsafe_list);
        safe_list
    }

    fn get_internal(&self, id: &ID) -> ListResult<BareList> {
        let store = &self.store;
        let mut lock = store.lock().expect("lock list store");
        match lock.get(&id.clone()) {
            Some(safeList) => {
                let listLock = safeList.lock().expect("lock list");
                let bareList: BareList = listLock.to_vec();
                Ok(bareList)
            }
            None => Err(ListError::DuplicateListIdError(id.clone())),
        }
    }

    fn insert_into_safe_list(
        &self,
        save_list: &Mutex<Vec<String>>,
        id: &String,
        element: String,
        index: usize,
    ) -> Result<SafeList, ListError> {
        let list_lock = save_list.lock().expect("lock list");
        let mut bare_list: BareList = list_lock.to_vec();
        if bare_list.len() < index {
            Err(ListError::ListIndexOutOfRangeError(id.clone(), index))
        } else {
            bare_list.insert(index, element);
            Ok(Mutex::new(bare_list))
        }
    }
}
