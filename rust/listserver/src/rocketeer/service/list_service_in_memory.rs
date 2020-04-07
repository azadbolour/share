use std::collections::HashMap;
use std::sync::{Mutex, MutexGuard};

use crate::base::{Element, ListError, ListResult, ID};
use crate::rocketeer::service::list_service::{BareList, ListService};

// use crate::{BareList, ListService};

type SafeList = Mutex<BareList>;
type UnsafeListStore = HashMap<ID, SafeList>;

pub struct ListServiceInMemory {
    pub store: Mutex<UnsafeListStore>,
}

pub fn boxed_service_factory() -> Box<dyn ListService> {
    Box::new(ListServiceInMemory {
        store: Mutex::new(HashMap::new()),
    })
}

impl ListService for ListServiceInMemory {
    fn create(&self, id: &ID) -> Result<(), ListError> {
        self.validate_id(&id).and(self.create_internal(&id))
    }

    fn get(&self, id: &ID) -> Result<BareList, ListError> {
        self.validate_id(&id).and(self.get_internal(&id))
    }

    #[allow(unused)]
    fn update(&self, id: &ID, list: BareList) -> Result<(), ListError> {
        unimplemented!()
    }

    #[allow(unused)]
    fn delete(&self, id: &ID) -> Result<(), ListError> {
        unimplemented!()
    }

    fn add_element(&self, id: &ID, element: &Element, index: usize) -> Result<(), ListError> {
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

    #[allow(unused)]
    fn update_element(&self, id: &ID, element: &Element, index: usize) -> Result<(), ListError> {
        unimplemented!()
    }

    #[allow(unused)]
    fn remove_element(&self, id: &ID, index: usize) -> ListResult<()>{
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

    fn do_insert(&self, id: &ID, lock: &mut MutexGuard<UnsafeListStore>) -> Result<(), ListError> {
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
        let lock = store.lock().expect("lock list store");
        match lock.get(&id.clone()) {
            Some(safe_list) => {
                let list_lock = safe_list.lock().expect("lock list");
                let bare_list: BareList = list_lock.to_vec();
                Ok(bare_list)
            }
            None => Err(ListError::DuplicateListIdError(id.clone())),
        }
    }

    fn insert_into_safe_list(&self, save_list: &Mutex<Vec<String>>, id: &ID, element: &Element, index: usize)
      -> Result<SafeList, ListError> {
        let list_lock = save_list.lock().expect("lock list");
        let mut bare_list: BareList = list_lock.to_vec();
        if bare_list.len() < index {
            Err(ListError::ListIndexOutOfRangeError(id.clone(), index))
        } else {
            bare_list.insert(index, element.clone());
            Ok(Mutex::new(bare_list))
        }
    }
}
