use listserver::base::{Element, ListError, ID, MAX_ID_LENGTH};

pub type BareList = Vec<Element>;

// pub fn lookup_bare_list(id: &String, index: usize, list: BareList) -> Result<Option<ID>, ListError> {
//     let maybeElement = list.get(index);
//     match maybeElement {
//         Some(element) => Ok(element.clone()),
//         None => Err(ListError::ListIndexOutOfRangeError(id.clone(), index))
//     }
// }

/**
 * Require the implementation to be thread-safe by using Sync.
 * Also require implementation to be transferred across thread boundaries by using Send.
 * TODO. Are these still needed? Much refactoring has occurred.
 */
pub trait ListService: Sync + Send {
    fn create(&self, id: ID) -> Result<(), ListError>;
    fn get(&self, id: &ID) -> Result<BareList, ListError>;
    fn update(&self, id: &ID, list: BareList) -> Result<(), ListError>;
    fn delete(&self, id: &ID) -> Result<(), ListError>;

    // The rest can be implemented in terms of the list-level functions.

    fn add_element(&self, id: &ID, element: Element, index: usize) -> Result<(), ListError>;
    fn get_element(&self, id: &ID, index: usize) -> Result<Option<Element>, ListError>;
    fn update_element(&self, id: &ID, element: Element, index: usize) -> Result<(), ListError>;
    fn remove_element(&self, id: &ID, index: usize);

    fn validate_id(&self, id: &ID) -> Result<(), ListError> {
        if id.is_empty() {
            Err(ListError::ListIdEmptyError)
        } else if id.len() > MAX_ID_LENGTH {
            Err(ListError::ListIdTooLongError(
                id.clone(),
                id.len(),
                MAX_ID_LENGTH,
            ))
        } else {
            Ok(())
        }
    }
}
