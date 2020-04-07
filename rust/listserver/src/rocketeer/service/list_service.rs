use crate::base::{Element, ListResult, ListError, ID, MAX_ID_LENGTH};

pub type BareList = Vec<Element>;

/**
 * Require the implementation to be thread-safe by using Sync.
 * Also require implementation to be transferred across thread boundaries by using Send.
 * TODO. Are these still needed? Much refactoring has occurred.
 */
pub trait ListService: Sync + Send {
    fn create(&self, id: &ID) -> ListResult<()>;
    fn get(&self, id: &ID) -> ListResult<BareList>;
    fn update(&self, id: &ID, list: BareList) -> ListResult<()>;
    fn delete(&self, id: &ID) -> Result<(), ListError>;

    // The rest can be implemented in terms of the list-level functions.

    fn add_element(&self, id: &ID, element: &Element, index: usize) -> ListResult<()>;
    fn get_element(&self, id: &ID, index: usize) -> ListResult<Option<Element>>;
    fn update_element(&self, id: &ID, element: &Element, index: usize) -> ListResult<()>;
    fn remove_element(&self, id: &ID, index: usize) -> ListResult<()>;

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
