use crate::base::{ID, Element, BareList, ListResult, ListError, MAX_ID_LENGTH};

/**
 * Service interface for manipulating lists.
 *
 * This trait will be used by the controller layer of the application.
 * The rocket controller requires such a data structure to implement the Send the Sync traits.
 * Sync is used for thread-safety. Send is used for moving between thread boundaries.
 */
pub trait ListService: Sync + Send {
    fn create(&self, id: &ID) -> ListResult<()>;
    fn get(&self, id: &ID) -> ListResult<BareList>;
    fn update(&self, id: &ID, list: &BareList) -> ListResult<()>;
    fn delete(&self, id: &ID) -> Result<(), ListError>;

    // The rest can be implemented in terms of the list-level functions.

    fn add_element(&self, id: &ID, element: &Element, index: usize) -> ListResult<()>;
    fn get_element(&self, id: &ID, index: usize) -> ListResult<Element>;
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
