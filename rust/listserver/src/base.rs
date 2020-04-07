use std::fmt::Debug;

pub type ID = String;
pub type Element = String;

pub const MAX_ID_LENGTH: usize = 20; // TODO. Increase max length.

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq)]
pub enum ListError {
    ListIdEmptyError,
    ListIdTooLongError(ID, usize, usize),
    DuplicateListIdError(ID),
    ListIdNotFoundError(ID),
    ListIndexOutOfRangeError(ID, usize),
    ListInternalError(String)
}

// Use this as shorthand where possible.
// TODO. How to derive Eq?
pub type ListResult<T> = Result<T, ListError>;
