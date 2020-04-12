use std::fmt::Debug;
use std::fmt::Display;

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
    ListJsonConversionError(String),
    ListDbError(String),
    ListLockingError(String),
    ListInternalError(String)
}

// Use this as shorthand where possible.
// TODO. How to derive Eq?
pub type ListResult<T> = Result<T, ListError>;

pub fn to_internal_error<E>(error: E) -> ListError
    where E: ToString + Display {
    ListError::ListInternalError(error.to_string())
}

pub fn to_db_error<E>(error: E) -> ListError
    where E: ToString + Display {
    ListError::ListDbError(error.to_string())
}

pub fn to_locking_error<E>(error: E) -> ListError
    where E: ToString + Display { ListError::ListLockingError(error.to_string()) }

pub fn to_json_error<E>(error: E) -> ListError
    where E: ToString + Display { ListError::ListJsonConversionError(error.to_string()) }


pub type BareList = Vec<Element>;

// Could not use unstable library function Vec.remove_item even with enabled feature.
// Vec first has the wrong type.
// Cannot use vector as both mutable and immutable!
pub fn vec_head_option<T>(vec: &Vec<T>) -> Option<T> where T: Clone {
    match vec.is_empty() {
        true => None,
        false => Some(vec.to_vec().remove(0))
    }
}

// Q: Why FnMut? What is mutated? And why mut f?
pub fn vec_map<T: Clone, U, F: FnMut(T) -> U>(vec: &Vec<T>, mut f: F) -> Vec<U> {
    vec
        .iter()
        .map(|t| f((*t).clone()))
        .collect()
}

