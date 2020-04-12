

use diesel::prelude::*;

use crate::jsonlist;
use crate::base::{ID, ListError, ListResult, BareList};
use crate::base::{vec_head_option, to_internal_error, to_db_error, to_json_error};
use crate::dbbase::ListConnection;
use crate::rocketeer::db::schema;
use crate::rocketeer::db::models::{ListWriter, List};

/*
 * The data access implementation for lists.
 *
 * Lists are saved in the database as json strings.
 *
 * TODO. These methods should belong to a struct.
 * But the struct would need a &ListConnection field.
 * Borrowed struct fields need lifetimes. So left for later as an advanced exercise.
 */

pub fn create(connection: &ListConnection, name: &str) -> ListResult<()> {
    let list: Vec<String> = vec![];
    let json_list = serde_json::to_string(&list)
        .map_err(to_internal_error)?;
    insert_list(connection,name.to_string(), json_list)?;
    Ok(())
}

fn insert_list(connection: &ListConnection, name: String, json: String) -> ListResult<usize> {
    use schema::lists;
    diesel::insert_into(lists::table)
        .values(&ListWriter { name: name, json: json })
        .execute(connection)
        .map_err(to_db_error)
}

pub fn delete(connection: &ListConnection, id: &str) -> ListResult<usize> {
    use crate::rocketeer::db::schema::lists::dsl::*;
    diesel::delete(lists.filter(name.like(id)))
        .execute(connection)
        .map_err(to_db_error)
}

pub fn get(connection: &ListConnection, id: &str) -> ListResult<BareList> {
    let list: List = get_internal(connection, id)?;
    serde_json::from_str(&list.json)
        .map_err(|err| to_json_error(err))
}

pub fn get_internal(connection: &ListConnection, id: &str) -> ListResult<List> {
    use crate::rocketeer::db::schema::lists::dsl::*;
    let vec_result: ListResult<Vec<List>> = lists
        .filter(name.eq(&id))
        .limit(1)
        .load::<List>(connection)
        .map_err(to_db_error);

    vec_result.and_then(|vec_list| {
        // Type annotations needed to avoid compiler confusion.
        let option: Option<List> = vec_head_option(&vec_list);
        let list_result: ListResult<List> =
            option.ok_or(ListError::ListIdNotFoundError(id.to_string()));
        list_result
    })
}

pub fn update(connection: &ListConnection, id: &str, list: BareList) -> ListResult<()> {
    let jsonlist = serde_json::to_string(&list)
        .map_err(to_internal_error)?;
    update_internal(connection, id, &jsonlist)
}

fn update_internal(connection: &ListConnection, id: &str, jsonlist: &str) -> ListResult<()> {
    use crate::rocketeer::db::schema::lists::dsl::*;
    let target = lists.filter(name.eq(id));
    diesel::update(target)
        .set(json.eq(jsonlist))
        .execute(connection)
        .map(|_| ())
        .map_err(to_db_error)
}

// TODO. URGENT. Use transaction.
pub fn add_element(connection: &ListConnection, id: &str, element: &str, index: usize) -> ListResult<()> {
    let list: List = get_internal(connection, id)?;
    let json_added_list: String = jsonlist::add_element(&list.json, element, index)?;
    update_internal(connection,id, &json_added_list)
}

// TODO. Add transactional update_element, delete_element.

pub fn get_element(connection: &ListConnection, id: &str, index: usize) -> ListResult<ID> {
    let list: List = get_internal(connection, id)?;
    jsonlist::get_element(&list.json, index)
}



