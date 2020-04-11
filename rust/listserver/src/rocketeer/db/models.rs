
use super::schema::lists;

/*
 * Structures paralleling the lists table used by diesel
 * for querying and insertion of lists.
 */

/**
 * Model struct for querying the lists table.
 *
 * Cannot specify table name for this struct.
 * Diesel assumes the table name is the plural of the queryable model name,
*/
#[derive(Queryable, Serialize, Deserialize, Clone)]
pub struct List {
    pub name: String,
    pub json: String,
}

/**
 * Model struct for inserting lists records.
 */
#[derive(Insertable, Serialize, Deserialize, Clone)]
#[table_name="lists"]
pub struct ListWriter {
    pub name: String,
    pub json: String,
}
