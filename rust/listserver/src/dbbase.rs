
extern crate dotenv;

use std::env;
use dotenv::dotenv;

use diesel::prelude::*;
use diesel::r2d2::{ConnectionManager, Pool, PooledConnection};

use crate::base::{ListResult, to_db_error};

/**
 * To repurpose to Postgres, one *should* be able to change the
 * SqliteConnection type parameter to PgConnection in
 * ListConnectionManager. But the code has not been tested yet
 * with Postgres.
 *
 * Of course, repurposing requires recompilation!
 *
 * I ran out of time trying to make the code generic in the type
 * of database to avoid recompilation.
 */
pub type ListConnectionManager = ConnectionManager<SqliteConnection>;
pub type ListConnection = PooledConnection<ListConnectionManager>;

pub fn mk_pool(db_url: &str) -> ListResult<Pool<ListConnectionManager>> {
    Pool::builder()
        .build(ListConnectionManager::new(db_url))
        .map_err(|err| to_db_error(err))
}

pub struct ConnectionProvider {
    pool: Pool<ListConnectionManager>
}

impl ConnectionProvider {
    pub fn new(db_url: &str) -> ListResult<ConnectionProvider> {
        mk_pool(db_url).map(|pool|
            ConnectionProvider {pool}
        )
    }

    pub fn get_connection(&self) -> ListResult<ListConnection> {
        self.pool.get().map_err(|err| to_db_error(err))
    }
}

// Not used any more.
// May want to get the db url from .env file for the pool as well as shown here.
pub fn establish_connection() -> ListResult<SqliteConnection> {
    dotenv().ok(); // Load .env file.
    env::var("DATABASE_URL")
        .map_err(to_db_error)
        .and_then( |url| SqliteConnection::establish(&url)
            .map_err(to_db_error)
        )
}