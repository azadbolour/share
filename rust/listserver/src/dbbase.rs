
extern crate dotenv;

use std::env;
// use dotenv::dotenv;

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

const DIESEL_DATABASE_URL_KEY: &str = "DATABASE_URL";

#[cfg(feature = "sqlite")]
type TheConnection = SqliteConnection;

#[cfg(feature = "sqlite")]
const DB_SYSTEM: &str = "sqlite";

#[cfg(feature = "postgres")]
type TheConnection = PgConnection;

#[cfg(feature = "postgres")]
const DB_SYSTEM: &str = "postgres";

pub type ListConnectionManager = ConnectionManager<TheConnection>;
// pub type ListConnectionManager = ConnectionManager<SqliteConnection>;
pub type ListConnection = PooledConnection<ListConnectionManager>;

pub fn mk_pool(db_url: &str) -> ListResult<Pool<ListConnectionManager>> {
    println!("making connection pool for db: {}, at : {}", DB_SYSTEM, db_url);
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

pub fn get_database_url(filename: &str) -> ListResult<String> {
    dotenv::from_filename(filename).ok();
    env::var(DIESEL_DATABASE_URL_KEY)
        .map_err(to_db_error)
}

#[cfg(test)]
mod tests {
    use super::*;

    const ENV_FILE: &str = ".env.test";

    #[test]
    fn test_get_connection() {
        let db_url = get_database_url(ENV_FILE).expect("getting database url");
        let connection_provider =
            ConnectionProvider::new(&db_url).expect("new connection provider");
        connection_provider.get_connection().expect("getting connection");
    }
}