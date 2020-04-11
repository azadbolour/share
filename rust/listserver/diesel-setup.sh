#!/bin/sh

dbdir=database
mkdir -p $dbdir
diesel setup --database-url="${dbdir}/lists.db"

echo "default setup done"
echo "update the generated diesel.toml file to use your specific schema.rs file"

#
# creates the database if not there - directory must exist
# creates a migrarion table in the database
# creates empty migrations folder
# creates diesel.toml file 
# but we need the schema to go to the db directory 
# so have to change the schema file name in diesel.toml
#

