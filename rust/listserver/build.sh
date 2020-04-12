#!/bin/sh

# Build debugging version.

db=$1

#
# BEWARE! Changing the db feature needs forcing a rebuild!
# That does not occur by default!
#

default_db=sqlite
# default_db=postgres

if [ -z "$db" ]; then
  db="$default_db"
fi

cargo build --features "$db"

