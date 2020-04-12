#!/bin/sh

executable=$1
db=$2

#
# BEWARE! Changing this feature needs forcing a rebuild!
# That does not occur by default!
#

default_db=sqlite
# default_db=postgres

if [ -z "$db" ]; then
  db="$default_db"
fi

cargo run --features "$db" --bin $executable




