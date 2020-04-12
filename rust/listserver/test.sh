#!/bin/sh

the_test=$1
db=$2

# export RUST_BACKTRACE=1
# cargo test --features "sqlite" $1 -- --nocapture

#
# BEWARE! Changing this feature needs forcing a rebuild!
# That does not occur by default!
#

default_db=sqlite
# default_db=postgres

if [ -z "$db" ]; then
  db="$default_db"
fi

cargo test --features "$db" "$the_test" -- --nocapture

