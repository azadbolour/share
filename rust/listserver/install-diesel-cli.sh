#!/bin/sh

#
# I don't use mysql and don't have mysqlclient on my machines. 
# So set up the diesel client for just the databases we need.
# Note - the list of features is quoted.
#
cargo install diesel_cli --no-default-features --features "postgres sqlite"

# WARNING! --force removes existing installation.
# cargo install diesel_cli --force --no-default-features --features "postgres sqlite"

# This will get all the features.
# cargo install diesel_cli 

#
# https://crates.io/crates/diesel_cli
#
