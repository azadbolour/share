#!/bin/sh

# export RUST_BACKTRACE=1
cargo test $1 -- --nocapture
