#!/bin/sh

diesel migration run

#
# This may work to revert and redo - not always!
#
# diesel migration redo 

#
# You can always just delete the data and start again in development.
#
# rm database/lists.db

