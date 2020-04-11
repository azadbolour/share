#!/bin/sh

migrationsequencename=$1
diesel migration generate $migrationsequencename

