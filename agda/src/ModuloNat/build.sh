#!/bin/sh

set -e
set -u

sources="
  UtilProperties.agda
  SigmaMod.agda
  SigmaModGroupProperties.agda
"

for f in $sources ; do
    agda -v 2 $f
done
