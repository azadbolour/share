#!/bin/sh

examples="
  BasicEvaluator.hs
  MonadicEvaluator.hs
  GracefulEvaluator.hs
  EnvironmentDependentEvaluator.hs
  ProfilingEvaluator.hs
  LoggingEvaluator.hs
  IOEvaluator.hs
"

for example in $examples; do 
  echo "running $example"
  runghc $example
  echo ""
done
