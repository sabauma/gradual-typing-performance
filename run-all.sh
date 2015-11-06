#!/bin/bash

export OUTPUT="benchmarks/all-results.png"

echo "### Preparing to run ALL benchmarks. This may take over 6 months."
read -p "Are you sure you want to continue? " -n 1 -r
echo ""
if [[ $REPLY =~ ^[Yy]$ ]]
then
    for project in `echo benchmarks/*/`
    do
      sh run.sh $project
    done
    racket tools/view.rkt -o $OUTPUT benchmarks/*.rktd
    echo "### Saved results to '"$OUTPUT"'"
fi