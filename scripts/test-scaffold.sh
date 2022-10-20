#!/bin/bash

set -x

rm -v -f "./scaffold-test-src"
rm -v -f "./scaffold-test-out"

mkdir "./scaffold-test-src"
mkdir "./scaffold-test-out"

bash ./scaffolding/sourcedir.sh "./scaffold-test-src"
bash ./scaffolding/outputdir.sh "./scaffold-test-out"

kitchen-sink serve \
  --servMode "DEV" \
  --srcDir "./scaffold-test-src" \
  --outDir "./scaffold-test-out" \
  --port 8765
