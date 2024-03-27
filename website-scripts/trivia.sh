#!/bin/bash

echo "git commits:"
git log --oneline | wc -l

echo "src files:"
find ./hs/src -name "*.hs" | wc -l

echo "code size (lines):"
find ./hs/src -name "*.hs" | xargs cat | wc -l

echo "code size (bytes):"
find ./hs/src -name "*.hs" | xargs cat | wc -c

echo "details:"
find ./hs/src -name "*.hs" | xargs wc
