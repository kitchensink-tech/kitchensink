#!/bin/bash

set +x

cd website-www/
git add .
git commit -m 'auto-publish'
git push github main
