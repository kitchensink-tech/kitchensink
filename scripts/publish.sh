#!/bin/bash

set +x

cd website-www/
git add .
git commit -m 'auto-publish'
git push github main

curl -XGET https://www.google.com/ping?sitemap=https://kitchensink-tech.github.io/sitemap.xml
