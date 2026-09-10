#!/bin/bash

set -x 

cp -v ./website-src/article.css ./scaffolding/css
cp -v ./website-src/articles-listing.css ./scaffolding/css
cp -v ./website-src/colors.css ./scaffolding/css
cp -v ./website-src/dev.css ./scaffolding/css
cp -v ./website-src/index.css ./scaffolding/css
cp -v ./website-src/navigation.css ./scaffolding/css
cp -v ./website-src/tags.css ./scaffolding/css

cp -v ./website-src/add-dev-route.js ./scaffolding/js
cp -v ./website-src/autoreload.js ./scaffolding/js
cp -v ./website-src/echart-histogram.js ./scaffolding/js
cp -v ./website-src/echarts.min.js ./scaffolding/js
cp -v ./website-src/search-box.js ./scaffolding/js
cp -v ./website-src/topicgraph.js ./scaffolding/js

# hs/scaffolding is a copy of scaffolding/ (minus the *.sh scripts) kept
# inside the cabal package so `kitchen-sink init` can embed it at
# compile-time (file-embed requires embedded paths to live inside the
# package directory).
cp -v ./scaffolding/kitchen-sink.json ./hs/scaffolding
cp -v ./scaffolding/README.md ./hs/scaffolding
cp -v ./scaffolding/*.tmpl ./hs/scaffolding
cp -v ./scaffolding/js/*.js ./hs/scaffolding/js
cp -v ./scaffolding/css/*.css ./hs/scaffolding/css
