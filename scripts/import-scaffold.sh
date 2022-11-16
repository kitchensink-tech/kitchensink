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
