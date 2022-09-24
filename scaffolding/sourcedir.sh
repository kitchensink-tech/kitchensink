#!/bin/bash

path=$1

mkdir -p -v "${path}"
cp -v "scaffolding/kitchen-sink.json" "${path}/kitchen-sink.json"
cp -v "scaffolding/index.cmark.tmpl" "${path}/index.cmark"
cp -v "scaffolding/tags.cmark.tmpl" "${path}/tags.cmark"
cp -v "scaffolding/newpage.cmark.tmpl" "${path}/first-article.cmark"
cp -v "scaffolding/js/autoreload.js" "${path}/autoreload.js"
cp -v "scaffolding/js/add-dev-route.js" "${path}/add-dev-route.js"
cp -v "scaffolding/js/echart-histogram.js" "${path}/echart-histogram.js"
cp -v "scaffolding/js/echarts.min.js" "${path}/echarts.min.js"
cp -v "scaffolding/js/search-box.js" "${path}/search-box.js"
cp -v "scaffolding/js/site-graph.js" "${path}/site-graph.js"
