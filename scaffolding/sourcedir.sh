#!/bin/bash

path=$1

mkdir -p -v "${path}"
cp -v "scaffolding/kitchen-sink.json" "${path}/kitchen-sink.json"
cp -v "scaffolding/index.cmark.tmpl" "${path}/index.cmark"
cp -v "scaffolding/glossary.cmark.tmpl" "${path}/glossary.cmark"
cp -v "scaffolding/topics.cmark.tmpl" "${path}/topics.cmark"
cp -v "scaffolding/newpage.cmark.tmpl" "${path}/first-article.cmark"

cp -v "scaffolding/js/add-dev-route.js" "${path}/add-dev-route.js"
cp -v "scaffolding/js/autoreload.js" "${path}/autoreload.js"
cp -v "scaffolding/js/echart-histogram.js" "${path}/echart-histogram.js"
cp -v "scaffolding/js/echarts.min.js" "${path}/echarts.min.js"
cp -v "scaffolding/js/search-box.js" "${path}/search-box.js"
cp -v "scaffolding/js/topicgraph.js" "${path}/topicgraph.js"

cp "./website-src/article.css" "${path}/article.css"
cp "./website-src/articles-listing.css" "${path}/articles-listing.css"
cp "./website-src/colors.css" "${path}/colors.css"
cp "./website-src/dev.css" "${path}/dev.css"
cp "./website-src/index.css" "${path}/index.css"
cp "./website-src/navigation.css" "${path}/navigation.css"
cp "./website-src/topics.css" "${path}/topics.css"
