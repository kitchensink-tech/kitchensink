#!/bin/bash

set +x

cd purs/graphexplorer
npm i
spago bundle-app -y -t ../../website-src/topicgraph.js
cd -

cd purs/search-box
npm i
spago bundle-app -y -t ../../website-src/search-box.js
cd -
