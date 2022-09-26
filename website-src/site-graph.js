(function(){
  const go = () => {
    var chartDom = document.getElementById('echartzone');
    var myChart = echarts.init(chartDom);
    var option;
    
    const categoryIndices = ["ArticleNode", "TopicNode", "ImageNode"];
    
    const indexArticleRef = "article:/index.html";
    
    const urlForNode = (n) => {
    	switch (n[1].tag) {
              case "ArticleNode": {
    	    return n[1].contents[0];
              }
              case "ImageNode": {
    	    return n[1].contents;
    	  }
              case "TopicNode": {
    	    const topic = n[0].split(':',2)[1];
    	    const urlTopic = topic.replace(/\s/g, '-');
                return `/topics/${urlTopic}.html`
    	  }
    	}
    }
    
    const sizeForNode = (n, selection, edges) => {
      let neighborSelected = false;
      if (selection.node !== null) {
        if (edges.findIndex((e) => { return (e[0] === n[0] || e[1] === n[0]) && (e[0] === selection.node[0] || e[1] === selection.node[0]) }) > 0) {
          neighborSelected = true;
        }
      }
    
      if (neighborSelected) {
          if (selection.node === n) {
            return 35;
          }
          return 30;
      }
    
      switch (n[1]?.tag) {
        case "ArticleNode":
          return 10 + Math.log(n[1].contents[1]) / Math.log(2);
        case "TopicNode":
          return 10;
        default:
          return 5;
      }
    };
    
    const symbolForNode = (n, selection, edges) => {
      let neighborSelected = false;
      if (selection.node !== null) {
        if (edges.findIndex((e) => { return (e[0] === n[0] || e[1] === n[0]) && (e[0] === selection.node[0] || e[1] === selection.node[0]) }) > 0) {
          neighborSelected = true;
        }
      }
    
      switch (n[1]?.tag) {
        case "ArticleNode":
          return 'rect';
        case "TopicNode":
          return 'diamond';
        case "ImageNode":
          if (neighborSelected) {
            return `image://${urlForNode(n)}`;
          }
          return 'circle';
        default:
          return 'none';
      }
    };
    
    myChart.showLoading();
    const url = new URL(document.location.origin + '/json/topicsgraph.json');
    fetch(url)
    .then((response) => response.json())
    .then((topicsgraph) => {
      const nodes = topicsgraph.nodes
            .filter((n) => { return n[0] !== indexArticleRef });
      const edges = topicsgraph.edges;
      myChart.hideLoading();
      let selection = {node: null, idx: -1};
    
      const run = () => {
        const graph = {
          categories: [{"name":"articles"},{"name":"topics"},{"name":"images"}],
          links: edges
            .filter((e) => { return e[0] !== indexArticleRef && e[1] !== indexArticleRef })
            .map((e) => { return {source:e[0], target:e[1]} }),
          nodes: nodes
            .map((n) => { return {
              id: n[0]
            , name: n[0]
            , symbol: symbolForNode(n, selection, edges)
            , symbolSize: sizeForNode(n, selection, edges)
            , category: categoryIndices.findIndex((t) => t == n[1].tag)
            } 
          }),
        }
      
        option = {
          title: {
            top: 'bottom',
            left: 'right'
          },
          tooltip: {},
          legend: [
            {
              data: graph.categories.map(function (a) {
                return a.name;
              })
            }
          ],
          series: [
            {
              name: 'blog',
              type: 'graph',
              layout: 'force',
              data: graph.nodes,
              links: graph.links,
              categories: graph.categories,
              roam: true,
              draggable: true,
              label: {
                position: 'right'
              },
              force: {
                repulsion: 100
              }
            }
          ]
        };
        myChart.setOption(option);
      }
      run();
    
      myChart.on('click', function(params) {
        if (params.componentType === 'series') {
          if (params.dataType === 'node') {
            const idx = params.dataIndex;
    	const n = nodes[idx];
            if (selection.node === n) {
              const url = urlForNode(n);
              window.open(url, '_blank');
            } else {
              selection = {node: n, idx: idx};
              console.log(selection);
              run();
            }
          }
        }
      });
    })
    .catch((error) => {
      console.error('Error:', error);
    });
    
    option && myChart.setOption(option);
  };
  document.addEventListener("DOMContentLoaded", go);
})();
