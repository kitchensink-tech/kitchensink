"use strict";

import * as echarts from 'echarts';

export const resizeChartsIn = function(selector) {
  return () => {
    document.querySelectorAll(selector).forEach((dom) => {
      const chart = echarts.getInstanceByDom(dom);
      if (chart) {
        chart.resize();
      }
    });
  };
}
