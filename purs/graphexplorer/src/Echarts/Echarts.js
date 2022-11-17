"use strict";

import * as echarts from 'echarts';

export const init = function(dom) {
  return () => echarts.init(dom);
}

export const initById = function(id) {
  return () => echarts.init(document.getElementById(id));
}

export const showLoading = function(chart) {
  return () => chart.showLoading();
}

export const hideLoading = function(chart) {
  return () => chart.hideLoading();
}

export const setOptions = function(chart) {
  return function(opts) {
    return function () {
      chart.setOption(opts);
    };
  };
}

export const on = function(chart) {
  return function(ev) {
    return function (f) {
      const go = (x) => {
        f(x)();
      };
      return function () {
        chart.on(ev, go);
      };
    };
  };
}

export const onQuery = function(chart) {
  return function(ev) {
    return function (q) {
      return function (f) {
        const go = (x) => {
          f(x)();
        };
        return function () {
          chart.on(ev, q, go);
        };
      };
    };
  };
}
