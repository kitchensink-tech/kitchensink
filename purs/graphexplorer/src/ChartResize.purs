module ChartResize
  ( resizeChartsIn
  ) where

import Prelude (Unit)
import Effect (Effect)

-- | Asks every ECharts instance mounted on an element matching the CSS
-- selector to re-measure its DOM element (`chart.resize()`).
--
-- `Halogen.ECharts` keeps the chart reference in its own component state
-- and exposes no resize query, so the instances are looked up from the DOM
-- (`echarts.getInstanceByDom`) instead.
foreign import resizeChartsIn :: String -> Effect Unit
