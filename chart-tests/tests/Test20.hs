module Test20 where

import Graphics.Rendering.Chart
import Data.Colour
import Data.Colour.Names
import Control.Lens
import Data.Default.Class

import Utils
import Test.QuickCheck (Result(Failure))

dat :: [[Double]]
dat = [[0.921112,1.123134 ]
      ,[25.112  ,35.5643  ]
      ,[100.123 ,150.123  ]
      ,[1.333   ,1.0132   ]
      ,[2.345342,2.784568 ]
      ,[110.1233,101.363  ]
      ,[15.2213 ,12.856783]
      ,[7.223   ,8.9832   ]]

chart :: Renderable (LayoutPick PlotIndex LogValue LogValue)
chart = layoutToRenderable layout
 where
  layout =
      -- title
        layout_title .~ "Sample Log Bars"
      $ layout_title_style . font_size .~ 10

      -- X
      $ layout_x_axis . laxis_generate .~ autoIndexAxis' True alabels
      $ layout_x_axis . laxis_override .~ axisGridAtTicks
      $ layout_x_axis . laxis_style . axis_grid_style .~ solidLine 0.3 (opaque lightgrey)
      $ layout_bottom_axis_visibility . axis_show_ticks .~ False

      -- Y
      $ layout_y_axis . laxis_style . axis_grid_style .~ solidLine 0.15 (opaque lightgrey)
      $ layout_y_axis . laxis_override .~ axisGridAtBigTicks
      $ layout_left_axis_visibility . axis_show_ticks .~ True
      $ layout_right_axis_visibility . axis_show_line .~ True
      $ layout_right_axis_visibility . axis_show_ticks .~ True

      -- data
      $ layout_plots .~ [ plotBars bars2 ]
      $ def :: Layout PlotIndex LogValue

  bars2 = plot_bars_titles .~ ["","after","before"]
      $ plot_bars_values .~ addIndexes  dat'
      $ plot_bars_style .~ BarsStacked
      $ plot_bars_spacing .~ BarsFixGap 20 5
      $ plot_bars_item_styles .~ map (\c -> (solidFillStyle $ withOpacity c 0.7, Nothing)) [grey, red, green]
      $ def

  dat' = map (\[a,b] -> [ LogValue $ min a b
                        , LogValue $ if a < b then b - a else 0
                        , LogValue $ if b < a then a - b else 0]) dat

  alabels = map show [1..8]

