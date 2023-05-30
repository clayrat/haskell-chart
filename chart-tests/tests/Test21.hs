module Test21 where

import Text.Printf
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
      ,[25.112  ,35.5643  ]
      ,[100.123 ,150.123  ]
      ,[1.333   ,1.0132   ]
      ,[2.345342,2.784568 ]
      ,[25.112  ,35.5643  ]
      ,[100.123 ,150.123  ]
      ,[1.333   ,1.0132   ]
      ,[2.345342,2.784568 ]
      ,[110.1233,101.363  ]
      ,[15.2213 ,12.856783]
      ,[25.112  ,35.5643  ]
      ,[100.123 ,150.123  ]
      ,[1.333   ,1.0132   ]
      ,[2.345342,2.784568 ]
      ,[2.345342,2.784568 ]
      ,[110.1233,101.363  ]
      ,[15.2213 ,12.856783]
      ,[25.112  ,35.5643  ]
      ,[100.123 ,150.123  ]
      ,[1.333   ,1.0132   ]
      ,[2.345342,2.784568 ]
      ,[110.1233,101.363  ]
      ,[15.2213 ,12.856783]
      ,[2.345342,2.784568 ]
      ,[110.1233,101.363  ]
      ,[15.2213 ,12.856783]
      ,[25.112  ,35.5643  ]
      ,[100.123 ,150.123  ]
      ,[1.333   ,1.0132   ]
      ,[2.345342,2.784568 ]
      ,[110.1233,101.363  ]
      ,[15.2213 ,12.856783]
      ,[7.223   ,8.9832   ]]

chart :: Renderable (LayoutPick LogValue PlotIndexRev PlotIndexRev)
chart = layoutToRenderable layout
 where
  layout =
      -- title
        layout_title .~ "Sample Horizontal Log Bars"
      $ layout_title_style . font_size .~ 10
      $ layout_legend . _Just . legend_position .~ LegendAbove

      -- Y
      $ layout_y_axis . laxis_generate .~ autoIndexRevTicksAxis alabels
      $ layout_y_axis . laxis_override .~ axisGridAtTicks
      $ layout_y_axis . laxis_style . axis_grid_style .~ solidLine 0.3 (opaque lightgrey)
      $ layout_left_axis_visibility . axis_show_ticks .~ False

      -- X
      $ layout_x_axis . laxis_style . axis_grid_style .~ solidLine 0.15 (opaque lightgrey)
      $ layout_x_axis . laxis_override .~ axisGridAtBigTicks
      $ layout_bottom_axis_visibility . axis_show_ticks .~ True
      $ layout_bottom_axis_visibility . axis_show_labels .~ False
      $ layout_top_axis_visibility . axis_show_line .~ True
      $ layout_top_axis_visibility . axis_show_ticks .~ True
      $ layout_top_axis_visibility . axis_show_labels .~ True

      -- data
      $ layout_plots .~ [ plotHBars bars2 ]
      $ def :: Layout LogValue PlotIndexRev

  bars2 = plot_bars_titles .~ ["","after","before"]
      $ plot_bars_values .~ addRevIndexes dat'
      $ plot_bars_style .~ BarsStacked
      $ plot_bars_spacing .~ BarsFixGap 20 3
      $ plot_bars_hover_labels ?~ (\[b,p,m] -> [ Nothing
                                               , Nothing
                                               , Just $ printf "%0.2f" $ if p /= 0 then unLogValue p else -unLogValue m])
      $ plot_bars_item_styles .~ map (\c -> (solidFillStyle $ withOpacity c 0.7, Nothing)) [grey, red, green]
      $ def

  dat' = map (\[a,b] -> [ LogValue $ min a b
                        , LogValue $ if a < b then b - a else 0
                        , LogValue $ if b < a then a - b else 0
                        ]) dat

  alabels = map (\n -> "val" ++ show n) $ take (length dat) [1..]

