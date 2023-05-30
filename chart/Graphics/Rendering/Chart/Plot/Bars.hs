-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Plot.Bars
-- Copyright   :  (c) Tim Docker 2006, 2014
-- License     :  BSD-style (see chart/COPYRIGHT)
--
-- Bar Charts
--
{-# LANGUAGE TemplateHaskell #-}

module Graphics.Rendering.Chart.Plot.Bars(
    PlotBars(..),
    PlotBarsStyle(..),
    PlotBarsSpacing(..),
    PlotBarsAlignment(..),
    BarsPlotValue(..),

    plotBars,
    plotHBars,
    plot_bars_style,
    plot_bars_item_styles,
    plot_bars_titles,
    plot_bars_spacing,
    plot_bars_alignment,
    plot_bars_reference,
    plot_bars_singleton_width,
    plot_bars_hover_labels,
    plot_bars_values,

) where

import Control.Lens
import Control.Monad
import Data.Tuple(swap)
import Data.List(nub,sort)
import Graphics.Rendering.Chart.Geometry hiding (x0, y0)
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Plot.Types
import Graphics.Rendering.Chart.Axis ()
import Graphics.Rendering.Chart.Axis.Types
import Graphics.Rendering.Chart.Axis.Floating (LogValue(..))
import Graphics.Rendering.Chart.Utils ( log10, whenJust )
import Data.Colour (opaque)
import Data.Colour.Names (black)
import Data.Default.Class

class PlotValue a => BarsPlotValue a where
    barsIsNull    :: a -> Bool
    barsReference :: [a] -> a
    barsAdd       :: a -> a -> a

instance BarsPlotValue Double where
    barsIsNull a  = a == 0.0
    barsReference = const 0
    barsAdd       = (+)
instance BarsPlotValue Int where
    barsIsNull a  = a == 0
    barsReference = const 0
    barsAdd       = (+)

instance BarsPlotValue LogValue where
    barsIsNull (LogValue a) = a == 0.0
    barsReference as        =
      10.0 ^^ (floor (log10 $ minimum $ filter (/= 0.0) as) :: Integer)
    barsAdd                 = (+)

data PlotBarsStyle
    = BarsStacked   -- ^ Bars for a fixed x are stacked vertically
                    --   on top of each other.
    | BarsClustered -- ^ Bars for a fixed x are put horizontally
                    --   beside each other.
     deriving (Show)

data PlotBarsSpacing
    = BarsFixWidth Double       -- ^ All bars have the same width in pixels.
    | BarsFixGap Double Double  -- ^ (BarsFixGap g mw) means make the gaps between
                                --   the bars equal to g, but with a minimum bar width
                                --   of mw
     deriving (Show)

-- | How bars for a given (x,[y]) are aligned with respect to screen
--   coordinate corresponding to x (deviceX).
data PlotBarsAlignment = BarsLeft      -- ^ The left edge of bars is at deviceX
                       | BarsCentered  -- ^ Bars are centered around deviceX
                       | BarsRight     -- ^ The right edge of bars is at deviceX
     deriving (Show)

-- | Value describing how to plot a set of bars.
--   Note that the input data is typed [(x,[y])], ie for each x value
--   we plot several y values. Typically the size of each [y] list would
--   be the same.
data PlotBars x y = PlotBars {
   -- | This value specifies whether each value from [y] should be
   --   shown beside or above the previous value.
   _plot_bars_style           :: PlotBarsStyle,

   -- | The style in which to draw each element of [y]. A fill style
   --   is required, and if a linestyle is given, each bar will be
   --   outlined.
   _plot_bars_item_styles     :: [ (FillStyle,Maybe LineStyle) ],

   -- | The title of each element of [y]. These will be shown in the legend.
   _plot_bars_titles          :: [String],

   -- | This value controls how the widths of the bars are
   --   calculated. Either the widths of the bars, or the gaps between
   --   them can be fixed.
   _plot_bars_spacing         :: PlotBarsSpacing,

   -- | This value controls how bars for a fixed x are aligned with
   --   respect to the device coordinate corresponding to x.
   _plot_bars_alignment       :: PlotBarsAlignment,

   -- | The starting level for the chart, a function of some statistic
   --   (normally the lowest value or just const 0).
   _plot_bars_reference       :: [y] -> y,

   _plot_bars_singleton_width :: Double,

   -- | Optional function to display hovering labels above each bar
   _plot_bars_hover_labels    :: Maybe ([y] -> [Maybe String]),

   -- | The actual points to be plotted.
   _plot_bars_values          :: [ (x,[y]) ]
}

instance BarsPlotValue y => Default (PlotBars x y) where
  def = PlotBars
    { _plot_bars_style           = BarsClustered
    , _plot_bars_item_styles     = cycle istyles
    , _plot_bars_titles          = []
    , _plot_bars_spacing         = BarsFixGap 10 2
    , _plot_bars_alignment       = BarsCentered
    , _plot_bars_values          = []
    , _plot_bars_singleton_width = 20
    , _plot_bars_hover_labels    = Nothing
    , _plot_bars_reference       = barsReference
    }
    where
      istyles   = map mkstyle defaultColorSeq
      mkstyle c = (solidFillStyle c, Just (solidLine 1.0 $ opaque black))

plotBars :: (BarsPlotValue y) => PlotBars x y -> Plot x y
plotBars p = Plot {
        _plot_render     = renderPlotBars p,
        _plot_legend     = zip (_plot_bars_titles p)
                               (map renderPlotLegendBars
                                    (_plot_bars_item_styles p)),
        _plot_all_points = allBarPoints p
    }

renderPlotBars :: (BarsPlotValue y) => PlotBars x y -> PointMapFn x y -> BackendProgram ()
renderPlotBars p pmap = case _plot_bars_style p of
      BarsClustered -> forM_ vals clusteredBars
      BarsStacked   -> forM_ vals stackedBars
  where
    clusteredBars (x,ys) = do
       let offset = case _plot_bars_alignment p of
              BarsLeft     -> \i -> fromIntegral i * width
              BarsRight    -> \i -> fromIntegral (i-nys) * width
              BarsCentered -> \i -> fromIntegral (2*i-nys) * width/2
       forM_ (zip3 [0..] ys styles) $ \(i, y, (fstyle,_)) ->
           unless (barsIsNull y) $
           withFillStyle fstyle $
             alignFillPath (barPath (offset i) x yref0 y)
             >>= fillPath
       forM_ (zip3 [0..] ys styles) $ \(i, y, (_,mlstyle)) ->
           unless (barsIsNull y) $
           whenJust mlstyle $ \lstyle ->
             withLineStyle lstyle $
               alignStrokePath (barPath (offset i) x yref0 y)
               >>= strokePath
       whenJust (_plot_bars_hover_labels p) $ \f ->
         forM_ (zip3 [0..] ys (f ys)) $ \(i, y, s') ->
            whenJust s' $ \s ->
              do let Point x' y' = pmap' (x, y)
                 (tx, ty) <- textDimension s
                 drawText (Point (x' - tx/2 + offset i + width/2) (y' - ty/2)) s

    stackedBars (x,ys) = do
       let y2s = zip (yref0:stack ys) (stack ys)
       let ofs = case _plot_bars_alignment p of
             BarsLeft     -> 0
             BarsRight    -> -width
             BarsCentered -> -(width/2)
       forM_ (zip y2s styles) $ \((y0,y1), (fstyle,_)) ->
           unless (y0 == y1) $
           withFillStyle fstyle $
             alignFillPath (barPath ofs x y0 y1)
             >>= fillPath
       forM_ (zip y2s styles) $ \((y0,y1), (_,mlstyle)) ->
           unless (y0 == y1) $
           whenJust mlstyle $ \lstyle ->
              withLineStyle lstyle $
                alignStrokePath (barPath ofs x y0 y1)
                >>= strokePath
       whenJust (_plot_bars_hover_labels p) $ \f ->
         forM_ (zip (stack ys) (f ys)) $ \(y,s') ->
            whenJust s' $ \s ->
              do let Point x' y' = pmap' (x, y)
                 (tx, ty) <- textDimension s
                 drawText (Point (x' - tx/2) (y' - ty/2)) s

    barPath xos x y0 y1 =
      let Point x' y' = pmap' (x,y1)
          Point _ y0' = pmap' (x,y0)
      in
      rectPath (Rect (Point (x'+xos) y0') (Point (x'+xos+width) y'))

    vals  = _plot_bars_values p
    lowerVals = case _plot_bars_style p of
                  BarsClustered -> concatMap snd vals
                  BarsStacked   -> map (head . snd) vals

    yref0 = _plot_bars_reference p lowerVals

    width = case _plot_bars_spacing p of
        BarsFixGap gap minw -> let w = max (minXInterval - gap) minw in
            case _plot_bars_style p of
                BarsClustered -> w / fromIntegral nys
                BarsStacked -> w
        BarsFixWidth width' -> width'
    styles = _plot_bars_item_styles p

    minXInterval = let diffs = zipWith (-) (tail mxs) mxs
                   in if null diffs
                        then _plot_bars_singleton_width p
                        else minimum diffs
      where mxs = nub $ sort $ map (mapX . fst) $ _plot_bars_values p

    nys    = maximum [ length ys | (_,ys) <- vals ]

    pmap'  = mapXY pmap
    mapX x = p_x $ pmap' (x, yref0)

allBarPoints :: (BarsPlotValue y) => PlotBars x y -> ([x],[y])
allBarPoints p = case _plot_bars_style p of
    BarsClustered ->
      let ys = concatMap snd pts in
      ( xs, f0 ys:ys )
    BarsStacked   ->
      let ys = map snd pts in
      ( xs, f0 (map head ys):concatMap stack ys)
  where
    pts = _plot_bars_values p
    xs  = map fst pts
    f0  = _plot_bars_reference p

--- horizontal

plotHBars :: (BarsPlotValue x) => PlotBars y x -> Plot x y
plotHBars p = Plot {
        _plot_render     = renderPlotHBars p,
        _plot_legend     = zip (_plot_bars_titles p)
                               (map renderPlotLegendBars
                                    (_plot_bars_item_styles p)),
        _plot_all_points = swap $ allBarPoints p

    }

renderPlotHBars :: (BarsPlotValue x) => PlotBars y x -> PointMapFn x y -> BackendProgram ()
renderPlotHBars p pmap = case _plot_bars_style p of
      BarsClustered -> forM_ vals clusteredBars
      BarsStacked   -> forM_ vals stackedBars
  where
    clusteredBars (y,xs) = do
       let offset = case _plot_bars_alignment p of
              BarsLeft     -> \i -> fromIntegral i * height
              BarsRight    -> \i -> fromIntegral (i-nxs) * height
              BarsCentered -> \i -> fromIntegral (2*i-nxs) * height/2
       forM_ (zip3 [0..] xs styles) $ \(i, x, (fstyle,_)) ->
           unless (barsIsNull x) $
           withFillStyle fstyle $
             alignFillPath (barPath (offset i) xref0 x y)
             >>= fillPath
       forM_ (zip3 [0..] xs styles) $ \(i, x, (_,mlstyle)) ->
           unless (barsIsNull x) $
           whenJust mlstyle $ \lstyle ->
             withLineStyle lstyle $
               alignStrokePath (barPath (offset i) xref0 x y)
               >>= strokePath
       whenJust (_plot_bars_hover_labels p) $ \f ->
         forM_ (zip3 [0..] xs (f xs)) $ \(i, x, s') ->
            whenJust s' $ \s ->
              do let Point x' y' = pmap' (x, y)
                 (tx, ty) <- textDimension s
                 drawText (Point (x' - tx/2 + offset i + height/2) (y' - ty/2)) s

    stackedBars (y,xs) = do
       let x2s = zip (xref0:stack xs) (stack xs)
       let ofs = case _plot_bars_alignment p of
             BarsLeft     -> 0
             BarsRight    -> -height
             BarsCentered -> -(height/2)
       forM_ (zip x2s styles) $ \((x0,x1), (fstyle,_)) ->
           unless (x0 == x1) $
           withFillStyle fstyle $
             alignFillPath (barPath ofs x0 x1 y)
             >>= fillPath
       forM_ (zip x2s styles) $ \((x0,x1), (_,mlstyle)) ->
           unless (x0 == x1) $
           whenJust mlstyle $ \lstyle ->
              withLineStyle lstyle $
                alignStrokePath (barPath ofs x0 x1 y)
                >>= strokePath
       whenJust (_plot_bars_hover_labels p) $ \f ->
         forM_ (zip (stack xs) (f xs)) $ \(x,s') ->
            whenJust s' $ \s ->
              do let Point x' y' = pmap' (x, y)
                 (tx, ty) <- textDimension s
                 drawText (Point (x' - tx/2) (y' - ty/2)) s

    barPath yos x0 x1 y =
      let Point x' y' = pmap' (x1,y)
          Point x0' _ = pmap' (x0,y)
      in
      rectPath (Rect (Point x0' (y' + yos)) (Point x' (y' + yos + height)))

    vals  = --let (as, bs) = unzip $
            _plot_bars_values p
            -- in zip as (reverse bs)
    lowerVals = case _plot_bars_style p of
                  BarsClustered -> concatMap snd vals
                  BarsStacked   -> map (head . snd) vals

    xref0 = _plot_bars_reference p lowerVals

    height = case _plot_bars_spacing p of
        BarsFixGap gap minw -> let w = max (minYInterval - gap) minw in
            case _plot_bars_style p of
                BarsClustered -> w / fromIntegral nxs
                BarsStacked -> w
        BarsFixWidth width' -> width'
    styles = _plot_bars_item_styles p

    minYInterval = let diffs = zipWith (-) (tail mxs) mxs
                   in if null diffs
                        then _plot_bars_singleton_width p
                        else minimum diffs
      where mxs = nub $ sort $ map (mapY . fst) $ _plot_bars_values p

    nxs    = maximum [ length xs | (_,xs) <- vals ]

    pmap'  = mapXY pmap
    mapY y = p_y $ pmap' (xref0, y)

-- util

stack :: (BarsPlotValue y) => [y] -> [y]
stack = scanl1 barsAdd

renderPlotLegendBars :: (FillStyle,Maybe LineStyle) -> Rect -> BackendProgram ()
renderPlotLegendBars (fstyle,_) r =
  withFillStyle fstyle $
    fillPath (rectPath r)

$( makeLenses ''PlotBars )
