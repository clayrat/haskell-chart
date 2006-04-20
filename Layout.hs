module Layout where

import qualified Graphics.Rendering.Cairo as C
import Axis
import Types
import Plot
import Renderable

data HAxis = HA_Top | HA_Bottom
data VAxis = VA_Left | VA_Right

-- | A Layout1 value is a single plot area, with optional axes on
-- each of the 4 sides, and an optional label at the top.
data Layout1 = Layout1 {
    layout1_background :: CairoFillStyle,
    layout1_title :: String,
    layout1_title_style :: CairoFontStyle,
    layout1_horizontal_axes :: AxesFn,
    layout1_vertical_axes :: AxesFn,
    layout1_margin :: Double,
    layout1_plots :: [(String,HAxis,VAxis,Plot)],
    layout1_legend :: Maybe(LegendStyle)
}

instance Rend Layout1 where
    toRenderable = layout1ToRenderable

layout1ToRenderable l =
    fillBackground (layout1_background l) (
        vertical [
            (0, addMargins (lm/2,0,0,0)    (mkTitle l)),
	    (1, addMargins (lm/2,lm,lm,lm) (plotArea l)),
	    (0, mkLegend l)
            ]
        )
  where
    lm = layout1_margin l

    mkTitle l = label (layout1_title_style l) HTA_Centre VTA_Centre (layout1_title l)

    mkLegend l = maybe emptyRenderable mkLegend' (layout1_legend l)
    mkLegend' ls = addMargins (0,lm,lm,0) (toRenderable (Legend True ls [(s,p) | (s,_,_,p) <- layout1_plots l]))
 
    plotArea l = Renderable {
        minsize=minsizePlotArea l,
        render=renderPlotArea l
    }

minsizePlotArea l = do
    (w1,h1,w2,h2) <- axisSizes l
    return (w1+w2,h1+h2)

renderPlotArea l (Rect p1 p5) = do
    let margin  = (layout1_margin l)

    (w1,h1,w2,h2) <- axisSizes l

    let p2 = p1 `padd` (Point w1 h1)
    let p4  = p5
    let p3  = p4 `psub` (Point w2 h2)

    -- render the axes
    rMAxis tAxis (mkrect p2 p1 p3 p2)
    rMAxis bAxis (mkrect p2 p3 p3 p4)
    rMAxis lAxis (mkrect p1 p2 p2 p3)
    rMAxis rAxis (mkrect p3 p2 p4 p3)

    -- render the plots
    C.save
    setClipRegion p2 p3 
    mapM_ (rPlot (Rect p2 p3)) (layout1_plots l)
    C.restore

  where
    (bAxis,lAxis,tAxis,rAxis) = getAxes l

    rMAxis :: Maybe AxisT ->  Rect -> C.Render ()
    rMAxis (Just at) rect = render (toRenderable at) rect
    rMAxis Nothing  _ = return ()

    rPlot :: Rect -> (String,HAxis,VAxis,Plot) -> C.Render ()
    rPlot rect (_,ha,va,p) = 
        let mxaxis = case ha of HA_Bottom -> bAxis
				HA_Top    -> tAxis
	    myaxis = case va of VA_Left   -> lAxis
				VA_Right  -> rAxis
        in rPlot1 rect mxaxis myaxis p
	      
    rPlot1 :: Rect -> Maybe AxisT -> Maybe AxisT -> Plot -> C.Render ()
    rPlot1 rect (Just (AxisT _ xaxis)) (Just (AxisT _ yaxis)) p = 
	let (x1,x2) = axis_viewport xaxis
	    (y1,y2) = axis_viewport yaxis
	in renderPlot p rect (Rect (Point x1 y1) (Point x2 y2))
    rPlot1 _ _ _ _ = return ()

axisSizes l = do
    w1a <- asize fst lAxis
    h1a <- asize snd tAxis
    w2a <- asize fst rAxis
    h2a <- asize snd bAxis
    (h1b,h2b) <- aohang lAxis
    (w1b,w2b) <- aohang tAxis
    (h1c,h2c) <- aohang rAxis
    (w1c,w2c) <- aohang bAxis

    return (maximum [w1a,w1b,w1c],
	    maximum [h1a,h1b,h1c],
	    maximum [w2a,w2b,w2c],
	    maximum [h2a,h2b,h2c] )
  where
    (bAxis,lAxis,tAxis,rAxis) = getAxes l

    asize xyfn Nothing = return 0
    asize xyfn (Just at) = do
        sz <- minsize (toRenderable at)
	return (xyfn sz)

    aohang Nothing = return (0,0)
    aohang (Just a) = axisOverhang a


getAxes :: Layout1 -> (Maybe AxisT, Maybe AxisT, Maybe AxisT, Maybe AxisT)
getAxes l = (mk E_Bottom bAxis, mk E_Left lAxis,
	     mk E_Top tAxis, mk E_Right rAxis)
  where 
    (xvals0,xvals1,yvals0,yvals1) = allPlottedValues (layout1_plots l)
    (bAxis,tAxis) = layout1_horizontal_axes l xvals0 xvals1
    (lAxis,rAxis) = layout1_vertical_axes l yvals0 yvals1
    mk _ Nothing = Nothing
    mk at (Just a) = Just (AxisT at a)


allPlottedValues :: [(String,HAxis,VAxis,Plot)] -> ( [Double], [Double], [Double], [Double] )
allPlottedValues plots = (xvals0,xvals1,yvals0,yvals1)
  where
    pts = concat [ [ (ha,va,pt)| pt <- allPoints p] | (_,ha,va,p) <- plots ]
    xvals0 = [ (p_x pt) | (HA_Bottom,_,pt) <- pts  ]
    xvals1 = [ (p_x pt) | (HA_Top,_,pt) <- pts  ]
    yvals0 = [ (p_y pt) | (_,VA_Left,pt) <- pts  ]
    yvals1 = [ (p_y pt) | (_,VA_Right,pt) <- pts  ]


defaultLayout1 = Layout1 {
    layout1_background = solidFillStyle 1 1 1,
    layout1_title = "",
    layout1_title_style = fontStyle "sans" 15 C.FontSlantNormal C.FontWeightBold,
    layout1_horizontal_axes = linkedAxes (autoScaledAxis defaultAxis),
    layout1_vertical_axes = linkedAxes (autoScaledAxis defaultAxis),
    layout1_margin = 10,
    layout1_plots = [],
    layout1_legend = Just defaultLegendStyle
}
