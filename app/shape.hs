module SVG where

-- Types

type Length = Int

data Pos = Pos (Int, Int) 
                deriving (Eq, Show)

data Shape = Circle Pos Length 
           | Rect Pos Pos  
           | Polygon [Pos]
           | Polyline [Pos]
                deriving (Eq, Show)

data Color = Red 
           | Green 
           | Blue 
           | RGB (Int, Int, Int) 
           | RGBA (Int, Int, Int, Float) 
           | Hex [Char]
                deriving (Eq, Show)

data SVGElement = E Shape Color
                deriving (Eq, Show)

data SVG = SVG [SVGElement]
                deriving (Eq, Show)

data SVGType = Point 
             | Line
             | PlaneFigure

-- Predicates

type SVGPred = SVGElement -> Bool

onSVG :: ([SVGElement] -> [SVGElement]) -> SVG -> SVG
onSVG f (SVG es) = SVG (f es)

mapSVG :: (SVGElement -> SVGElement) -> SVG -> SVG
mapSVG f = onSVG (map f)

filterSVG :: (SVGElement -> Bool) -> SVG -> SVG
filterSVG f = onSVG (filter f)

hasColor :: SVGElement -> Color -> Bool
hasColor (E _ c) c' = c == c'   

hasType :: SVGType -> SVGElement -> Bool
hasType PlaneFigure (E (Circle _ _) _) = True 
hasType Line (E (Polyline _) _) = True 
hasType Point (E (Polyline _) _) = True 
hasType _ _ = False

(/\) :: SVGPred -> SVGPred -> SVGPred
(/\) p q e = p e && q e

(\/) :: SVGPred -> SVGPred -> SVGPred
(\/) p q e = p e || q e

c = Circle (Pos (1,2)) 4
d = Circle (Pos (1,2)) 5
els = [E c Red, E d Red]

select :: SVGPred -> SVG -> SVG
select = filterSVG
