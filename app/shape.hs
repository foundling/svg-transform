module SVG where

-- Types

type Length = Int
type Layer = Int

data Rotation = Clockwise 
              | CounterClockwise

data Pos = Pos (Int, Int) 
                deriving (Eq, Show)

instance Num Pos where
    (+)         (Pos (x,y)) (Pos (x', y'))  = Pos (x + x', y + y')
    (*)         (Pos (x,y)) (Pos (x', y'))  = Pos (x * x', y + y')
    (abs)       (Pos (x,y))                 = undefined -- QUESTION 
    (negate)    (Pos (x, y))                = Pos (negate x, negate y)
    (fromInteger) a                         = Pos (fromInteger a, fromInteger a)
    (signum)    (Pos (x,y))  
                    | x < 0 || y < 0 = (-1)
                    | otherwise = 1


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

data SVGElement = E Shape Color Layer
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
hasColor (E _ c _) c' = c == c'   

hasType :: SVGType -> SVGElement -> Bool
hasType PlaneFigure (E (Circle _ _) _ _) = True 
hasType PlaneFigure (E (Rect _ _) _ _) = True 
hasType Line (E (Polyline _) _ _) = True 
hasType Point (E (Polyline _) _ _) = True 
hasType _ _ = False

(/\) :: SVGPred -> SVGPred -> SVGPred
(/\) p q e = p e && q e

(\/) :: SVGPred -> SVGPred -> SVGPred
(\/) p q e = p e || q e

c = E (Circle (Pos (1,2)) 4) Red 1
d = E (Circle (Pos (1,2)) 4) Green 1
e = E (Rect (Pos (1,2)) (Pos (3,4))) Red 1
els = [c,d]

select :: SVGPred -> SVG -> SVG
select = filterSVG

-- translate
translateShape :: Pos -> Shape -> Shape
translateShape p' (Circle p r) = Circle (p + p') r 
translateShape p' (Rect p1 p2) = Rect (p' + p1) (p' + p2)
translateShape p' (Polyline ps) = Polyline $ map (+p') ps
translateShape p' (Polygon ps) = Polygon $ map (+p') ps

translate :: Pos -> SVGElement -> SVGElement
translate p (E s c l) = E (translateShape p s) c l

-- rotate

rotateShape :: Rotation -> Shape -> Shape
rotateShape Clockwise (Rect (Pos (x1,y1)) (Pos (x2,y2))) = (Rect (Pos (negate y1,x1)) (Pos (negate y2,x2)))   
rotateShape CounterClockwise (Rect (Pos (x1,y1)) (Pos (x2,y2))) = (Rect (Pos (negate y1,x1)) (Pos (negate y2,x2)))   

rotate :: Rotation -> SVGElement -> SVGElement
rotate r (E s c l) = E (rotateShape r s) c l

-- duplicate :: 
-- replace :: 
-- scale ::
-- reflect ::
-- append
-- align
