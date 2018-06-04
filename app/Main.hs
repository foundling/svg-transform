module SVG where

-- Types

type Length = Float
type Layer = Int
type Degree = Float
type Rotation = (Bool,Degree)
type ScaleFactor = Float

data Pos = Pos (Float,Float) 
                deriving (Eq,Show)

instance Num Pos where
    Pos (x,y) + Pos (x',y')               = Pos (x+x',y+y')
    Pos (x,y) * Pos (x',y')               = Pos (x*x',y*y')
    abs (Pos (x,y))                       = Pos (abs x,abs y) 
    negate (Pos (x,y))                    = Pos (negate x,negate y) 
    fromInteger a                         = Pos (fromInteger a,fromInteger a)
    signum (Pos (x,y))                    = Pos (signum x,signum y)

data Shape = Circle Pos Length 
           | Rect Pos Pos  
           | Polygon [Pos]
           | Polyline [Pos]
                deriving (Eq,Show)

data Color = Red 
           | Green 
           | Blue 
           | RGB (Int,Int,Int) 
           | RGBA (Int,Int,Int,Float) 
           | Hex [Char]
                deriving (Eq,Show)

data SVGElement = E Shape Color Layer
                deriving (Eq,Show)

data SVG = SVG [SVGElement]
                deriving (Eq,Show)

data SVGType = Point 
             | Line
             | PlaneFigure

type SVGPred = SVGElement -> Bool

-- Basic Predicates

hasColor :: SVGElement -> Color -> Bool
hasColor (E _ c _) c' = c == c'   

hasType :: SVGType -> SVGElement -> Bool
hasType PlaneFigure (E (Circle _ _) _ _) = True 
hasType PlaneFigure (E (Rect _ _) _ _) = True 
hasType Line (E (Polyline _) _ _) = True 
hasType Point (E (Polyline _) _ _) = True 
hasType _ _ = False


-- Higher-Order Predicates

onSVG :: ([SVGElement] -> [SVGElement]) -> SVG -> SVG
onSVG f (SVG es) = SVG (f es)

mapSVG :: (SVGElement -> SVGElement) -> SVG -> SVG
mapSVG f = onSVG (map f)

select :: (SVGElement -> Bool) -> SVG -> SVG
select f = onSVG (filter f)

(/\) :: SVGPred -> SVGPred -> SVGPred
(/\) p q e = p e && q e

(\/) :: SVGPred -> SVGPred -> SVGPred
(\/) p q e = p e || q e

-- Transformations

-- Translate 

translateShape :: Pos -> Shape -> Shape
translateShape p' (Circle p r) = Circle (p + p') r 
translateShape p' (Rect p1 p2) = Rect (p' + p1) (p' + p2)
translateShape p' (Polyline ps) = Polyline $ map (+p') ps
translateShape p' (Polygon ps) = Polygon $ map (+p') ps

translate :: Pos -> SVGElement -> SVGElement
translate p (E s c l) = E (translateShape p s) c l

-- Rotate

swap :: Pos -> Pos
swap (Pos (a,b)) = Pos (b,a)

flipFirst :: Pos -> Pos
flipFirst (Pos (a,b)) = Pos (negate a,b)

rotateShape :: Rotation -> Shape -> Shape
rotateShape r (Rect p1 p2) = Rect ((flipFirst . swap) p1) ((flipFirst . swap ) p2)

rotate :: Rotation -> SVGElement -> SVGElement
rotate r (E s c l) = E (rotateShape r s) c l

-- Scale
multiplyPoint :: Float -> Pos -> Pos
multiplyPoint n (Pos (p1,p2)) = Pos (n*p1,n*p2)

scale :: ScaleFactor -> Shape -> Shape
scale s (Rect (Pos (x,y)) (Pos (x',y'))) = Rect (Pos (x*s,y*s)) (Pos (x'*s,y'*s))
scale s (Circle (Pos (x,y)) r) = Circle (Pos ((x * s),(y*s)))  (r * s)
scale s (Polyline ps) = Polyline $ map (multiplyPoint s) ps
scale s (Polygon ps) = Polygon $ map (multiplyPoint s) ps



-- duplicate 
-- reflect
-- stretch
-- skew
-- append
-- align

-- instances for experimentation

c = E (Circle (Pos (1,2)) 4) Red 1
d = E (Circle (Pos (1,2)) 4) Green 1
e = E (Rect (Pos (1,2)) (Pos (3,4))) Red 1
els = [c,d,e]
