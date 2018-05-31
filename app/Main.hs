module SVG where

-- Types

type Length = Int
type Layer = Int
type Degree = Float

data Pos = Pos (Int,Int) 
                deriving (Eq,Show)


instance Num Pos where
    Pos (x,y) + Pos (x',y')               = Pos (x+x',y+y')
    Pos (x,y) * Pos (x',y')               = Pos (x*x',y*y')
    abs (Pos (x,y))                       = Pos (abs x,abs y) 
    negate (Pos (x,y))                    = Pos (negate x,negate y) 
    fromInteger a                         = Pos (fromInteger a,fromInteger a)
    signum (Pos (x,y))                    = Pos (signum x,signum y)

type Rotation = (Bool,Degree)

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

-- this method is not correct for SVG coordinates
-- d for now and does a 90 degree rotation

swap :: Pos -> Pos
swap (Pos (a,b)) = Pos (b,a)

flipFirst :: Pos -> Pos
flipFirst (Pos (a,b)) = Pos (negate a,b)

rotateShape :: Rotation -> Shape -> Shape
rotateShape (Clockwise d) (Rect p1 p2) = Rect ((flipFirst . swap) p1) ((flipFirst . swap ) p2)

rotate :: Rotation -> SVGElement -> SVGElement
rotate r (E s c l) = E (rotateShape r s) c l

-- duplicate 
-- scale
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
