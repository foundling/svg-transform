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
    Pos (x,y) - Pos (x',y')               = Pos (x-x',y-y')
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
rotateShape r (Circle p l) = Circle p l

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

-- Reflect --

slopeFromPoints :: Pos -> Pos -> Float
slopeFromPoints (Pos (x,y)) (Pos (x',y')) = (y'-y)/(x'-x)

intersectionOfTwoLines :: (Float,Float) -> (Float,Float) -> Pos
intersectionOfTwoLines (a,c) (b,d) = Pos (x,y) 
    where
        y = a*x + c 
        x = (d-c)/(a-b)

pointsToLineEq :: Pos -> Pos -> (Float,Float)
pointsToLineEq (Pos (x,y)) (Pos (x',y')) = (m,b)
    where
        b = y - m*x
        m = (y' - y)/(x' - x)

reflectOnXAxis :: Shape -> Float -> Shape
reflectOnXAxis (Circle (Pos(x,y)) r) rY = (Circle (Pos(x,y')) r)    
    where
        y' = y - 2*(y - rY)

reflectOnYAxis :: Shape -> Float -> Shape
reflectOnYAxis (Circle (Pos (x,y)) r) rX = (Circle (Pos (x',y)) r)    
    where 
        x' = x - 2*(x - rX)


reflectOnDiagonal :: Shape -> Pos -> Pos -> Shape
reflectOnDiagonal (Circle (Pos (x,y)) r) p1 p2 = (Circle reflectedPoint r) 
    where
        reflectedPoint = dVector + intersectionPoint
        dVector = intersectionPoint - (Pos (x, y))
        intersectionPoint = Pos (iX, iY)
            where 
                -- https://en.wikipedia.org/wiki/Line-line_intersection
                iY = a * iX + c 
                iX = (d - c)/(a - b)
                d = snd perpendicularLineEq
                c = snd lineEq
                a = fst lineEq
                b = fst perpendicularLineEq

        perpendicularLineEq = (m,b)
            where
                b = y - m * x 
                m = (negate . recip) $ fst lineEq 

        lineEq = pointsToLineEq p1 p2

reflect :: Shape -> Pos -> Pos -> Shape
reflect s (Pos (x,y)) (Pos (x',y'))
    | x == x' = reflectOnYAxis s x 
    | y == y' = reflectOnXAxis s y
    | otherwise = reflectOnDiagonal s (Pos (x,y)) (Pos (x',y'))
     
-- ALIGN

alignX :: Shape -> Pos -> Shape 
alignX (Circle (Pos (x,y)) r) xAxis = (Circle (Pos alignedPoint, y) r)
    where 
        alignedPoint = closestPoint - distance 
        distance = closestPoint - x
        closestPoint = min [leftmostPoint,rightmostPoint]
        leftmostPoint = (r + x) - xAxis
        rightmostPoint = (r - x) - xAxis
    




-- stretch
-- skew
-- append
-- duplicate 
