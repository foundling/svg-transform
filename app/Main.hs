module SVG where

-- Types

type Length = Int

data Pos = Pos (Int, Int) 
             deriving (Eq, Show, Ord)

instance Num Pos where
    (+) (Pos (x,y)) (Pos (x', y')) = Pos (x + x', y + y')
    (-) (Pos (x,y)) (Pos (x', y')) = Pos (x - x', y - y')
    (*) = error "error"
    abs (Pos (x,y)) = Pos (abs x, abs y) 
    signum = error "error" 
    fromInteger = error "error" 

data Color = Red | Green | Blue
             deriving (Eq, Show)

data Type = Point | Line | PlaneFigure
             deriving (Eq, Show)

data Shape = Circle Pos Length
           | Rect Pos Pos Pos Pos
           | Polyline [Pos]
           | Polygon [Pos]

data SVGElement = E Shape Color

data SVG = SVG [SVGElement]

-- Predicates

type SVGPred = SVGElement -> Bool

onSVG :: ([SVGElement] -> [SVGElement]) -> SVG -> SVG
onSVG f (SVG es) = SVG (f es)

mapSVG :: (SVGElement -> SVGElement) -> SVG -> SVG
mapSVG f = onSVG (map f)

hasColor :: Color -> SVGPred
hasColor c (E _ c') = c==c'

hasType :: Type -> SVGPred
hasType Point (E (Polyline [_]) _) = True
hasType Line  (E (Polyline _) _) = True
hasType PlaneFigure (E (Circle _ _) _) = True
hasType PlaneFigure (E (Rect _ _ _ _) _) = True
hasType _ _ = False

(/\) :: SVGPred -> SVGPred -> SVGPred
p /\ q = \e -> p e && q e

(\/) :: SVGPred -> SVGPred -> SVGPred
p \/ q = \e -> p e || q e 

neg :: SVGPred -> SVGPred
neg p = \e -> not (p e) 

addPositions :: Pos -> Pos -> Pos
addPositions (Pos p) (Pos p') = Pos (fst p + fst p', snd p + snd p')


-- Filtering
--
select :: SVGPred -> SVG -> SVG
select p (SVG es) = SVG (filter p es)

remove :: SVGPred -> SVG -> SVG 
remove p = select (neg p)

translate :: Pos -> SVGElement -> SVGElement
translate pos' (E (Circle pos r) c) = E (Circle (addPositions pos' pos) r) c
translate pos' (E (Rect p1 p2 p3 p4) c) = E (Rect (addPositions pos' p1) (addPositions pos' p2) (addPositions pos' p3) (addPositions pos' p4)) c
translate pos' (E (Polyline ps) c) = E (Polyline (map (addPositions pos') ps)) c

-- Transformations
--
setColorTo :: Color -> SVGElement -> SVGElement
setColorTo c (E s _) = E s c


-- Examples
--
mkRedLinesBlue :: SVG -> SVG
mkRedLinesBlue = mapSVG (setColorTo Blue) . select (hasColor Red /\ hasType Line)

moveRedCirclesAndSquares :: Pos -> SVG -> SVG
moveRedCirclesAndSquares p = mapSVG (translate p) . select (hasType PlaneFigure /\ hasColor Red)

removePlaneFigures :: SVG -> SVG 
removePlaneFigures = remove (hasType PlaneFigure)

circles = SVG [
    E (Circle (Pos (0,1)) 1) Red, 
    E (Circle (Pos (0,1)) 4) Blue  ]

circlesRemoved = removePlaneFigures circles
