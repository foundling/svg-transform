import Test.QuickCheck hiding (scale)
import SVG

-- scale 
prop_inverse :: ScaleFactor -> Shape -> Bool
prop_inverse f s = scale (fromRational 1/f) (scale f s) == s 
