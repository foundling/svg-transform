import Test.QuickCheck hiding (scale)
import SVG

-- scale 

class Arbitrary a where
    arbitrary :: Gen a

--quickCheck (prop_inverse :: ScaleFactor -> Shape -> Bool)
prop_inverse :: ScaleFactor -> Shape -> Bool
prop_inverse f s = scale (fromRational 1/f) (scale f s) == s 

