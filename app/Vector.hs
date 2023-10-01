module Vector(Vector(..), v1v2ToV, euclidMod, vectorProduct, cos2V) where


data Vector = Vector { x :: Double,
                       y :: Double }
    deriving Show

instance Eq Vector where
    Vector x1 y1 == Vector x2 y2 = (x1 == x2) && (y1 == y2)

v1v2ToV :: Vector -> Vector -> Vector
v1v2ToV v1 v2 = Vector (x2 - x1) (y2 - y1) 
    where { x1 = x v1; x2 = x v2;
            y1 = y v1; y2 = y v2 }

euclidMod :: Vector -> Double
euclidMod v = sqrt ((x v) ^ 2 + (y v) ^ 2)

vectorProduct :: Vector -> Vector -> Double
vectorProduct a b = (x a) * (y b) - (x b) * (y a)

cos2V :: Vector -> Vector -> Double
cos2V a b = (((x a) * (x b)) + ((y a) * (y b))) / ((euclidMod a) * (euclidMod b))