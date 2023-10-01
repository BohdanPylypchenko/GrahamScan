module Main where


import Vector
import GrahamScan
import Data.Tuple


main :: IO ()
main = do
    let points = [ Vector { x = 1, y = 1 },
                   Vector { x = 1, y = 2 },
                   Vector { x = 3.6, y = 2 },
                   Vector { x = 2, y = 4 },
                   Vector { x = 4, y = 3 },
                   Vector { x = 3, y = 1 },
                   Vector { x = 4.2, y = 4 },
                   Vector { x = 6, y = 5 } ]
    print (grahamScan points)