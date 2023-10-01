module GrahamScan(grahamScan) where


import Vector
import Data.List (sortBy, delete, sortOn)


-- Direction

data Direction = LeftD | RightD | StraightD
    deriving (Eq, Show)

directionOfTriplet :: Vector -> Vector -> Vector -> Direction
directionOfTriplet v1 v2 v3
    | vectorProductFromTriplet == 0 = StraightD
    | vectorProductFromTriplet > 0  = LeftD
    | otherwise                     = RightD
        where
            vectorProductFromTriplet = vectorProduct (v1v2ToV v1 v2) (v1v2ToV v2 v3)


-- Graham scan

grahamScan :: [Vector] -> [Vector]
grahamScan [] = []
grahamScan points = grahamIteration pointsSortedByAngleP0 []
    where
        xOrt = Vector { x = 1.0, y = 0.0 }
        p0 = minByYX (head points) points
            where
                minByYX min [] = min
                minByYX min (p:ps)
                    | (y min) == (y p) = if (x min) <= (x p)
                                         then minByYX min ps
                                         else minByYX p ps
                    | (y min) < (y p) = minByYX min ps
                    | otherwise       = minByYX p ps
        pointsSortedByAngleP0 = reverse (sortBy (\a b -> compare (cos2V (v1v2ToV p0 a) xOrt) (cos2V (v1v2ToV p0 b) xOrt)) points)
        grahamIteration points hull
            | null points                                               = hull
            | (length hull > 1) && isDirectionOfCurrentTripletClockwise = grahamIteration points (tail hull)
            | otherwise                                                 = grahamIteration (tail points) (currentPoint:hull)
                where
                    currentPoint = head points
                    lastHullPoint = head hull
                    beforeLastHullPoint = hull!!1
                    directionOfCurrentTriplet = directionOfTriplet beforeLastHullPoint lastHullPoint currentPoint
                    isDirectionOfCurrentTripletClockwise = directionOfCurrentTriplet == RightD || directionOfCurrentTriplet == StraightD