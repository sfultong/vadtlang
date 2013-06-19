module Point
	( Point
	, Direction(..)
	, otherDirections
	, allDirections
	, oppositeDirection
	, getOffset 
	, moveByDirection
	) where

import Prelude hiding (Right, Left)
import Data.List

-- TODO - refactor so that Main.hs uses directions

type Point = (Int, Int)

data Direction = Up | Left | Down | Right deriving (Eq, Show)
allDirections = [ Up, Right, Down, Left ]

otherDirections :: Direction -> [Direction]
otherDirections d = take 3 . drop 1 . dropWhile (/= d) $ cycle allDirections

oppositeDirection :: Direction -> Direction
oppositeDirection Up = Down
oppositeDirection Left = Right
oppositeDirection Down = Up
oppositeDirection Right = Left

getOffset :: Direction -> Point
getOffset Up = (0, (-1))
getOffset Left = ((-1), 0)
getOffset Down = (0, 1)
getOffset Right = (1, 0)

moveByDirection :: Direction -> Point -> Point
moveByDirection d = (\(a,b) (c,d) -> (a+c,b+d)) (getOffset d)
