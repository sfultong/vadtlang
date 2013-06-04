
module Compile where

import Prelude hiding (Right, Left)
import Data.List

import qualified MainState as MS
import GuiState (Point)
import qualified Icon

-- TODO - refactor so that Main.hs uses directions (probably should put point and direction stuff in their own module)
data Direction = Up | Left | Down | Right deriving (Eq, Show)
allDirections = [ Up, Left, Down, Right ]

data CompileState = CompileState
	{ saveableState :: MS.SaveableState
	, iconSet :: Icon.IconSet
	, fromDirection :: Direction
	, programOutput :: String -- should be a rope or some AST representation, or really anything but a string
}

otherDirections :: Direction -> [Direction]
otherDirections d = take 3 . drop 1 . dropWhile (/= d) $ cycle allDirections

oppositeDirection :: Direction -> Direction
oppositeDirection Up = Down
oppositeDirection Left = Right
oppositeDirection Down = Up
oppositeDirection Right = Left

getOffset :: Direction -> (Int, Int)
getOffset Up = (0, (-1))
getOffset Left = ((-1), 0)
getOffset Down = (0, 1)
getOffset Right = (1, 0)

moveByDirection :: Direction -> CompileState -> CompileState
moveByDirection d cs = let
		newPos = (\(a,b) (c,d) -> (a+c,b+d)) (getOffset d) $ MS.position (saveableState cs)
		newSS = MS.changePosition (const newPos) $ saveableState cs
	in cs { saveableState = newSS, fromDirection = oppositeDirection d }

getIconDefinition :: CompileState -> String
getIconDefinition cs = let
		icon = Icon.getByID (iconSet cs) (MS.getIconID (saveableState cs))
	in Icon.definition icon

hasAdjacentIconAt :: CompileState -> Direction -> Bool
hasAdjacentIconAt cs d = let
		newCS = moveByDirection d cs
	in MS.hasIconAtSS (saveableState newCS) (MS.position $ saveableState newCS) 

-- makes initial CompileState by finding the first tile adjacent to the starting tile (if none is found, Nothing is returned)
makeInitialCompileState :: MS.SaveableState -> Icon.IconSet -> Maybe CompileState
makeInitialCompileState ss iconSet = let
		foundIcons = map (\d -> (d, MS.hasIconAtSS ss (getOffset d))) allDirections
		foundDirection = find snd foundIcons >>= (return . fst)
		changePos d = MS.changePosition (const (getOffset d))
	in foundDirection >>= (\d -> return $ CompileState (changePos d ss) iconSet (oppositeDirection d) "main = ")

compileR :: CompileState -> String
compileR cs = let
		searchDirections = map (\d -> (d, hasAdjacentIconAt cs d)) . otherDirections . fromDirection $ cs
		foundDirections = map fst . filter snd $ searchDirections
		branches = map ((\s -> "(" ++ s ++ ")") . compileR . (\d -> moveByDirection d cs)) foundDirections
	in getIconDefinition cs ++ " "  ++ concat branches

compile :: MS.SaveableState -> Icon.IconSet -> Maybe String
compile ss iconSet = makeInitialCompileState ss iconSet >>= (return . compileR)
