
module Compile 
	( compileMS
	) where

import Data.List

import qualified MainState as MS
import GuiState (GuiState(icons))
import qualified Icon
import qualified Point as P

data CompileState = CompileState
	{ saveableState :: MS.SaveableState
	, iconSet :: Icon.IconSet
	, fromDirection :: P.Direction
}

moveByDirection :: P.Direction -> CompileState -> CompileState
moveByDirection d cs = let
		newPos = (\(a,b) (c,d) -> (a+c,b+d)) (P.getOffset d) $ MS.position (saveableState cs)
		newSS = MS.changePosition (const newPos) $ saveableState cs
	in cs { saveableState = newSS, fromDirection = P.oppositeDirection d }

getIconDefinition :: CompileState -> String
getIconDefinition cs = let
		icon = Icon.getByID (iconSet cs) (MS.getIconID (saveableState cs))
	in Icon.definition icon

hasAdjacentIconAt :: CompileState -> P.Direction -> Bool
hasAdjacentIconAt cs d = let
		newCS = moveByDirection d cs
	in MS.hasIconAtSS (saveableState newCS) (MS.position $ saveableState newCS) 

-- makes initial CompileState by finding the first tile adjacent to the starting tile (if none is found, Nothing is returned)
makeInitialCompileState :: MS.SaveableState -> Icon.IconSet -> Maybe CompileState
makeInitialCompileState ss iconSet = let
		foundIcons = map (\d -> (d, MS.hasIconAtSS ss (P.getOffset d))) P.allDirections
		foundDirection = find snd foundIcons >>= (return . fst)
		changePos d = MS.changePosition (const (P.getOffset d))
	in foundDirection >>= (\d -> return $ CompileState (changePos d ss) iconSet (P.oppositeDirection d))

compileR :: CompileState -> String
compileR cs = let
		searchDirections = map (\d -> (d, hasAdjacentIconAt cs d)) . P.otherDirections . fromDirection $ cs
		foundDirections = map fst . filter snd $ searchDirections
		branches = map ((\s -> "(" ++ s ++ ")") . compileR . (\d -> moveByDirection d cs)) foundDirections
	in getIconDefinition cs ++ " "  ++ concat branches

compile :: MS.SaveableState -> Icon.IconSet -> Maybe String
compile ss iconSet = makeInitialCompileState ss iconSet >>= (return . compileR)

compileMS :: MS.MainState -> Maybe String
compileMS ms = compile ( MS.saveableState ms ) ( icons $ MS.guiState ms )
