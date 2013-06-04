{-# LANGUAGE DeriveGeneric #-}

module MainState 
	( MainState(..)
	, SaveableState(..)
	, makeDefaultState
	, changePosition
	, changeGrid
	, changeSaveable
	, changeGui
	, hasIconAt
	, hasIconAtSS
	, getIconAt
	, getIconAtCursor
	, getIconID
	, setIconAt
	, setIconAtCursor
	, setIconAtCursorByName
	, removeIconAt
	, removeIconAtCursor
	) where

import GHC.Generics
import Data.Serialize
import qualified Data.Map as Map

import Icon as Icon
import qualified GuiState as GUI

data SaveableState = SaveableState {
   position :: GUI.Point,
   grid :: Map.Map GUI.Point Int
} deriving Generic

instance Serialize SaveableState

data MainState = MainState {
	saveableState :: SaveableState,
	guiState :: GUI.GuiState
} 

-- accessors
changeGrid :: (Map.Map GUI.Point Int -> Map.Map GUI.Point Int) -> SaveableState -> SaveableState
changeGrid f ss = ss { grid = f (grid ss) }

changeSaveable :: (SaveableState -> SaveableState) -> MainState -> MainState
changeSaveable f ms = ms { saveableState = f (saveableState ms) } 

changePosition :: (GUI.Point -> GUI.Point) -> SaveableState -> SaveableState
changePosition f ss = let
		newPosition = f (position ss) 
	in case newPosition of
		(0,0) -> ss
		_ -> ss { position = newPosition }

changeGui :: (GUI.GuiState -> GUI.GuiState) -> MainState -> MainState
changeGui f ms = ms { guiState = f (guiState ms) }

-- setup
testInitMap = Map.fromList [((0,0), Icon.startIconIndex)]

makeDefaultState :: GUI.GuiState -> MainState
makeDefaultState = MainState (SaveableState (0,1) testInitMap)

-- exported functions
hasIconAt :: MainState -> GUI.Point -> Bool
hasIconAt mainState point = Map.member point . grid . saveableState $ mainState

hasIconAtSS :: SaveableState -> GUI.Point -> Bool
hasIconAtSS ss point = Map.member point . grid $ ss

getIconAt :: MainState -> GUI.Point -> Icon.Icon
getIconAt mainState point = let
		_icons = GUI.icons . guiState $ mainState
		_grid = grid . saveableState $ mainState
	in Icon.getByID _icons $ _grid Map.! point

getIconAtCursor :: MainState -> Icon.Icon
getIconAtCursor mainState = getIconAt mainState . position . saveableState $ mainState

getIconID :: SaveableState -> Int
getIconID ss = (grid ss) Map.! (position ss)

setIconAt :: GUI.Point -> Int -> MainState -> MainState 
setIconAt point id = changeSaveable (changeGrid (Map.insert point id))
	
setIconAtCursor :: Int -> MainState -> MainState 
setIconAtCursor id mainState = setIconAt (position $ saveableState mainState) id mainState 

setIconAtCursorByName :: String -> MainState -> MainState
setIconAtCursorByName name mainState = let
		iconSet = GUI.icons . guiState $ mainState
		id = getID iconSet name
	in setIconAtCursor id mainState

removeIconAt :: GUI.Point -> MainState -> MainState 
removeIconAt point = changeSaveable (changeGrid (Map.delete point))

removeIconAtCursor :: MainState -> MainState
removeIconAtCursor mainState = removeIconAt (position $ saveableState mainState) mainState 


