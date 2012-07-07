module MainState 
	( MainState(..)
	, InputContext(..)
	, Point
	, initMainState
	, hasIconAt
	, getIconAt
	, getIconAtCursor
	, setIconAt
	, setIconAtCursor
	, removeIconAt
	, removeIconAtCursor
	) where

import Graphics.UI.SDL as SDL
import Data.Map as Map

import Icon as Icon

startXSize = 640
startYSize = 480

type Point = (Int, Int)

data InputContext = IsInsert {
} | NoContext

data MainState = MainState {
   surface :: Surface,
   icons :: Icon.IconSet,
   position :: Point,
	screenSize :: (Int, Int),
   grid :: Map.Map Point Int,
	inputContext :: InputContext
}

testInitMap = Map.fromList [((0,0), 1), ((0,1), 2), ((0,(-1)), 2)]

initMainState = do
	SDL.init [InitEverything]
	setVideoMode startXSize startYSize 32 [SDL.DoubleBuf]
	icons_ <- getDefaultIcons
	screen <- getVideoSurface
	return $ MainState
		screen
		icons_
		(0,0)
		(startXSize, startYSize)
		testInitMap
		NoContext

hasIconAt :: MainState -> Point -> Bool
hasIconAt mainState point = Map.member point $ grid mainState

getIconAt :: MainState -> Point -> Icon.Icon
getIconAt mainState point = let
		_icons = icons mainState
		_grid = grid mainState
	in Icon.getByID _icons $ _grid Map.! point

getIconAtCursor :: MainState -> Icon.Icon
getIconAtCursor mainState = getIconAt mainState $ position mainState

setIconAt :: Point -> Int -> MainState -> MainState 
setIconAt point id mainState = let
		grid_ = grid mainState
		newGrid = Map.insert point id grid_
	in mainState { grid = newGrid }
	
setIconAtCursor :: Int -> MainState -> MainState 
setIconAtCursor id mainState = setIconAt (position mainState) id mainState 

removeIconAt :: Point -> MainState -> MainState 
removeIconAt point mainState = let
		newGrid = Map.delete point (grid mainState) 
	in mainState { grid = newGrid }

removeIconAtCursor :: MainState -> MainState
removeIconAtCursor mainState = removeIconAt (position mainState) mainState 
