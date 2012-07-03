module MainState 
	( MainState(..)
	, initMainState
	, Point
	) where

import Graphics.UI.SDL as SDL
import Data.Map as Map

import Icon as Icon

startXSize = 640
startYSize = 480

type Point = (Int, Int)

data MainState = MainState {
   surface :: Surface,
   icons :: Icon.IconSet,
   position :: Point,
   grid :: Map.Map Point Int
}

testInitMap = Map.fromList [((0,0), 1), ((0,1), 2), ((0,(-1)), 2)]

initMainState = do
	SDL.init [InitEverything]
	setVideoMode startXSize startYSize 32 []
	icons_ <- getDefaultIcons
	screen <- getVideoSurface
	return $ MainState
		screen
		icons_
		(0,0)
		testInitMap

getIconAt :: MainState -> Point -> Icon.Icon
getIconAt (MainState _ icons _ grid) point = Icon.getByID icons $ grid Map.! point
