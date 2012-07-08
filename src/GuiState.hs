module GuiState 
	( GuiState(..)
	, Point
	, InputContext(..)
	, initGui
	, changeContext
	) where

import Graphics.UI.SDL as SDL
import Data.Map as Map

import Icon as Icon

type Point = (Int, Int)

data InputContext = IsInsert {
} | NoContext 

data GuiState = GuiState {
	surface :: Surface,
	icons :: IconSet,  -- eventually think about having user-created and preloaded icons
	screenSize :: (Int, Int),
	inputContext :: InputContext
}

-- accessors
changeContext :: (InputContext -> InputContext) -> GuiState -> GuiState
changeContext f gs = gs { inputContext = f (inputContext gs) }

-- setup 
startXSize = 640
startYSize = 480

initGui = do
	SDL.init [InitEverything]
	setVideoMode startXSize startYSize 32 [SDL.DoubleBuf]
	icons_ <- getDefaultIcons
	screen <- getVideoSurface
	return $ GuiState
		screen
		icons_
		(startXSize, startYSize)
		NoContext
