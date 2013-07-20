module GuiState 
	( GuiState(..)
	, InputContext(..)
	, initGui
	, changeContext
	, iconGridSize
	, getFeedbackRect
	) where

import Graphics.UI.SDL as SDL
import Data.Map as Map
import Graphics.UI.SDL.TTF as TTF

import Icon as Icon
import qualified Point as P

data InputContext = IsInsert {
} | IsTextInsert | NoContext 

data GuiState = GuiState {
	surface :: Surface,
	font :: Font,
	icons :: IconSet,  -- eventually think about having user-created and preloaded icons
	inputContext :: InputContext
}

-- accessors
changeContext :: (InputContext -> InputContext) -> GuiState -> GuiState
changeContext f gs = gs { inputContext = f (inputContext gs) }

iconGridSize :: GuiState -> (Int, Int)
iconGridSize gs = let
		surf = surface gs
	in (SDL.surfaceGetWidth surf, SDL.surfaceGetHeight surf - startFeedbackYSize)

-- setup 
startGridXSize = 640
startGridYSize = 480
startFeedbackYSize = 100

-- helpers
getFeedbackRect :: GuiState -> SDL.Rect
getFeedbackRect gs = let
		surf = surface gs
		height = SDL.surfaceGetHeight surf
		width = SDL.surfaceGetWidth surf
		y1 = height - startFeedbackYSize
		x2 = width - 1
		y2 = height - 1
	in SDL.Rect 0 y1 x2 y2  

initGui = do
	SDL.init [InitEverything]
	TTF.init
	setVideoMode startGridXSize startGridYSize 32 [SDL.DoubleBuf]
	icons_ <- getDefaultIcons
	screen <- getVideoSurface
	font_ <- openFont "/usr/share/fonts/truetype/freefont/FreeSans.ttf" 12
	return $ GuiState
		screen
		font_
		icons_
		NoContext
