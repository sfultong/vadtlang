module DrawFeedbackGrid
	( drawFeedback
	) where

import qualified GuiState 
import MainState as MS
import qualified Icon

import qualified Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF as TTF
import Graphics.UI.SDL.Color as Color

drawIconDefinition :: MS.MainState -> IO ()
drawIconDefinition ms = do
	let
		gs = MS.guiState ms
		rect = Just .GuiState.getFeedbackRect $ gs 
		iconText = Icon.definition . MS.getIconAtCursor $ ms
		fontColor = Color.Color 255 255 255
		font_ = GuiState.font gs
		screen_ = GuiState.surface gs
	--SDL.fillRect (GuiState.surface gs) (Just rect) (SDL.Pixel (128 * 128 * 128 - 1))
	-- TODO add in font writing here	
	fontSurface <- TTF.renderTextSolid font_ iconText fontColor
	SDL.blitSurface fontSurface Nothing screen_ rect
	return ()


drawFeedback :: MS.MainState -> IO ()
drawFeedback ms = let
		pos = MS.position . MS.saveableState $ ms
	in case MS.hasIconAt ms pos of
		True -> drawIconDefinition ms
		False -> return ()
