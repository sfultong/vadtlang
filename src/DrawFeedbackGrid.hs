module DrawFeedbackGrid
	( drawFeedback
	) where

import qualified GuiState 
import MainState as MS
import qualified Icon

import qualified Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF as TTF
import Graphics.UI.SDL.Color as Color

data FeedbackGridParts = FeedbackGridParts {
	functionDisplay :: SDL.Rect,
	iconDisplay :: SDL.Rect
}

makeFeedbackGridParts :: SDL.Rect -> FeedbackGridParts
makeFeedbackGridParts (SDL.Rect x1 y1 x2 y2) = let
		funDisplay = SDL.Rect x1 y1 x2 (y1 + 50)
		iconDisplay = SDL.Rect x1 (y1 + 51) x2 y2
	in FeedbackGridParts funDisplay iconDisplay

drawIconDefinition :: MS.MainState -> IO ()
drawIconDefinition ms = do
	let
		gs = MS.guiState ms
		displayParts = makeFeedbackGridParts . GuiState.getFeedbackRect $ gs 
		rect = Just $ iconDisplay displayParts 
		iconText = Icon.definition . MS.getIconAtCursor $ ms
		fontColor = Color.Color 255 255 255
		font_ = GuiState.font gs
		screen_ = GuiState.surface gs
	--SDL.fillRect (GuiState.surface gs) (Just rect) (SDL.Pixel (128 * 128 * 128 - 1))
	-- TODO add in font writing here	
	iconDisplaySurface <- TTF.renderTextSolid font_ iconText fontColor
	SDL.blitSurface iconDisplaySurface Nothing screen_ rect
	return ()


drawFeedback :: MS.MainState -> IO ()
drawFeedback ms = let
		gs = MS.guiState ms
		displayParts = makeFeedbackGridParts . GuiState.getFeedbackRect $ gs 
		fontColor = Color.Color 255 255 255
		font_ = GuiState.font gs
		screen_ = GuiState.surface gs
		pos = MS.position . MS.saveableState $ ms
		gridName = case MS.currentGrid . MS.saveableState $ ms of
			0 -> "main"
			n -> Icon.definition . flip Icon.getByID n . GuiState.icons . MS.guiState $ ms 
	in do
		functionDisplaySurface <- TTF.renderTextSolid font_ gridName fontColor
		SDL.blitSurface functionDisplaySurface Nothing screen_ . Just $ functionDisplay displayParts
		case MS.hasIconAt ms pos of
			True -> drawIconDefinition ms
			False -> return ()
