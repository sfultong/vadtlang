import Graphics.UI.SDL as SDL
import Data.Map as Map

import MainState
import qualified Icon 

iconXSize = 16
iconYSize = 16

drawScreen :: MainState -> IO ()
drawScreen mainstate = let
		xSize = 640
		ySize = 480
		surface_ = surface mainstate
		icons_ = icons mainstate
		(px,py) = position mainstate
		grid_ = grid mainstate
		xIcons = div xSize (iconXSize + 1)
		yIcons = div ySize (iconYSize + 1)
		positions = [(x, y) | -- positions of icons 
			x <- [px - div xIcons 2.. px + div xIcons 2], 
			y <- [py - div yIcons 2.. py + div yIcons 2]]
		drawIcon (x,y) = case Map.member (x,y) grid_ of
			True -> do 
				let drawIcon = Icon.getByID icons_ (grid_ Map.! (x,y)) 
				blitSurface (Icon.pic drawIcon) Nothing surface_ . Just $ Rect
					(div xSize 2 + (x - px) * (iconXSize + 1))
					(div ySize 2 + (y - py) * (iconYSize + 1))
					0 0
				return ()
			False -> return ()
	in do
		mapM_ drawIcon positions
		SDL.flip surface_

main = do
	state <- initMainState
	drawScreen state
	quitHandler

quitHandler :: IO ()
quitHandler = do
	e <- waitEvent
	case e of
		Quit -> return ()
		otherwise -> quitHandler
