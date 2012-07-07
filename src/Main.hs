import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Primitives as PSDL
import qualified Data.Map as Map
import qualified Control.Concurrent.STM as STM

import MainState
import qualified Icon 

iconXSize = 16
iconYSize = 16

drawCursor :: MainState -> IO Bool
drawCursor mainState = let 
		(xSize, ySize) = screenSize mainState
		surface_ = surface mainState
		[centerX, centerY] = map (\x -> div x 2) [xSize, ySize]
		rect = SDL.Rect (centerX - 1) (centerY - 1) (centerX + iconXSize) (centerY + iconYSize)
	in PSDL.rectangle surface_ rect (SDL.Pixel (256 * 256 * 256 - 1)) 


drawScreen :: MainState -> IO ()
drawScreen mainstate = let
		(xSize, ySize) = screenSize mainstate
		[centerX, centerY] = map (\x -> div x 2) [xSize, ySize]
		surface_ = surface mainstate
		icons_ = icons mainstate
		(px,py) = position mainstate
		grid_ = grid mainstate
		xIcons = div xSize (iconXSize + 1)
		yIcons = div ySize (iconYSize + 1)
		positions = [(x, y) | -- positions of icons 
			x <- [px - div xIcons 2.. px + div xIcons 2], 
			y <- [py - div yIcons 2.. py + div yIcons 2]]
		clearScreen = SDL.fillRect surface_ Nothing (SDL.Pixel 0)
		drawIcon (x,y) = case Map.member (x,y) grid_ of
			True -> do 
				let drawIcon = Icon.getByID icons_ (grid_ Map.! (x,y)) 
				SDL.blitSurface (Icon.pic drawIcon) Nothing surface_ . Just $ SDL.Rect
					(centerX + (x - px) * (iconXSize + 1))
					(centerY + (y - py) * (iconYSize + 1))
					0 0
				return ()
			False -> return ()
	in do
		clearScreen
		mapM_ drawIcon positions
		drawCursor mainstate
		SDL.flip surface_

main = do
	state <- initMainState
	tvms <- STM.atomically . STM.newTVar $ state
	drawScreen state
	quitHandler tvms

quitHandler :: STM.TVar MainState -> IO ()
quitHandler tvms = do
	mainState <- STM.atomically . STM.readTVar $ tvms
	e <- SDL.waitEvent
	case e of
		SDL.Quit -> return ()
		SDL.KeyDown (SDL.Keysym k _ _) -> let
				modPosition s f = s { position = f $ position s }
				doDirectionKey f = do
					oldMainState <- STM.atomically . STM.readTVar $ tvms
					let newMainState = modPosition oldMainState f
					drawScreen newMainState
					STM.atomically $ STM.writeTVar tvms newMainState
					quitHandler tvms
			in case k of 
				SDL.SDLK_UP -> doDirectionKey (\(x, y) -> (x, y - 1))
				SDL.SDLK_DOWN -> doDirectionKey (\(x, y) -> (x, y + 1))
				SDL.SDLK_RIGHT -> doDirectionKey (\(x, y) -> (x + 1, y))
				SDL.SDLK_LEFT -> doDirectionKey (\(x, y) -> (x - 1, y))
				SDL.SDLK_ESCAPE -> return ()
				otherwise -> quitHandler tvms
		otherwise -> quitHandler tvms
