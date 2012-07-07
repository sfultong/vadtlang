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
		(px,py) = position mainstate
		xIcons = div xSize (iconXSize + 1)
		yIcons = div ySize (iconYSize + 1)
		positions = [(x, y) | -- positions of icons 
			x <- [px - div xIcons 2.. px + div xIcons 2], 
			y <- [py - div yIcons 2.. py + div yIcons 2]]
		clearScreen = SDL.fillRect surface_ Nothing (SDL.Pixel 0)
		drawIcon (x,y) = case hasIconAt mainstate (x,y) of
			True -> do 
				let drawIcon = getIconAt mainstate (x,y)
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
	let inputContext_ = inputContext mainState
	e <- SDL.waitEvent
	case e of
		SDL.Quit -> return ()
		SDL.KeyDown (SDL.Keysym k _ _) -> case inputContext_ of
			NoContext -> let
					modPosition s f = s { position = f $ position s }
					doDirectionKey f = do
						oldMainState <- STM.atomically . STM.readTVar $ tvms
						let newMainState = modPosition oldMainState f
						drawScreen newMainState
						STM.atomically $ STM.writeTVar tvms newMainState
						quitHandler tvms
				in case k of 
					SDL.SDLK_u -> doDirectionKey (\(x, y) -> (x, y - 1)) -- UP
					SDL.SDLK_e-> doDirectionKey (\(x, y) -> (x, y + 1)) -- DOWN
					SDL.SDLK_t -> doDirectionKey (\(x, y) -> (x + 1, y)) -- RIGHT
					SDL.SDLK_h -> doDirectionKey (\(x, y) -> (x - 1, y)) -- LEFT
					SDL.SDLK_i -> do -- INPUT
						STM.atomically $ do
							oldMainState <- STM.readTVar tvms
							STM.writeTVar tvms oldMainState { inputContext = IsInsert }
						quitHandler tvms
					SDL.SDLK_ESCAPE -> return ()
					otherwise -> quitHandler tvms
			IsInsert ->	let
					setNewIcon id = do
						newMainState <- STM.atomically $ do
							oldMainState <- STM.readTVar tvms
							STM.writeTVar tvms $ setIconAtCursor id $ oldMainState { inputContext = NoContext }
							STM.readTVar tvms
						drawScreen newMainState
						quitHandler tvms
					cancelInput = do
						STM.atomically $ do
							oldMainState <- STM.readTVar tvms
							STM.writeTVar tvms oldMainState { inputContext = NoContext }
						quitHandler tvms
				in case k of
					SDL.SDLK_i -> do -- clear icon
						newMainState <- STM.atomically $ do
							oldMainState <- STM.readTVar tvms
							STM.writeTVar tvms $ removeIconAtCursor $ oldMainState { inputContext = NoContext }
							STM.readTVar tvms
						drawScreen newMainState
						quitHandler tvms
					SDL.SDLK_u -> setNewIcon 1
					SDL.SDLK_e -> setNewIcon 2
					SDL.SDLK_ESCAPE -> cancelInput
					otherwise -> cancelInput
				
		otherwise -> quitHandler tvms
			
