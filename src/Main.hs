import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Primitives as PSDL
import qualified Data.Map as Map
import qualified Control.Concurrent.STM as STM
import qualified Data.ByteString as B
import qualified Data.Serialize as Serial

import MainState
import GuiState
import qualified Icon 

iconXSize = 16
iconYSize = 16

drawCursor :: MainState -> IO Bool
drawCursor mainState = let 
		(xSize, ySize) = screenSize $ guiState mainState
		surface_ = surface $ guiState mainState
		[centerX, centerY] = map (\x -> div x 2) [xSize, ySize]
		rect = SDL.Rect (centerX - 1) (centerY - 1) (centerX + iconXSize) (centerY + iconYSize)
	in PSDL.rectangle surface_ rect (SDL.Pixel (256 * 256 * 256 - 1)) 


drawScreen :: MainState -> IO ()
drawScreen mainstate = let
		(xSize, ySize) = screenSize $ guiState mainstate
		[centerX, centerY] = map (\x -> div x 2) [xSize, ySize]
		surface_ = surface $ guiState mainstate
		(px,py) = position $ saveableState mainstate
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
	guiState <- initGui
	let state = makeDefaultState guiState
	tvms <- STM.atomically . STM.newTVar $ state
	drawScreen state
	eventHandler tvms

dataFile = "language.dat"

saveState :: MainState -> IO ()
saveState ms = B.writeFile dataFile . Serial.encode . saveableState $ ms

loadState :: MainState -> IO (MainState)
loadState ms = do
	bstring <- B.readFile dataFile
	case Serial.decode bstring of
		Left s -> do
			putStrLn $ "error reading/decoding file: " ++ s
			return ms
		Right newSaveableState -> return ms { saveableState = newSaveableState }

eventHandler :: STM.TVar MainState -> IO ()
eventHandler tvms = do
	mainState <- STM.atomically . STM.readTVar $ tvms
	let inputContext_ = inputContext $ guiState mainState
	e <- SDL.waitEvent
	case e of
		SDL.Quit -> return ()
		SDL.KeyDown (SDL.Keysym k _ _) -> case inputContext_ of
			NoContext -> let
					doDirectionKey f = do
						oldMainState <- STM.atomically . STM.readTVar $ tvms
						let newMainState = changeSaveable (changePosition f) oldMainState
						drawScreen newMainState
						STM.atomically $ STM.writeTVar tvms newMainState
						eventHandler tvms
				in case k of 
					SDL.SDLK_u -> doDirectionKey (\(x, y) -> (x, y - 1)) -- UP
					SDL.SDLK_e-> doDirectionKey (\(x, y) -> (x, y + 1)) -- DOWN
					SDL.SDLK_t -> doDirectionKey (\(x, y) -> (x + 1, y)) -- RIGHT
					SDL.SDLK_h -> doDirectionKey (\(x, y) -> (x - 1, y)) -- LEFT
					SDL.SDLK_i -> do -- INPUT
						STM.atomically $ do
							oldMainState <- STM.readTVar tvms
							STM.writeTVar tvms $ changeGui (changeContext (const IsInsert)) oldMainState
						eventHandler tvms
					SDL.SDLK_QUOTE -> do -- LOAD
						oldMainState <- STM.atomically . STM.readTVar $ tvms
						ms <- loadState oldMainState
						STM.atomically $ STM.writeTVar tvms ms
						drawScreen ms
						eventHandler tvms
					SDL.SDLK_l -> do -- SAVE
						ms <- STM.atomically $ STM.readTVar tvms
						saveState ms
						eventHandler tvms
					SDL.SDLK_ESCAPE -> return () -- EXIT
					otherwise -> eventHandler tvms
			IsInsert ->	let
					setNewIcon id = do
						newMainState <- STM.atomically $ do
							oldMainState <- STM.readTVar tvms
							STM.writeTVar tvms . setIconAtCursor id . changeGui (changeContext (const NoContext)) $ oldMainState
							STM.readTVar tvms
						drawScreen newMainState
						eventHandler tvms
					cancelInput = do
						STM.atomically $ do
							oldMainState <- STM.readTVar tvms
							STM.writeTVar tvms $ changeGui (changeContext (const NoContext)) oldMainState
						eventHandler tvms
				in case k of
					SDL.SDLK_i -> do -- clear icon
						newMainState <- STM.atomically $ do
							oldMainState <- STM.readTVar tvms
							STM.writeTVar tvms . removeIconAtCursor . changeGui (changeContext (const NoContext)) $ oldMainState
							STM.readTVar tvms
						drawScreen newMainState
						eventHandler tvms
					SDL.SDLK_u -> setNewIcon 1
					SDL.SDLK_e -> setNewIcon 2
					SDL.SDLK_ESCAPE -> cancelInput
					otherwise -> cancelInput
				
		otherwise -> eventHandler tvms
			
