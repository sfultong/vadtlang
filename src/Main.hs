import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Primitives as PSDL
import qualified Control.Concurrent.STM as STM

import MainState
import GuiState
import DrawFeedbackGrid 
import Input
import qualified Compile
import qualified Icon 
import qualified Point as P

drawCursor :: MainState -> IO Bool
drawCursor mainState = let 
		(xSize, ySize) = iconGridSize $ guiState mainState
		surface_ = surface $ guiState mainState
		[centerX, centerY] = map (\x -> div x 2) [xSize, ySize]
		rect = SDL.Rect (centerX - 1) (centerY - 1) (centerX + Icon.iconXSize) (centerY + Icon.iconYSize)
	in PSDL.rectangle surface_ rect (SDL.Pixel (256 * 256 * 256 - 1)) 


drawScreen :: MainState -> IO ()
drawScreen mainstate = let
		(xSize, ySize) = iconGridSize $ guiState mainstate
		[centerX, centerY] = map (\x -> div x 2) [xSize, ySize]
		surface_ = surface $ guiState mainstate
		(px,py) = position $ saveableState mainstate
		xIcons = div xSize (Icon.iconXSize + 1)
		yIcons = div ySize (Icon.iconYSize + 1)
		positions = [(x, y) | -- positions of icons 
			x <- [px - div xIcons 2.. px + div xIcons 2], 
			y <- [py - div yIcons 2.. py + div yIcons 2]]
		clearScreen = SDL.fillRect surface_ Nothing (SDL.Pixel 0)
		drawIcon (x,y) = case hasIconAt mainstate (x,y) of
			True -> do 
				let drawIcon = getIconAt mainstate (x,y)
				SDL.blitSurface (Icon.pic drawIcon) (Icon.offset drawIcon) surface_ . Just $ SDL.Rect
					(centerX + (x - px) * (Icon.iconXSize + 1))
					(centerY + (y - py) * (Icon.iconYSize + 1))
					0 0
				return ()
			False -> return ()
	in do
		clearScreen
		mapM_ drawIcon positions
		drawCursor mainstate
		drawFeedback mainstate
		SDL.flip surface_

main = do
	guiState <- initGui
	let state = makeDefaultState guiState
	tvms <- STM.atomically . STM.newTVar $ state
	drawScreen state
	eventHandler tvms

compileFile = "out.hs"

eventHandler :: STM.TVar MainState -> IO ()
eventHandler tvms = do
	mainState <- STM.atomically . STM.readTVar $ tvms
	let inputContext_ = inputContext $ guiState mainState
	let changeMainState f = STM.atomically $ STM.readTVar tvms >>= (STM.writeTVar tvms . f) >> STM.readTVar tvms
	e <- SDL.waitEvent
	case e of
		SDL.Quit -> return ()
		SDL.KeyDown (SDL.Keysym k _ _) -> case inputContext_ of
			NoContext -> let
					doDirectionKey d = do
						oldMainState <- STM.atomically . STM.readTVar $ tvms
						let newMainState = changeSaveable (changePosition (P.moveByDirection d)) oldMainState
						drawScreen newMainState
						STM.atomically $ STM.writeTVar tvms newMainState
						eventHandler tvms
				in case k of 
					SDL.SDLK_u -> doDirectionKey P.Up
					SDL.SDLK_e-> doDirectionKey P.Down
					SDL.SDLK_t -> doDirectionKey P.Right
					SDL.SDLK_h -> doDirectionKey P.Left 
					SDL.SDLK_i -> do -- INPUT
						STM.atomically $ do
							oldMainState <- STM.readTVar tvms
							STM.writeTVar tvms $ changeGui (changeContext (const (IsInsert []))) oldMainState
						eventHandler tvms
					SDL.SDLK_d -> do -- TEXT INPUT
						STM.atomically $ do
							oldMainState <- STM.readTVar tvms
							STM.writeTVar tvms $ changeGui (changeContext (const IsTextInsert)) oldMainState
						eventHandler tvms
					SDL.SDLK_a -> do -- NEW FUNCTION
						ms <- STM.atomically $ STM.readTVar tvms
						newMainState <- newUserFunction ms
						drawScreen newMainState
						STM.atomically $ STM.writeTVar tvms newMainState
						eventHandler tvms
					SDL.SDLK_s -> do -- RETURN TO MAIN
						ms <- STM.atomically $ do
							oldMainState <- STM.readTVar tvms
							let newMainState = setActiveGrid 0 $ oldMainState
							STM.writeTVar tvms $ newMainState
							return newMainState
						drawScreen ms
						eventHandler tvms
					SDL.SDLK_n -> do -- GO TO FUNCTION DEFINITION
						ms <- STM.atomically $ do
							oldMainState <- STM.readTVar tvms
							let 
								gridNum = getIconID $ saveableState oldMainState
								newMainState = setActiveGrid gridNum $ oldMainState 
							STM.writeTVar tvms $ newMainState
							return newMainState
						drawScreen ms
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
					SDL.SDLK_z -> do -- COMPILE
						ms <- STM.atomically $ STM.readTVar tvms
						case Compile.compileMS ms of
							Just s -> writeFile compileFile ("main = " ++ s)
							Nothing -> return ()
						eventHandler tvms
					SDL.SDLK_ESCAPE -> return () -- EXIT
					otherwise -> eventHandler tvms
			IsInsert keyList -> let
					setNewIcon fun = do
						newMainState <- changeMainState $ changeGui (changeContext (const NoContext)) . (setIconAtCursor fun)
						drawScreen newMainState
						eventHandler tvms
					cancelInput = do
						STM.atomically $ do
							oldMainState <- STM.readTVar tvms
							STM.writeTVar tvms $ changeGui (changeContext (const NoContext)) oldMainState
						eventHandler tvms
				in case (k, decodeNumber (k:keyList)) of
					(SDL.SDLK_ESCAPE, _) -> cancelInput
					(_, Nothing) -> do
						STM.atomically $ do
							oldMainState <- STM.readTVar tvms
							STM.writeTVar tvms $ changeGui (addKeyToInputContext k) oldMainState
						eventHandler tvms
					(_, Just n) -> case n of
						0 -> do -- clear icon
							newMainState <- STM.atomically $ do
								oldMainState <- STM.readTVar tvms
								STM.writeTVar tvms . removeIconAtCursor . changeGui (changeContext (const NoContext)) $ oldMainState
								STM.readTVar tvms
							drawScreen newMainState
							eventHandler tvms
						n -> setNewIcon (n + Icon.startFunctionsIndex - 1) 
			IsTextInsert -> let 
					setNewIcon name = do
						newMainState <- changeMainState $ changeSaveable (changePosition (\(x, y) -> (x + 1, y))) . setIconAtCursorByName name 
						drawScreen newMainState
						eventHandler tvms
					cancelInput = do
						STM.atomically $ do
							oldMainState <- STM.readTVar tvms
							STM.writeTVar tvms $ changeGui (changeContext (const NoContext)) oldMainState
						eventHandler tvms
				in case k of
					SDL.SDLK_a -> setNewIcon "(:) 'a'"
					SDL.SDLK_b -> setNewIcon "(:) 'b'"
					SDL.SDLK_c -> setNewIcon "(:) 'c'"
					SDL.SDLK_d -> setNewIcon "(:) 'd'"
					SDL.SDLK_e -> setNewIcon "(:) 'e'"
					SDL.SDLK_f -> setNewIcon "(:) 'f'"
					SDL.SDLK_g -> setNewIcon "(:) 'g'"
					SDL.SDLK_h -> setNewIcon "(:) 'h'"
					SDL.SDLK_i -> setNewIcon "(:) 'i'"
					SDL.SDLK_j -> setNewIcon "(:) 'j'"
					SDL.SDLK_k -> setNewIcon "(:) 'k'"
					SDL.SDLK_l -> setNewIcon "(:) 'l'"
					SDL.SDLK_m -> setNewIcon "(:) 'm'"
					SDL.SDLK_n -> setNewIcon "(:) 'n'"
					SDL.SDLK_o -> setNewIcon "(:) 'o'"
					SDL.SDLK_p -> setNewIcon "(:) 'p'"
					SDL.SDLK_q -> setNewIcon "(:) 'q'"
					SDL.SDLK_r -> setNewIcon "(:) 'r'"
					SDL.SDLK_s -> setNewIcon "(:) 's'"
					SDL.SDLK_t -> setNewIcon "(:) 't'"
					SDL.SDLK_u -> setNewIcon "(:) 'u'"
					SDL.SDLK_v -> setNewIcon "(:) 'v'"
					SDL.SDLK_w -> setNewIcon "(:) 'w'"
					SDL.SDLK_x -> setNewIcon "(:) 'x'"
					SDL.SDLK_y -> setNewIcon "(:) 'y'"
					SDL.SDLK_z -> setNewIcon "(:) 'z'"
					SDL.SDLK_ESCAPE -> cancelInput
					otherwise -> cancelInput
		otherwise -> eventHandler tvms
			
