module GuiState 
	( GuiState(..)
	, InputContext(..)
	, initGui
	, changeContext
	, addKeyToInputContext
	, iconGridSize
	, getFeedbackRect
	, changeGuiForNewIcon
	) where

import System.Random
import Control.Monad.State
import Graphics.UI.SDL as SDL
import Data.Map as Map
import Graphics.UI.SDL.TTF as TTF
import Graphics.UI.SDL.Primitives as Primitives
import Graphics.UI.SDL.Color as Color

import Icon as Icon
import qualified Point as P

data InputContext = IsInsert {
	keyBuffer :: [SDL.SDLKey]
} | IsTextInsert | NoContext 

data GuiState = GuiState {
	surface :: Surface,
	stdGen :: StdGen,
	font :: Font,
	icons :: IconSet,  -- eventually think about having user-created and preloaded icons
	inputContext :: InputContext
}

-- accessors
changeContext :: (InputContext -> InputContext) -> GuiState -> GuiState
changeContext f gs = gs { inputContext = f (inputContext gs) }

addKeyToInputContext :: SDL.SDLKey -> GuiState -> GuiState
addKeyToInputContext k = let
		addKey (IsInsert ks) = IsInsert (k:ks)
		addKey x = x
	in changeContext addKey

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

getRand :: Random a => (a,a) -> Control.Monad.State.State StdGen a
getRand bounds = do
   g <- get
   (x, ng) <- return $ randomR bounds g
   put ng
   return x

randomColors :: StdGen -> ([Color.Color], StdGen)
randomColors gen = let
      makePixel = do
         r <- getRand (0, 255)
         g <- getRand (0, 255)
         b <- getRand (0, 255)
         return $ Color r g b
   in runState (sequence $ repeat makePixel) gen

iconXSize = fromIntegral Icon.iconXSize
iconYSize = fromIntegral Icon.iconYSize

makeRandomIcon :: StdGen -> Int -> IO (Icon.Icon, StdGen)
makeRandomIcon stdGen customNum = do 
	surface <- SDL.createRGBSurfaceEndian [SDL.SWSurface] GuiState.iconXSize GuiState.iconYSize 32
	let	
		iconXSize_ = fromIntegral Icon.iconXSize
		iconYSize_ = fromIntegral Icon.iconYSize
		coords = [(cx,cy) | cx <- [0 .. iconXSize_], cy <- [0 .. iconYSize_]]
		colorToPixel surface (Color r g b) = SDL.mapRGB (SDL.surfaceGetPixelFormat surface) r g b
		(colors, newGen) = randomColors stdGen
		totalSize = GuiState.iconXSize * GuiState.iconYSize
		iconName = "Custom Function " ++ show customNum
	pixels <- sequence . take totalSize $ Prelude.map (colorToPixel surface) colors
	sequence_ $ zipWith (uncurry $ Primitives.pixel surface) coords pixels
	return (Icon.Icon iconName Nothing surface, newGen)

changeGuiForNewIcon :: GuiState -> IO GuiState
changeGuiForNewIcon guiState = do
	let newIconIndex = Icon.maxIcon (icons guiState) + 1
	(newIcon, newGen) <- makeRandomIcon (stdGen guiState) newIconIndex
	let newIcons = Icon.insert newIcon (icons guiState)
	return guiState { stdGen = newGen, icons = newIcons }

initGui = do
	SDL.init [InitEverything]
	TTF.init
	setVideoMode startGridXSize startGridYSize 32 [SDL.DoubleBuf]
	icons_ <- getDefaultIcons
	screen <- getVideoSurface
	stdGen <- getStdGen
	font_ <- openFont "/usr/share/fonts/truetype/freefont/FreeSans.ttf" 12
	return $ GuiState
		screen
		stdGen
		font_
		icons_
		NoContext
