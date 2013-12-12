{-# LANGUAGE DeriveGeneric #-}

module MainState 
	( MainState(..)
	, SaveableState(..)
	, makeDefaultState
	, changePosition
	, changeCurrentGrid
	, changeSaveable
	, changeGui
	, hasIconAt
	, hasIconAtSS
	, getIconAt
	, getIconAtCursor
	, getIconID
	, setIconAt
	, setIconAtCursor
	, setIconAtCursorByName
	, removeIconAt
	, removeIconAtCursor
	, newUserFunction
	, hasUserFunctionSS
	, setActiveGridSS
	, setActiveGrid
	, loadState
	, saveState
	) where

import GHC.Generics
import Data.Serialize
import qualified Data.Map as Map

import Icon as Icon
import qualified GuiState as GUI
import qualified Point as P
import qualified Data.ByteString as B
import qualified Data.Serialize as Serial

data SaveableState = SaveableState {
   position :: P.Point,
	currentGrid :: Int,
   grids ::  Map.Map Int (Map.Map P.Point Int)
} deriving Generic

instance Serialize SaveableState

data MainState = MainState {
	saveableState :: SaveableState,
	guiState :: GUI.GuiState
} 

-- accessors
changeCurrentGrid :: (Map.Map P.Point Int -> Map.Map P.Point Int) -> SaveableState -> SaveableState
changeCurrentGrid f ss = ss { grids = Map.adjust f (currentGrid ss) (grids ss) }

changeSaveable :: (SaveableState -> SaveableState) -> MainState -> MainState
changeSaveable f ms = ms { saveableState = f (saveableState ms) } 

changePosition :: (P.Point -> P.Point) -> SaveableState -> SaveableState
changePosition f ss = let
		newPosition = f (position ss) 
	in case newPosition of
		(0,0) -> ss
		_ -> ss { position = newPosition }

changeGui :: (GUI.GuiState -> GUI.GuiState) -> MainState -> MainState
changeGui f ms = ms { guiState = f (guiState ms) }

grid :: SaveableState -> Map.Map P.Point Int
grid ss = grids ss Map.! currentGrid ss

-- setup
testInitMap = Map.fromList [((0,0), Icon.startIconIndex)]

testInitGrids = Map.fromList [(0, testInitMap)]

makeDefaultState :: GUI.GuiState -> MainState
makeDefaultState = MainState (SaveableState (0,1) 0 testInitGrids)

-- exported functions
hasIconAt :: MainState -> P.Point -> Bool
hasIconAt mainState point = Map.member point . grid . saveableState $ mainState

hasIconAtSS :: SaveableState -> P.Point -> Bool
hasIconAtSS ss point = Map.member point . grid $ ss

getIconAt :: MainState -> P.Point -> Icon.Icon
getIconAt mainState point = let
		_icons = GUI.icons . guiState $ mainState
		_grid = grid . saveableState $ mainState
	in Icon.getByID _icons $ _grid Map.! point

getIconAtCursor :: MainState -> Icon.Icon
getIconAtCursor mainState = getIconAt mainState . position . saveableState $ mainState

getIconID :: SaveableState -> Int
getIconID ss = (grid ss) Map.! (position ss)

setIconAt :: P.Point -> Int -> MainState -> MainState 
setIconAt point id mainState = case hasID id . GUI.icons $ guiState mainState of -- check if this ID is valid
	False -> mainState -- if not valid id, don't change state
	True -> changeSaveable (changeCurrentGrid (Map.insert point id)) mainState
	
setIconAtCursor :: Int -> MainState -> MainState 
setIconAtCursor id mainState = setIconAt (position $ saveableState mainState) id mainState 

setIconAtCursorByName :: String -> MainState -> MainState
setIconAtCursorByName name mainState = let
		iconSet = GUI.icons . guiState $ mainState
		id = getID iconSet name
	in setIconAtCursor id mainState

removeIconAt :: P.Point -> MainState -> MainState 
removeIconAt point = changeSaveable (changeCurrentGrid (Map.delete point))

removeIconAtCursor :: MainState -> MainState
removeIconAtCursor mainState = removeIconAt (position $ saveableState mainState) mainState 

newUserFunction :: MainState -> IO MainState
newUserFunction mainState = do
	newGuiState <- GUI.changeGuiForNewIcon $ guiState mainState
	let
		newIconIndex = Icon.maxIcon . GUI.icons $ newGuiState
		newGrid = Map.fromList [((0,0), newIconIndex)]
		newGrids = Map.insert newIconIndex newGrid . grids $ saveableState mainState
		newSaveableState = (saveableState mainState) { currentGrid = newIconIndex, grids = newGrids, position = (0,1) }
	return mainState { guiState = newGuiState, saveableState = newSaveableState }

hasUserFunctionSS :: Int -> SaveableState -> Bool
hasUserFunctionSS id ss = Map.member id . grids $ ss

-- TODO - consider finding the correct outlet from the function definition icon, and setting the position to that position
-- that would avoid errors in compiling where it expects the outlet to be below the icon 
setActiveGridSS :: Int -> SaveableState -> SaveableState
setActiveGridSS gridNum ss = case hasUserFunctionSS gridNum ss of
	True -> ss { currentGrid = gridNum, position = (0,1) } 
	False -> ss

setActiveGrid :: Int -> MainState -> MainState
setActiveGrid id = changeSaveable $ setActiveGridSS id

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

dataFile = "language.dat"
