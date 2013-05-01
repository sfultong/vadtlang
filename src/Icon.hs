module Icon 
	( Icon(..)
	, nullIcon
	, IconSet(IconSet)
	, Icon.insert
	, getByName
	, getByID
	, getID
	, getDefaultIcons
	) where

import System.Directory
import Control.Monad (liftM)
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Graphics.UI.SDL as SDL

iconDataDir = "/tmp/share/vooj-0.1/data"
functionIconDir = iconDataDir ++ "/functions"

data Icon = Icon {
   name :: String,
	offset :: Maybe SDL.Rect,
   pic :: SDL.Surface
}

nullIcon = Icon "" 

data IconSet = IconSet {
	iData :: IntMap.IntMap Icon,
	nameKey :: Map.Map String Int
}

empty :: IconSet
empty = IconSet IntMap.empty Map.empty

insert :: Icon -> IconSet -> IconSet
insert icon (IconSet iData nameKey) = let
		intKey = case IntMap.null iData of
			True -> 1
			False -> (+1) . fst $ IntMap.findMax iData
		iData_ = IntMap.insert intKey icon iData 
		nameKey_ = Map.insert (name icon) intKey nameKey
	in IconSet iData_ nameKey_ 

{-
insert :: String -> Maybe Rect -> SDL.Surface -> IconSet -> IconSet
insert name_ offset_ pic_ (IconSet iData nameKey) = let
		icon = Icon name_ offset_ pic_
		intKey = case IntMap.null iData of
			True -> 1
			False -> (+1) . fst $ IntMap.findMax iData
		iData_ = IntMap.insert intKey icon iData 
		nameKey_ = Map.insert name_ intKey nameKey
	in IconSet iData_ nameKey_ 
-}

getByName :: IconSet -> String -> Icon
getByName (IconSet iData nameKey) name_ = (iData IntMap.!) $ nameKey Map.! name_

getByID :: IconSet -> Int -> Icon
getByID (IconSet iData _) key = iData IntMap.! key

getID :: IconSet -> String -> Int
getID iconSet name = (nameKey iconSet) Map.! name

getTextIcons :: IO [Icon]
getTextIcons = let
		(startX, startY) = (10, 9)
		(sizeX, sizeY) = (11,13)
		offsets = iterate (\(x,y) -> (x + sizeX, y)) (startX, startY)
		rects = map ((\f -> f sizeX sizeY) . uncurry SDL.Rect) offsets
	in do
		fontMap <- SDL.loadBMP (iconDataDir ++ "/letters.bmp") 
		let icons = zipWith (\x y -> Icon (x : []) (Just y) fontMap) ['a' .. 'z'] rects
		return icons

getDefaultIcons :: IO IconSet
getDefaultIcons = do
	dirContents <- getDirectoryContents functionIconDir
	let
		imageNames = filter (\x -> x /= "." && x /= "..") $ dirContents
		fullImageNames = map ((functionIconDir ++) . ('/' :)) imageNames
		cleanedFunctionNames = map (takeWhile (/= '.')) imageNames
	images <- mapM SDL.loadBMP fullImageNames
	let funIcons = zipWith (flip Icon Nothing) cleanedFunctionNames images
	textIcons <- getTextIcons
	return . foldr Icon.insert Icon.empty $ (funIcons ++ textIcons)
	
