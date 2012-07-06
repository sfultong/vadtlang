module Icon 
	( Icon(..)
	, nullIcon
	, IconSet(IconSet)
	, Icon.insert
	, getByName
	, getByID
	, getDefaultIcons
	) where

import System.Directory
import Control.Monad (liftM)
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Graphics.UI.SDL as SDL

data Icon = Icon {
   name :: String,
   pic :: SDL.Surface
}

nullIcon = Icon "" 

data IconSet = IconSet {
	iData :: IntMap.IntMap Icon,
	nameKey :: Map.Map String Int
}

empty :: IconSet
empty = IconSet IntMap.empty Map.empty

insert :: String -> SDL.Surface -> IconSet -> IconSet
insert name_ pic_ (IconSet iData nameKey) = let
		icon = Icon name_ pic_
		intKey = case IntMap.null iData of
			True -> 1
			False -> (+1) . fst $ IntMap.findMax iData
		iData_ = IntMap.insert intKey icon iData 
		nameKey_ = Map.insert name_ intKey nameKey
	in IconSet iData_ nameKey_ 

getByName :: IconSet -> String -> Icon
getByName (IconSet iData nameKey) name_ = (iData IntMap.!) $ nameKey Map.! name_

getByID :: IconSet -> Int -> Icon
getByID (IconSet iData _) key = iData IntMap.! key

getDefaultIcons :: IO IconSet
getDefaultIcons = do
	dirContents <- getDirectoryContents "/tmp/share/vooj-0.1/data/functions"
	let imageNames = map ("/tmp/share/vooj-0.1/data/functions/" ++) . filter (\x -> x /= "." && x /= "..") $ dirContents
	images <- mapM SDL.loadBMP imageNames
	return . foldr (uncurry Icon.insert) Icon.empty $ zip imageNames images
	
