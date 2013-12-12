module Input 
	( keyMap
	, toSDL
	, isFinalizingKey
	, decodeNumber
	) where

import qualified Data.Map as Map
import qualified Graphics.UI.SDL as SDL
import Data.Maybe

keyMap =
	--hutenosadi
	[ ('h', SDL.SDLK_h)
	, ('u', SDL.SDLK_u)
	, ('t', SDL.SDLK_t)
	, ('e', SDL.SDLK_e)
	, ('n', SDL.SDLK_n)
	, ('o', SDL.SDLK_o)
	, ('s', SDL.SDLK_s)
	, ('a', SDL.SDLK_a)
	, ('d', SDL.SDLK_d)
	, ('i', SDL.SDLK_i)
	--fygpc.r,l'
	, ('f', SDL.SDLK_f)
	, ('y', SDL.SDLK_y)
	, ('g', SDL.SDLK_g)
	, ('p', SDL.SDLK_p)
	, ('c', SDL.SDLK_c)
	, ('.', SDL.SDLK_PERIOD)
	, ('r', SDL.SDLK_r)
	, (',', SDL.SDLK_COMMA)
	, ('l', SDL.SDLK_l)
	, ('\'', SDL.SDLK_QUOTE)
	--bxmkwjvqz;
	, ('b', SDL.SDLK_b)
	, ('x', SDL.SDLK_x)
	, ('m', SDL.SDLK_m)
	, ('k', SDL.SDLK_k)
	, ('w', SDL.SDLK_w)
	, ('j', SDL.SDLK_j)
	, ('v', SDL.SDLK_v)
	, ('q', SDL.SDLK_q)
	, ('z', SDL.SDLK_z)
	, (';', SDL.SDLK_SEMICOLON)
	--6574839201
	, ('6', SDL.SDLK_6)
	, ('5', SDL.SDLK_5)
	, ('7', SDL.SDLK_7)
	, ('4', SDL.SDLK_4)
	, ('8', SDL.SDLK_8)
	, ('3', SDL.SDLK_3)
	, ('9', SDL.SDLK_9)
	, ('2', SDL.SDLK_2)
	, ('0', SDL.SDLK_0)
	, ('1', SDL.SDLK_1)
	]

toSDL :: Char -> SDL.SDLKey
toSDL c = case lookup c keyMap of
	Just v -> v
	Nothing -> SDL.SDLK_ESCAPE

vigesimalize :: SDL.SDLKey -> Maybe Int
vigesimalize k = let
		numMap = zipWith (\(_, k) n -> (k, n)) keyMap [0..]
	in lookup k numMap

isFinalizingKey :: SDL.SDLKey -> Bool
isFinalizingKey k = case vigesimalize k of
	Nothing -> False
	Just v -> v < 20

decodeNumber :: [SDL.SDLKey] -> Maybe Int
decodeNumber [] = Nothing
decodeNumber (k:ks) = case isFinalizingKey k of
	False -> Nothing
	True -> let
			filteredKeys = catMaybes $ map vigesimalize ks
			makeNum = foldr (+) 0 . zipWith (*) (iterate (*20) 20)
		in vigesimalize k >>= (return . (+ makeNum filteredKeys))

