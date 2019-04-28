{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

import           Data.List
import qualified Data.Map.Strict                      as M
import           Data.Ord (comparing)
import qualified Graphics.Gloss.Data.Point.Arithmetic as P
import           Graphics.Gloss.Interface.Pure.Game

type ColorIx = Int
type Mean = (ColorIx, Point)

data State = State { clusters :: M.Map Mean [Point], running :: Bool } deriving Show

colors :: [Color]
colors = [red, green, blue, yellow, cyan, magenta, rose, violet, azure, aquamarine, chartreuse, orange]

pointRadius :: Float
pointRadius = 7

main :: IO ()
main = play FullScreen black 2 initialState draw handleInput (const step)

initialState = State { clusters = M.empty, running = False }

draw :: State -> Picture
draw State {..} = M.foldMapWithKey (\(col, (mx, my)) points -> Color (colors !! col) $
    Translate mx my (rectangleSolid (pointRadius * 2) (pointRadius * 2)) <> foldMap (\(x, y) -> Translate x y $ circleSolid pointRadius) points) clusters

handleInput :: Event -> State -> State
handleInput (EventKey key Down _ pos) State {..}
    | SpecialKey KeySpace <- key = State { running = not running, .. }
    | MouseButton LeftButton <- key = State { clusters =
        let points = concat $ M.elems clusters
            points' = case find ((< pointRadius) . distance pos) points of
                Just p -> delete p points
                Nothing -> if M.null clusters then points else pos : points
        in  assignClosest points' $ M.keys clusters
        , .. }
    | MouseButton RightButton <- key = State { clusters =
        let means = M.keys clusters
            means' = case find ((< pointRadius) . distance pos . snd) means of
                Just m -> if length means == 1 then means else delete m means
                Nothing -> case findIndex (\col -> all ((/= col) . (colors !!) . fst) means) colors of
                    Just colIx -> (colIx, pos) : means
                    Nothing -> means
        in  assignClosest (concat $ M.elems clusters) means'
        , .. }
    | Char 'c' <- key = State { clusters = M.empty, .. }
handleInput _ state = state

step :: State -> State
step State {..}
    | not running || M.null clusters = State {..}
    | otherwise = State
        { clusters = assignClosest (concat $ M.elems clusters) (updateMeans clusters)
        , .. }

assignClosest :: [Point] -> [Mean] -> M.Map Mean [Point]
assignClosest points means = M.fromListWith (++) $
    map (, []) means ++ if null means then [] else map (\p -> (minimumBy (comparing $ distance p . snd) means, [p])) points

updateMeans :: M.Map Mean [Point] -> [Mean]
updateMeans clusters
    | M.null clusters = M.keys clusters
    | otherwise = map (\((col, mean), points) -> (col, if null points then mean else findMean points)) $ M.assocs clusters

distance :: Point -> Point -> Float
distance (x1, y1) (x2, y2) = sqrt $ (x2 - x1)^2 + (y2 - y1)^2

findMean :: [Point] -> Point
findMean points = (1 / fromIntegral (length points)) P.* foldr1 (P.+) points
