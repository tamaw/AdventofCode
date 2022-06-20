{-# LANGUAGE OverloadedStrings #-}

import IntMachine
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Read (signed, decimal)
import Data.Foldable (forM_)
import Data.List (maximumBy)
import Data.Function (on)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H

readProgram :: FilePath -> IO Mem
readProgram filePath = do
    input <- T.readFile filePath
    return $ map parseInt $ T.splitOn "," input

parseInt :: T.Text -> Int
parseInt s =
    case signed decimal s of
        Right (n,_) -> n
        Left _ -> error "not a number"

part1 :: Int
part1 = countBlocks $ createMap output
    where
        (_, output) = run [] $ readProgram "day13.txt" -- IntMachine needs to accept IO

part2 :: IO ()
part2 = runGame playAI

runGame :: (HashMap (Int, Int) Int -> IO String) -> IO ()
runGame interact = go H.empty $ runIncremental (readProgram "day13.txt")
    where
        go :: HashMap (Int, Int) Int -> StepResult -> IO()
        go state stepResult =
            case stepResult of
                Done (_, output) -> do
                    let updated = updateMap output state
                    draw $ updateMap output state
                    print $ H.lookupDefault 0 (-1, 0) updated
                NeedsMoreInput output continue -> do
                    let updated = updateMap output state
                    nextInput <- interact updated
                    go updated $ continue [decodeInput nextInput]

playInteractive :: HashMap (Int, Int) Int -> IO String
playInteractive balls = do
    draw balls
    getLine

playAI :: HashMap (Int, Int) Int -> IO String
playAI m = return $ if ballX > paddleX then "d" else if ballX < paddleX then "a" else ""
    where
        ((ballX, _), _) = find (\((x,y), t) -> t == 4) m
        ((paddleX, _), _) = find (\((x,y), t) -> t == 3) m

find :: (((Int, Int), Int) -> Bool) -> HashMap (Int, Int) Int -> ((Int, Int), Int)
find pred hm = head $ filter pred $ H.toList hm

updateMap :: [Int] -> HashMap (Int, Int) Int -> HashMap (Int, Int) Int
updateMap [] a = a
updateMap (x:y:c:rest) a = updateMap rest (H.insert (x,y) c a)
updateMap l _ = error $ "unexpected number of outputs. remainder: " ++ show l

decodeInput :: String -> Int
decodeInput "a" = -1
decodeInput "d" = 1
decodeInput _ = 0

draw :: HashMap (Int, Int) Int -> IO()
draw hm = do
     forM_ [0..height hm] $ \y -> do
        forM_ [0..width hm] $ \x -> do
            let
                tile = H.lookupDefault 0 (x,y) hm
            case tile of
                0  -> putChar ' '
                1  -> putChar '█'
                2  -> putChar '*'
                3  -> putChar '-'
                4  -> putChar 'o'
            -- putStr $ show tile
        putStr "\n"

width, height :: HashMap (Int, Int) Int -> Int
height m = maximum $ map snd $ H.keys m
width m = maximum $ map fst $ H.keys m


createMap :: [Int] -> HashMap (Int, Int) Int
createMap ins = go ins mempty
    where
        go :: [Int] -> HashMap (Int, Int) Int -> HashMap (Int, Int) Int
        go [] a = a
        go (x:y:c:rest) a = go rest (H.insert (x,y) c a)
        go l _ = error $ "unexpected number of outputs. remainder: " ++ show l

countBlocks :: HashMap (Int, Int) Int -> Int
countBlocks m = H.size $ H.filter (\t -> t == 2) m