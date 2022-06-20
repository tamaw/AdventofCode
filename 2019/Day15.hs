{-# LANGUAGE ViewPatterns, OverloadedStrings #-}

import Control.Monad (unless)
import IntMachine
import Data.Maybe
import Control.Monad (forM_)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Read (signed, decimal)
import qualified Data.HashMap.Strict as H
import Data.HashMap.Strict (HashMap, singleton, empty)
import qualified Data.HashMap.Strict as H
import System.IO (hSetBuffering, stdin, BufferMode (..))
import Debug.Trace (traceShowId)

fromInput :: T.Text -> Int
fromInput "n" = 1
fromInput "s" = 2
fromInput "w" = 3
fromInput "e" = 4
fromInput x = error ("Invalid input: " ++ show x)

data Output = Wall | Floor | Oxygen
    deriving (Show, Eq)

toOutput :: Int -> Output
toOutput 0 = Wall
toOutput 1 = Floor
toOutput 2 = Oxygen
toOutput x = error ("Invalid output: " ++ show x)

part0 :: IO Int
part0 = do
    hSetBuffering stdin LineBuffering
    input <- T.readFile "src/Day15Input.txt"
    let mem = parseMem input
    mainloop 0 (runIncremental mem)
    where
        mainloop stepCount stepResult =
            case stepResult of
                Done _ -> return stepCount
                NeedsMoreInput outputs continue -> do
                    let statuses = map toOutput outputs
                    let nextStepCount = case statuses of
                                            [] -> stepCount
                                            [Wall] -> stepCount
                                            [_] -> stepCount+1
                                            _ -> error ("413 Payload too large: " ++ show statuses)
                    print $ show statuses ++ show nextStepCount
                    moveInput <- T.getLine
                    if T.null moveInput
                        then return nextStepCount
                        else do
                            let move = fromInput moveInput
                            mainloop nextStepCount $ continue [move]

theRealPart1 :: IO Int
theRealPart1 = do
    hSetBuffering stdin LineBuffering
    input <- T.readFile "day15.txt"
    let mem = parseMem input
        (chart, count) = findBf mem
    draw chart
    return $ fromMaybe 0 count

findBf :: Mem -> (HashMap (Int, Int) Output , Maybe Int)
findBf mem = search (runIncremental mem) (0, 0) (empty, 0)
    where
        search :: StepResult -> (Int, Int) -> (HashMap (Int, Int) Output, Int) -> (HashMap (Int, Int) Output, Maybe Int)
        search _ position (chart, count) | H.member position chart = (chart, Nothing)
        search stepResult position@(x, y) (chart, count) =
            case stepResult of
                Done _ -> undefined
                NeedsMoreInput outputs continue ->
                    let statuses = map toOutput outputs
                        updatedChart =
                            case statuses of
                                [] -> chart
                                [status] -> H.insert position status chart
                                _ -> error ("413 Payload too large: " ++ show statuses)
                    in case statuses of
                          [Oxygen] -> (updatedChart, Just $ count)
                          [Wall] -> (updatedChart, Nothing)
                          list | list == [Floor] || list == [] ->
                                    let
                                        (northChart, northCount) = search (continue [1]) (x, y + 1) (updatedChart, count + 1)
                                        (southChart, southCount) = search (continue [2]) (x, y - 1) (updatedChart, count + 1)
                                        (westChart, westCount) = search (continue [3]) (x - 1, y) (updatedChart, count + 1)
                                        (eastChart, eastCount) = search (continue [4]) (x + 1, y) (updatedChart, count + 1)
                                        nextCount = northCount `combine` southCount `combine` eastCount `combine` westCount
                                        nextChart = northChart <> southChart <> eastChart <> westChart -- <> == mappend
                                    in
                                        (nextChart, nextCount)
                               | otherwise -> error ("413 Payload too large: " ++ show statuses)

combine :: Maybe Int -> Maybe Int -> Maybe Int
combine (Just a) (Just b) = Just (min a b)
combine x Nothing = x
combine Nothing x = x

draw :: HashMap (Int, Int) Output -> IO ()
draw chart = do
    forM_ [bottom,bottom-1..top] $ \y -> do
        forM_ [left..right] $ \x -> do
            case H.lookup (x, y) chart of
                Just Oxygen -> putStr "O"
                Just Wall -> putStr "#"
                Just Floor | x == 0 && y == 0 -> putStr "X"
                           | otherwise -> putStr "."
                Nothing -> putStr " "
        putStr "\n"
    where
        xs = map fst (H.keys chart)
        ys = map snd (H.keys chart)
        left = minimum xs
        right = maximum xs
        top = minimum ys
        bottom = maximum ys
            -- found oxygen
            -- no O2,
                -- try N
                -- if not found, S
                -- if not found try E
                -- if not found try W

            -- 1. given chart count
            -- 2. test north if north is not already in the chart
            -- 3. test east ditto
            -- 4. test w ditto
            -- 5. test s
            -- 7. if north was floor -> recursive call north updated chart :: Maybe (Chart, Int)
            -- 8. if east was floor -> recursive call east updated chart
            -- ...
            -- combine charts of 7-10
            -- combine counts (min) of 7-10
            -- return

            -- 0. given originalChart originalCount
            -- 1. test "here"
            -- 2. update chart
            -- 3. if here was floor ->
                -- 4. recursive call n with updated chart, else (orignalChart, originalCount)
                -- 5. recursive call s with updated chart, else (orignalChart, originalCount)
                -- 6. recursive call e with updated chart, else (orignalChart, originalCount)
                -- 7. recursive call w with updated chart, else (orignalChart, originalCount)
                -- combine charts of 4-7
                -- combine counts (min) of 4-7
                -- return
            -- 3b. else here was not floor ->
                -- return (orignalChart, originalCount)


-- search :: StepResult -> (Int, Int) -> Int -> HashMap (Int, Int) Output -> (HashMap (Int, Int) Output, Int)
-- search :: StepResult -> (Int, Int) -> Int -> State (HashMap (Int, Int) Output) Int

parseMem :: T.Text -> [Int]
parseMem text = map (parseInt) $ T.splitOn "," text

parseInt :: T.Text -> Int
parseInt input =
    case signed decimal input of
        Right (value, _) -> value
        _ -> error "422 unprocessable entity"

{-
### ####### ################### #######
#...#.......#...................#.......#
#.#.#.###.#.#.#######.#########.#######.#
#.#...#.#.#.#.#...#...#.......#...#.....#
#.#####.#.#.#.#.#.#.###.###### ##.#.###.#
#...#...#.#.#.#.#...#.........#...#.#...#
 ##.#.###.#.###.#############.#.###.#.##
#...#.#...#.#...#.......#.....#.....#.#.#
#.###.#.###.#.#.#.#####.#.###########.#.#
#.....#.#.....#.#.#...#.#...........#.#.#
 ####.#.#######.#.###.#.#.#######.###.#.#
#.#...#...#...#.#.#...#.#.......#...#...#
#.#.#####.#.#.###.#.###.#######.###.###.#
#...#...#...#...#.#.#...#.....#.#.#...#.#
#.###.#########.#.#.#.###.#.###.#.###.#.#
#.....#.........#.#.#...#.#...#.#.#...#.#
 ####.#.###.#####.#.###.## ##.#.#.#.###.#
#.#...#.#...#...#.#...#...#...#...#.....#
#.#.###.#####.#.#.###.###.#.#.###.######
#.#.#.#.......#...#...#...#.#.#...#.....#
#.#.#.#############.###.###.###.###.###.#
#.#.......#.........#X#.#.....#.#...#.#.#
#.#######.#.#######.#.#.###.#.#.#.###.#.#
#.#.....#...#...#...#.#...#.#...#...#.#.#
#.#.#.#######.###.###.###.#.#### ##.#.#.#
#...#.#.........#.#.#...#.#.....#...#...#
#.###.#.#######.#.#.###.#.#####.#.###.##
#.#.#...#.#...#...#...#.#.....#.#.#.#...#
#.#.#####.#.#.#####.#.#.#.###.#.#.#.###.#
#.#.........#.....#.#.#.#.#.#.#...#.....#
#.###########.#.#.#.###.#.#.#.#####.####
#...........#.#.#.#.#...#...#...#.#.#...#
 ##########.###.#.#.#.###### ##.#.#.#.##
#.........#...#.#...#.#.....#...#...#...#
#.#######.###.#.#####.#.###.#.###.###.#.#
#...#.#.....#...#.....#.#.#.#.#...#O..#.#
 ##.#.#.#######.#.#####.#.#.#.#.#######.#
#...#.#.#.....#.#.#.....#...#.#.#.....#.#
#.###.#.#.#.###.#.###.###.###.#.###.#.#.#
#.....#...#.....#.....#.......#.....#...#
 ##### ### ##### ##### ####### ##### ###
-}

{-
 ############# ########### ####### #####
#.............#...........#.......#.....#
#.#####.#####.#.#########.#.###.###.###.#
#.....#.#...#.#.#...#...#.#...#.....#...#
 ####.#.#.#.###.#.#.###.#.###.#######.##
#.#...#...#...#...#.....#...#.......#...#
#.#.#########.###.#######.#.######## ##.#
#.#.#*......#...#.#.....#.#.........#...#
#.#.#####.#.#.###.#.###.#####.#####.#.#.#
#.#.......#.#.....#...#.....#.....#.#.#.#
#.#########.#########.#####.#####.#.#.##
#.........#.....#.....#.#...#...#.#.#...#
#.#######.###.###.#####.#.###.#.###.#.#.#
#.#.........#.#...#.....#.#...#.....#.#.#
#.#.###.#####.#.###.###.#.#.## ########.#
#.#...#.#.....#.#.....#.#.#...#...#.....#
 ####.#.#.#####.#.###.###.###.#.#.#.#.##
#.....#.#.#.#...#...#...#.....#.#.#.#...#
#.#####.#.#.#.###.#####.#######.#.#####.#
#.#.#...#...#.#...#...#.....#...#.......#
#.#.#.#####.#.###.#.#.#.#####.#.#.#####.#
#.#.......#.#...#.#.#.#.....#.#.#.#...#.#
#.#########.###.#.#.#######.#.#.###.#.#.#
#...#.........#.#.#.....#...#.#.....#.#.#
#.#.#.#########.#.#####.#.###.#######.#.#
#.#.....#...#...#.#...#.#.......#.....#.#
#.#######.#.#.###.###.#.#.#######.#####.#
#...#.....#...#.......#.#...#...#...#.#.#
 ##.#.#########.#####.#.#.###.#.#.#.#.#.#
#.#.#.#.....#.#.#...#.#.#.#...#.#.#.#...#
#.#.#.#.###.#.#.#.#.###.###.###.#.#.#.##
#...#...#.#.#...#.#.....#...#.#.#.#.#...#
#.## ####.#.###.#.#######.###.#.###.####
#...#.....#...#.#.#.......#...#...#.#...#
 ##.#.###.###.###.#.###.###.#.###.#.#.#.#
#.#.#.#...#.#.....#.#...#...#.#...#...#.#
#.#.#.#.###.###### ##.#####.###.#######.#
#.#.#.#.#.........#...#.....#...#...#...#
#.#.#.#.#####.###.#.###.#####.###.#.#.#.#
#.....#.......#.....#.............#...#.#
 ##### ####### ##### ############# ### #
-}