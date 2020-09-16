module Day10 where
import Data.HashSet (HashSet)
import Data.Function (on)
import Data.List (sortBy)
import Data.Foldable (maximumBy)
import qualified Data.HashSet as H

asteroidmap :: String -> HashSet (Int, Int)
asteroidmap s = result
    where
        (result, _, _) = foldl add (H.empty, 0, 0) s
        add (set, x, y) '#' = (H.insert (x,y) set, x+1, y)
        add (set, _, y) '\n' = (set, 0, y + 1)
        add (set, x, y) _ = (set, x+1, y)

-- Cast a ray from the origin to the target and return the points
-- the ray passes through.
--
-- The origin and the target points are not returned.
--
-- e.g.
--
-- cast (2, 0) (8, 3) == [(4, 1), (6, 2)]
--
-- ..o.........
-- ....*.......
-- ......*.....
-- ........t...
-- ............
cast :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
cast (x1, y1) (x2, y2) = travel (x1, y1)
    where
        dx = (x2 - x1) `div` (gcd (x2-x1) (y2-y1))
        dy = (y2 - y1) `div` (gcd (x2-x1) (y2-y1))
        travel (x, y)
            | (x, y) /= (x2, y2) = (newX, newY) : travel (newX, newY)
            | otherwise = []
            where
                newY = y + dy
                newX = x + dx

isBlocked :: HashSet (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
isBlocked set asteroid canSee = any (\p -> H.member p set && p /= asteroid && p /= canSee) (cast asteroid canSee)

isBlocked' :: HashSet (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
isBlocked' set asteroid canSee =
        any (`H.member` set) $ filter (/= asteroid) $ filter (/= canSee) $ cast asteroid canSee

delta :: (Int, Int) -> (Int, Int) -> (Double, Double)
delta (x1, y1) (x2, y2) = (posAngle, distance)
    where
        dx = fromIntegral (x1-x2)
        dy = fromIntegral (y1-y2)
        distance = sqrt (dx*dx + dy*dy)
        a = angle dy dx
        posAngle = if a < 0 then 2*pi + a else a

angle :: Double -> Double -> Double
angle opp adj = -(atan2 adj opp)

part2 :: String  -> [(Int, Int)]
part2 s = take 200 $ blast initialSet
    where 
        -- orderedAstroids = sortBy (compare `on` angleOf) astroids ++ newSet asteroids
        blast :: HashSet (Int,Int) -> [(Int, Int)]
        blast asteroidSet
            | H.null asteroidSet = []
            | otherwise = sortBy (compare `on` angleOf) (H.toList astroids) ++ blast (asteroidSet `H.difference` astroids)
            where 
                astroids = detected asteroidSet laserGunPost

        -- subtract old removed = filter (`notElem` removed) old
        angleOf = fst . delta laserGunPost 
        laserGunPost = snd $ part1 s
        initialSet = H.delete laserGunPost $ asteroidmap s 

part1 :: String -> (Int, (Int, Int))
part1 s = answer
    where
        answer = maximumBy (compare `on` fst) $ H.map (\x -> (countDetected set x, x)) set
        set = asteroidmap s

countDetected :: HashSet (Int, Int) -> (Int, Int) -> Int
countDetected set a = length [() | s <- H.toList set, not $ isBlocked set a s, a /= s]

detected :: HashSet (Int, Int) -> (Int, Int) -> HashSet (Int, Int)
-- detected set a = [s | s <- H.toList set, not $ isBlocked set a s, a /= s]
detected set a = H.filter (\s -> not $ isBlocked set a s && a /= s) set
-- ones xxxx = 1 : ones (xxxx - 1)



-- (x1, y1) = (0, 0)
-- (x2, y2) = (9, 3)

-- cast (0, 0) (9, 3) = [(0, 0), (3, 1), (6, 2), (9, 3)]

{-
#.........
...A......
...B..a...
.EDCG....a
..F.c.b...
.....c....
..efd.c.gb
.......c..
....f...c.
...e..d..c
-}
-- A = [3,1] * n
-- a = [6,2], [9,3]

eg1 :: String
eg1 = ".#..#\n\
      \.....\n\
      \#####\n\
      \....#\n\
      \...##"

eg3 :: String
eg3 = ".#..#..###\n\
      \####.###.#\n\
      \....###.#.\n\
      \..###.##.#\n\
      \##.##.#.#.\n\
      \....###..#\n\
      \..#.#..#.#\n\
      \#..#.#.###\n\
      \.##...##.#\n\
      \.....#.#.."

input :: String
input = ".###.###.###.#####.#\n\
        \#####.##.###..###..#\n\
        \.#...####.###.######\n\
        \######.###.####.####\n\
        \#####..###..########\n\
        \#.##.###########.#.#\n\
        \##.###.######..#.#.#\n\
        \.#.##.###.#.####.###\n\
        \##..#.#.##.#########\n\
        \###.#######.###..##.\n\
        \###.###.##.##..####.\n\
        \.##.####.##########.\n\
        \#######.##.###.#####\n\
        \#####.##..####.#####\n\
        \##.#.#####.##.#.#..#\n\
        \###########.#######.\n\
        \#.##..#####.#####..#\n\
        \#####..#####.###.###\n\
        \####.#.############.\n\
        \####.#.#.##########."


tamainput :: String
tamainput = ".###.###.###.#####.#\n\
        \#####.##.###..###..#\n\
        \.#...####.###.######\n\
        \######.###.####.####\n\
        \#####..###..########\n\
        \#.##.###########.#.#\n\
        \##.###.######..#.#.#\n\
        \.#.##.###.#.####.###\n\
        \##..#.#.##.#########\n\
        \###.#######.###..##.\n\
        \###.###.##.##..####.\n\
        \.##.####.##########.\n\
        \#######.##.###.#####\n\
        \#####.##..####.#####\n\
        \##.#.#####.##.#.#..#\n\
        \###########.#######.\n\
        \#.##..#####.#####..#\n\
        \#####..#####.###.###\n\
        \####.#.############.\n\
        \####.#.#.##########."


input2 :: String
input2 =
        ".#..##.###...#######\n\
        \##.############..##.\n\
        \.#.######.########.#\n\
        \.###.#######.####.#.\n\
        \#####.##.#.##.###.##\n\
        \..#####..#.#########\n\
        \####################\n\
        \#.####....###.#.#.##\n\
        \##.#################\n\
        \#####.##.###..####..\n\
        \..######..##.#######\n\
        \####.##.####...##..#\n\
        \.#####..#.######.###\n\
        \##...#.##########...\n\
        \#.##########.#######\n\
        \.####.#.###.###.#.##\n\
        \....##.##.###..#####\n\
        \.#.#.###########.###\n\
        \#.#.#.#####.####.###\n\
        \###.##.####.##.#..##"
