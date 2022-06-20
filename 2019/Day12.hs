-- <x=-1, y=0, z=2>
-- <x=2, y=-10, z=-7>
-- <x=4, y=-8, z=8>
-- <x=3, y=5, z=-1>

-- <x=-10, y=-13, z=7>
-- <x=1, y=2, z=1>
-- <x=-15, y=-3, z=13>
-- <x=3, y=7, z=-4>

-- <x=3, y=2, z=-6>
-- <x=-13, y=18, z=10>
-- <x=-8, y=-1, z=13>
-- <x=5, y=10, z=4>

newtype Position = Position (Int, Int, Int)
    deriving Show
newtype Velocity = Velocity (Int, Int, Int)
    deriving Show
newtype Moon = Moon (Position, Velocity)
    deriving Show

applyGravity :: [Moon] -> Moon -> Moon
applyGravity ms m = foldl ag m ms
    where
        ag :: Moon -> Moon -> Moon
        ag (Moon(p1@(Position(x1, y1, z1)), Velocity(vx, vy, vz))) (Moon(Position (x2, y2, z2), _)) = Moon (p1, Velocity(vx + x', vy + y', vz + z'))
            where
                x' = applyGravityTo x1 x2
                y' = applyGravityTo y1 y2
                z' = applyGravityTo z1 z2

allGravity :: [Moon] -> [Moon]
allGravity ms = map (applyGravity ms) ms

applyGravityTo :: Int -> Int -> Int
applyGravityTo p1 p2
                    | (p1 < p2) = 1
                    | (p1 > p2) = -1
                    | otherwise = 0

applyPosition :: [Moon] -> [Moon]
applyPosition ms = map apply ms
    where
        apply :: Moon -> Moon
        apply (Moon(Position(x, y, z), v@(Velocity(vx, vy, vz)))) = Moon(Position(x+vx, y+vy, z+vz), v)

part2 :: [Moon] -> (Int, Int, Int, Int)
part2 originalMoons = (countX, countY, countZ, lcm countX (lcm countY countZ) )
    where
        steps :: [[Moon]]
        steps = iterate stepOnce originalMoons

        countUntil test = length $ takeWhile (not . test) $ drop 1 steps
        countX = succ $ succ $ countUntil moonsIsOriginalX
        countY = succ $ succ $ countUntil moonsIsOriginalY
        countZ = succ $ succ $ countUntil moonsIsOriginalZ

        originalX = moonsX originalMoons
        originalY = moonsY originalMoons
        originalZ = moonsZ originalMoons
        moonsIsOriginalX ms = originalX == (moonsX ms)
        moonsIsOriginalY ms = originalY == (moonsY ms)
        moonsIsOriginalZ ms = originalZ == (moonsZ ms)
        moonsX = map (\(Moon(Position(x, _, _),_)) -> x)
        moonsY = map (\(Moon(Position(_, y, _),_)) -> y)
        moonsZ = map (\(Moon(Position(_, _, z),_)) -> z)

-- updateposition
stepOnce ::  [Moon] -> [Moon]
stepOnce ms = applyPosition $ allGravity ms

-- foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
part1 :: [Moon] -> Int -> Int
part1 mx times = sum $ map (\m -> pot m * kin m) $ foldl (\a b -> stepOnce a) mx [1..times]

-- [Moon (Position (2,1,-3),Velocity (-3,-2,1)),Moon (Position (1,-8,0),Velocity (-1,1,3)),Moon (Position (3,-6,1),Velocity (3,2,-3)),Moon (Position (2,0,4),Velocity (1,-1,-1))]

pot :: Moon -> Int
pot (Moon(Position(x,y,z), _)) = abs x + abs y + abs z

kin :: Moon -> Int
kin (Moon(_, Velocity(x,y,z))) = abs x + abs y + abs z

moons :: [Moon]
moons = [
    Moon (Position(-10, -13, 7), Velocity(0, 0, 0)),
    Moon (Position(1, 2, 1), Velocity(0, 0, 0)),
    Moon (Position(-15, -3, 13), Velocity(0, 0, 0)),
    Moon (Position(3, 7, -4), Velocity(0, 0, 0))]

example :: [Moon]
example = [
    Moon (Position(-1, 0, 2), Velocity(0, 0, 0)),
    Moon (Position(2, -10, -7), Velocity(0, 0, 0)),
    Moon (Position(4, -8, 8), Velocity(0, 0, 0)),
    Moon (Position(3, 5, -1), Velocity(0, 0, 0))]

-- <x=-8, y=-10, z=0>
-- <x=5, y=5, z=10>
-- <x=2, y=-7, z=3>
-- <x=9, y=-8, z=-3>
example2 :: [Moon]
example2 = [
    Moon (Position(-8, -10, 0), Velocity(0, 0, 0)),
    Moon (Position(5, 5, 10), Velocity(0, 0, 0)),
    Moon (Position(2, -7, 3), Velocity(0, 0, 0)),
    Moon (Position(9, -8, -3), Velocity(0, 0, 0))]
