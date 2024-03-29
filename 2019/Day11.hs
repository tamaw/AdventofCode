import Control.Monad (forM_)
import Debug.Trace
import Data.Bool
import Data.Maybe (fromMaybe)
import Data.List (permutations, maximum, nub)
import Debug.Trace (traceShowId)
import IntMachine

program :: [Int]
program = [3,8,1005,8,326,1106,0,11,0,0,0,104,1,104,0,3,8,102,-1,8,10,101,1,10,10,4,10,1008,8,1,10,4,10,1001,8,0,29,2,1003,17,10,1006,0,22,2,106,5,10,1006,0,87,3,8,102,-1,8,10,101,1,10,10,4,10,1008,8,1,10,4,10,1001,8,0,65,2,7,20,10,2,9,17,10,2,6,16,10,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,0,10,4,10,101,0,8,99,1006,0,69,1006,0,40,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,1,10,4,10,101,0,8,127,1006,0,51,2,102,17,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,1,8,10,4,10,1002,8,1,155,1006,0,42,3,8,1002,8,-1,10,101,1,10,10,4,10,108,0,8,10,4,10,101,0,8,180,1,106,4,10,2,1103,0,10,1006,0,14,3,8,102,-1,8,10,1001,10,1,10,4,10,108,0,8,10,4,10,1001,8,0,213,1,1009,0,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,0,8,10,4,10,1002,8,1,239,1006,0,5,2,108,5,10,2,1104,7,10,3,8,102,-1,8,10,101,1,10,10,4,10,108,0,8,10,4,10,102,1,8,272,2,1104,12,10,1,1109,10,10,3,8,102,-1,8,10,1001,10,1,10,4,10,108,1,8,10,4,10,102,1,8,302,1006,0,35,101,1,9,9,1007,9,1095,10,1005,10,15,99,109,648,104,0,104,1,21102,937268449940,1,1,21102,1,343,0,1105,1,447,21101,387365315480,0,1,21102,1,354,0,1105,1,447,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,21101,0,29220891795,1,21102,1,401,0,1106,0,447,21101,0,248075283623,1,21102,412,1,0,1105,1,447,3,10,104,0,104,0,3,10,104,0,104,0,21101,0,984353760012,1,21102,1,435,0,1105,1,447,21102,1,718078227200,1,21102,1,446,0,1105,1,447,99,109,2,21202,-1,1,1,21102,40,1,2,21101,0,478,3,21101,468,0,0,1106,0,511,109,-2,2106,0,0,0,1,0,0,1,109,2,3,10,204,-1,1001,473,474,489,4,0,1001,473,1,473,108,4,473,10,1006,10,505,1102,1,0,473,109,-2,2105,1,0,0,109,4,1202,-1,1,510,1207,-3,0,10,1006,10,528,21102,1,0,-3,22102,1,-3,1,22101,0,-2,2,21101,0,1,3,21102,1,547,0,1105,1,552,109,-4,2105,1,0,109,5,1207,-3,1,10,1006,10,575,2207,-4,-2,10,1006,10,575,21202,-4,1,-4,1105,1,643,21202,-4,1,1,21201,-3,-1,2,21202,-2,2,3,21102,1,594,0,1106,0,552,22102,1,1,-4,21101,1,0,-1,2207,-4,-2,10,1006,10,613,21101,0,0,-1,22202,-2,-1,-2,2107,0,-3,10,1006,10,635,22101,0,-1,1,21101,0,635,0,106,0,510,21202,-2,-1,-2,22201,-4,-2,-4,109,-5,2105,1,0]
--program = [3,8,1005,8,321,1106,0,11,0,0,0,104,1,104,0,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,1,10,4,10,1002,8,1,29,3,8,1002,8,-1,10,101,1,10,10,4,10,108,0,8,10,4,10,1002,8,1,50,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,0,10,4,10,1001,8,0,73,1,1105,16,10,2,1004,8,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,0,10,4,10,1002,8,1,103,1006,0,18,1,105,14,10,3,8,102,-1,8,10,101,1,10,10,4,10,108,0,8,10,4,10,102,1,8,131,1006,0,85,1,1008,0,10,1006,0,55,2,104,4,10,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,1,10,4,10,1001,8,0,168,2,1101,1,10,1006,0,14,3,8,102,-1,8,10,101,1,10,10,4,10,108,1,8,10,4,10,102,1,8,196,1006,0,87,1006,0,9,1,102,20,10,3,8,1002,8,-1,10,101,1,10,10,4,10,108,1,8,10,4,10,1001,8,0,228,3,8,1002,8,-1,10,101,1,10,10,4,10,108,0,8,10,4,10,1002,8,1,250,2,5,0,10,2,1009,9,10,2,107,17,10,1006,0,42,3,8,102,-1,8,10,101,1,10,10,4,10,108,1,8,10,4,10,1001,8,0,287,2,102,8,10,1006,0,73,1006,0,88,1006,0,21,101,1,9,9,1007,9,925,10,1005,10,15,99,109,643,104,0,104,1,21102,1,387353256856,1,21101,0,338,0,1105,1,442,21101,936332866452,0,1,21101,349,0,0,1105,1,442,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,21101,0,179357024347,1,21101,0,396,0,1105,1,442,21102,1,29166144659,1,21102,407,1,0,1105,1,442,3,10,104,0,104,0,3,10,104,0,104,0,21102,1,718170641252,1,21102,430,1,0,1106,0,442,21101,825012151040,0,1,21102,441,1,0,1106,0,442,99,109,2,21202,-1,1,1,21102,1,40,2,21102,1,473,3,21102,463,1,0,1105,1,506,109,-2,2106,0,0,0,1,0,0,1,109,2,3,10,204,-1,1001,468,469,484,4,0,1001,468,1,468,108,4,468,10,1006,10,500,1102,1,0,468,109,-2,2105,1,0,0,109,4,1202,-1,1,505,1207,-3,0,10,1006,10,523,21101,0,0,-3,22101,0,-3,1,21202,-2,1,2,21102,1,1,3,21102,1,542,0,1105,1,547,109,-4,2106,0,0,109,5,1207,-3,1,10,1006,10,570,2207,-4,-2,10,1006,10,570,22102,1,-4,-4,1105,1,638,22102,1,-4,1,21201,-3,-1,2,21202,-2,2,3,21101,0,589,0,1106,0,547,22102,1,1,-4,21101,1,0,-1,2207,-4,-2,10,1006,10,608,21102,0,1,-1,22202,-2,-1,-2,2107,0,-3,10,1006,10,630,21202,-1,1,1,21102,630,1,0,105,1,505,21202,-2,-1,-2,22201,-4,-2,-4,109,-5,2106,0,0]

data Colour = Black | White deriving (Show, Enum)
data Direction = Up | Down | L | R
data DirectionCommand = TurnLeft | TurnRight deriving (Enum)
type POS = (Int, Int)

camera :: [(POS, Colour)] -> POS -> Colour
camera panels position = fromMaybe Black $ lookup position panels

paint :: POS -> Colour -> [(POS, Colour)] -> [(POS, Colour)]
paint panel colour panels = (panel, colour) : panels

paintHull :: Mem -> [(POS, Colour)]
paintHull initialMem = go (0,0) Up [((0,0), White)] (runIncremental initialMem)
    where
        go position facing panels stepResult =
            case stepResult of
                Done (mem, [colorCmd, _]) -> paint position (toEnum colorCmd) panels
                NeedsMoreInput [] continue -> go position facing panels (continue [fromEnum (camera panels position)])
                NeedsMoreInput [colorCmd, moveCmd] continue ->
                    let (newPos, newDirection) = move (toEnum moveCmd) position facing
                    in  go newPos newDirection (paint position (toEnum colorCmd) panels) (continue [fromEnum (camera panels newPos)])

draw :: [(POS, Colour)] -> IO ()
draw panels = do
    forM_ [bottom,bottom-1..top] $ \y -> do
        forM_ [left..right] $ \x -> do
            let
                colour = camera panels (x,y)
            case colour of
                Black -> putStr " "
                White -> putStr "█"
        putStr "\n"
    where
        left = -6
        right = 50
        top = -10
        bottom = 0

-- positive y goes up
move :: DirectionCommand -> POS -> Direction -> (POS, Direction)
move TurnLeft  (x, y) Up = ((x-1, y), L)
move TurnLeft  (x, y) Down = ((x+1, y), R)
move TurnLeft  (x, y) L = ((x, y-1), Down)
move TurnLeft  (x, y) R = ((x, y+1), Up)
move TurnRight (x, y) Up = ((x+1, y), R)
move TurnRight (x, y) Down = ((x-1, y), L)
move TurnRight (x, y) L = ((x, y+1), Up)
move TurnRight (x, y) R = ((x, y-1), Down)

part1 = length $ nub $ map fst $ paintHull program
part2 = draw $ paintHull program