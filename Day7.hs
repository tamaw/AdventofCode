import Debug.Trace
import Data.Bool
import Data.List (permutations, maximum)
type PC = Int
type Mem = [Int]
type Outputs = [Int]
type Inputs = [Int]

initialMem :: Mem
initialMem = [3,8,1001,8,10,8,105,1,0,0,21,42,67,76,89,110,191,272,353,434,99999,3,9,102,2,9,9,1001,9,2,9,1002,9,2,9,1001,9,2,9,4,9,99,3,9,1001,9,4,9,102,4,9,9,101,3,9,9,1002,9,2,9,1001,9,4,9,4,9,99,3,9,102,5,9,9,4,9,99,3,9,1001,9,3,9,1002,9,3,9,4,9,99,3,9,102,3,9,9,101,2,9,9,1002,9,3,9,101,5,9,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,99]

phaseSettings :: [[Int]]
phaseSettings = permutations [0..4]

runPhases :: Int
runPhases = maximum $ map runPhase phaseSettings

runPhase :: [Int] -> Int
runPhase [a, b, c, d, e]  = oe
    where 
        (_, [oa]) = run [a, 0] 
        (_, [ob]) = run [b, oa]
        (_, [oc]) = run [c, ob]
        (_, [od]) = run [d, oc]
        (_, [oe]) = run [e, od]

data Opcode
    = NOP
    | Add
    | Multiply
    | Input
    | Output
    | JumpIfTrue
    | JumpIfFalse
    | LessThan
    | EqualTo
    deriving (Enum, Eq, Show)

data Mode = Position | Immediate deriving (Enum, Eq, Show)
type Argument = (Mode, Int)
halted :: Int
halted = 99

data Result
  = Done  State
  | Yield Int State
  | Await State

step :: PC -> Mem -> Inputs -> Outputs -> (Mem, Outputs)
step pc mem inputs outputs = if ins == halted then (mem, outputs) else 
     case opcode of
            Add         -> step (pc+4)      add      inputs outputs
            Multiply    -> step (pc+4)      multiply inputs outputs
            Input       -> step (pc+2)      mem'     inputs' outputs
            Output      -> step (pc+2)      mem      inputs output
            JumpIfTrue  -> step jumpIfTrue  mem      inputs outputs
            JumpIfFalse -> step jumpIfFalse mem      inputs outputs
            LessThan    -> step (pc+4)      lessThan inputs outputs
            EqualTo     -> step (pc+4)      equalTo  inputs outputs
            e           -> error ("segfault " ++ show (pc, e))
    where
        ins = mem !! pc
        a = mem !! (pc + 1)
        b = mem !! (pc + 2)
        c = mem !! (pc + 3)
        (opcode, (ma,mb,mc)) = traceShowId $ decode ins
        add = operation (+) paramA paramB paramC mem
        multiply = operation (*) paramA paramB paramC mem
        (inputs', mem') = input' a (inputs, mem)
        output = output' paramA outputs
        jumpIfTrue = jump (/= 0) pc paramA paramB
        jumpIfFalse = jump (== 0) pc paramA paramB
        lessThan = operation (\x y -> if x < y then 1 else 0) paramA paramB c mem
        equalTo = operation (\x y -> if x == y then 1 else 0) paramA paramB c mem
        paramA = fetch ma a mem
        paramB = fetch mb b mem
        paramC = c

decode :: Int -> (Opcode, (Mode, Mode, Mode))
decode word = (opcode, (mode, mode2, mode3))
    where
        opcode = toEnum (word `div` 1 `rem` 100) :: Opcode
        [mode, mode2, mode3] = [toEnum (word `div` 10^p `rem` 10) | p <- [2,3,4]] :: [Mode]

fetch :: Mode -> Int -> Mem -> Int
fetch Position a mem = mem !! a
fetch Immediate a _ = a

jump :: (Int -> Bool) -> PC -> Int -> Int -> PC
jump op pc paramA paramB = if op paramA then paramB else pc+3

operation :: (Int -> Int -> Int) -> Int -> Int -> Int -> Mem -> Mem
operation op paramA paramB addr = store (addr, paramA `op` paramB) 

input' :: Int -> (Inputs, Mem) -> (Inputs, Mem)
input' a ((x:xs), mem) = (xs, store (a, x) mem)

output' :: Int -> Outputs -> Outputs
output' v1 outputs = outputs ++ [v1]

store :: (Int, Int) -> Mem -> Mem
store (a, v) mem = output
    where output = before ++ [v] ++ after
          before = take a mem
          after = drop (a+1) mem

run :: Inputs -> (Mem, Outputs)
run inputs = step 0 initialMem inputs []

