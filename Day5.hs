import Debug.Trace
import Data.Bool
type PC = Int
type Mem = [Int]
type Outputs = [Int]
-- data Mem = Mem [Int]
-- newtype Mem = Mem [Int]

initialMem :: Mem
--mem = [1002, 4, 3, 4, 33]
--mem = [3,0,4,0,99]
initialMem  = [3,225,1,225,6,6,1100,1,238,225,104,0,1102,68,5,225,1101,71,12,225,1,117,166,224,1001,224,-100,224,4,224,102,8,223,223,101,2,224,224,1,223,224,223,1001,66,36,224,101,-87,224,224,4,224,102,8,223,223,101,2,224,224,1,223,224,223,1101,26,51,225,1102,11,61,224,1001,224,-671,224,4,224,1002,223,8,223,1001,224,5,224,1,223,224,223,1101,59,77,224,101,-136,224,224,4,224,1002,223,8,223,1001,224,1,224,1,223,224,223,1101,11,36,225,1102,31,16,225,102,24,217,224,1001,224,-1656,224,4,224,102,8,223,223,1001,224,1,224,1,224,223,223,101,60,169,224,1001,224,-147,224,4,224,102,8,223,223,101,2,224,224,1,223,224,223,1102,38,69,225,1101,87,42,225,2,17,14,224,101,-355,224,224,4,224,102,8,223,223,1001,224,2,224,1,224,223,223,1002,113,89,224,101,-979,224,224,4,224,1002,223,8,223,1001,224,7,224,1,224,223,223,1102,69,59,225,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,7,677,677,224,1002,223,2,223,1006,224,329,1001,223,1,223,1007,226,226,224,1002,223,2,223,1006,224,344,1001,223,1,223,1108,226,677,224,102,2,223,223,1005,224,359,1001,223,1,223,1107,226,677,224,1002,223,2,223,1006,224,374,101,1,223,223,1107,677,226,224,1002,223,2,223,1006,224,389,101,1,223,223,7,226,677,224,1002,223,2,223,1005,224,404,101,1,223,223,1008,677,226,224,102,2,223,223,1005,224,419,101,1,223,223,1008,226,226,224,102,2,223,223,1006,224,434,101,1,223,223,107,226,226,224,1002,223,2,223,1005,224,449,1001,223,1,223,108,226,677,224,102,2,223,223,1005,224,464,101,1,223,223,1108,677,226,224,102,2,223,223,1005,224,479,101,1,223,223,1007,226,677,224,102,2,223,223,1006,224,494,101,1,223,223,107,677,677,224,102,2,223,223,1005,224,509,101,1,223,223,108,677,677,224,102,2,223,223,1006,224,524,1001,223,1,223,8,226,677,224,102,2,223,223,1005,224,539,101,1,223,223,107,677,226,224,102,2,223,223,1005,224,554,1001,223,1,223,8,226,226,224,102,2,223,223,1006,224,569,1001,223,1,223,7,677,226,224,1002,223,2,223,1005,224,584,1001,223,1,223,1108,226,226,224,102,2,223,223,1005,224,599,1001,223,1,223,1107,677,677,224,1002,223,2,223,1006,224,614,1001,223,1,223,1007,677,677,224,1002,223,2,223,1006,224,629,1001,223,1,223,108,226,226,224,102,2,223,223,1005,224,644,1001,223,1,223,8,677,226,224,1002,223,2,223,1005,224,659,1001,223,1,223,1008,677,677,224,1002,223,2,223,1006,224,674,1001,223,1,223,4,223,99,226]

inputbuf :: Int
inputbuf = 5

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

step :: PC -> Mem -> Outputs -> (Mem, Outputs)
step pc mem outputs = if ins == halted then (mem, outputs) else 
     case opcode of
            Add         -> step (pc+4)      add      outputs
            Multiply    -> step (pc+4)      multiply outputs
            Input       -> step (pc+2)      input    outputs
            Output      -> step (pc+2)      mem      output
            JumpIfTrue  -> step jumpIfTrue  mem      outputs
            JumpIfFalse -> step jumpIfFalse mem      outputs
            LessThan    -> step (pc+4)      lessThan outputs
            EqualTo     -> step (pc+4)      equalTo  outputs
            e           -> error ("segfault " ++ (show (pc, e)))
    where
        ins = mem !! pc
        a = mem !! (pc + 1)
        b = mem !! (pc + 2)
        c = mem !! (pc + 3)
        (opcode, (ma,mb,mc)) = traceShowId $ decode ins
        add = operation (+) paramA paramB paramC mem
        multiply = operation (*) paramA paramB paramC mem
        input = input' a mem
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

input' :: Int -> Mem -> Mem
input' a = store (a, inputbuf) 

output' :: Int -> Outputs -> Outputs
output' v1 outputs = outputs ++ [v1]

store :: (Int, Int) -> Mem -> Mem
store (a, v) mem = output
    where output = before ++ [v] ++ after
          before = take a mem
          after = drop (a+1) mem

run :: (Mem, Outputs)
run = step 0 initialMem []