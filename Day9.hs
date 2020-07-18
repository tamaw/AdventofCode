import Debug.Trace
import Data.Bool
import Data.List (permutations, maximum)
type PC = Int
type Mem = [Int]
type Outputs = [Int]
type Inputs = [Int]
type Pointer = Int

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
    | RelativeTo
    deriving (Enum, Eq, Show)

data Mode = Position | Immediate | Relative deriving (Enum, Eq, Show)
type Argument = (Mode, Int)
halted :: Int
halted = 99

ptr :: Pointer
ptr = 0

initialMem :: Mem
initialMem  = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]

run :: (Mem, Outputs)
run = step 0 0 initialMem [] []

step :: PC -> Pointer -> Mem -> Inputs -> Outputs -> (Mem, Outputs)
step pc ptr mem inputs outputs = if ins == halted then (mem, outputs) else 
     case opcode of
            Add         -> step (pc+4)      ptr     add      inputs    outputs
            Multiply    -> step (pc+4)      ptr     multiply inputs    outputs
            Input       -> step (pc+2)      ptr     mem'     inputs'   outputs
            Output      -> step (pc+2)      ptr     mem      inputs    output
            JumpIfTrue  -> step jumpIfTrue  ptr     mem      inputs    outputs
            JumpIfFalse -> step jumpIfFalse ptr     mem      inputs    outputs
            LessThan    -> step (pc+4)      ptr     lessThan inputs    outputs
            EqualTo     -> step (pc+4)      ptr     equalTo  inputs    outputs
            RelativeTo  -> step (pc+2)      ptr     mem      inputs    outputs
            e           -> error ("segfault " ++ show (pc, e))
    where
        ins = mem !! pc
        a = mem !! (pc + 1)
        b = mem !! (pc + 2)
        c = mem !! (pc + 3)
        (opcode, (ma,mb,mc)) = decode ins
        add = operation (+) paramA paramB paramC mem
        multiply = operation (*) paramA paramB paramC mem
        (inputs', mem') = input' a (inputs, mem)
        output = output' paramA outputs
        jumpIfTrue = jump (/= 0) pc paramA paramB
        jumpIfFalse = jump (== 0) pc paramA paramB
        lessThan = operation (\x y -> if x < y then 1 else 0) paramA paramB c mem
        equalTo = operation (\x y -> if x == y then 1 else 0) paramA paramB c mem
        relative = relative + paramA
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
-- fetch Relative a mem = 

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


