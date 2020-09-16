module IntMachine where 

import Debug.Trace
import Data.Bool
import Data.List (permutations, maximum)
import Debug.Trace (traceShowId)
type PC = Int
type Mem = [Int]
type Outputs = [Int]
type Inputs = [Int]
type RelativeBase = Int
type Addr = Int

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

data StepResult
    = Done (Mem, Outputs)
    | NeedsMoreInput Outputs (Inputs -> StepResult)

showSR :: StepResult -> String
showSR (NeedsMoreInput _ _) = "NeedsMoreInput"
showSR (Done _) = "Done"

run :: Inputs -> Mem -> (Mem, Outputs)
run inputs initialMem = case step 0 0 initialMem inputs [] of
        Done r -> r
        NeedsMoreInput _ _ -> error "ran out of inputs"

runIncremental :: Mem -> StepResult
runIncremental initialMem = step 0 0 initialMem [] []

step :: PC -> RelativeBase -> Mem -> Inputs -> Outputs -> StepResult
step pc base mem inputs outputs = if ins == halted then Done (mem, outputs) else 
     case opcode of
            Add         -> step (pc+4)      base        (op (+))  inputs    outputs
            Multiply    -> step (pc+4)      base        (op (*))  inputs    outputs
            Input       -> if null inputs 
                            then NeedsMoreInput outputs (\newInput -> step pc base mem newInput []) 
                            else step (pc+2) base mem' inputs' outputs
            Output      -> step (pc+2)      base        mem       inputs    outputs'
            JumpIfTrue  -> step jumpIfTrue  base        mem       inputs    outputs
            JumpIfFalse -> step jumpIfFalse base        mem       inputs    outputs
            LessThan    -> step (pc+4)      base        (bop(<))  inputs    outputs
            EqualTo     -> step (pc+4)      base        (bop(==)) inputs    outputs
            RelativeTo  -> step (pc+2)      relativeTo  mem       inputs    outputs
            e           -> error ("segfault " ++ show (pc, e))
    where
        ins = mem !! pc
        a = mem !! (pc + 1)
        b = mem !! (pc + 2)
        c = mem !! (pc + 3)
        paramA = fetch ma base a mem
        paramB = fetch mb base b mem
        paramC = c

        (opcode, (ma,mb,mc)) = decode ins
        (inputs', mem') = input' ma base a (inputs, mem)
        outputs' = output' paramA outputs

        op :: (Int -> Int -> Int) -> Mem
        op fun = store mc base paramC (paramA `fun` paramB) mem
        bop fun = op (\x y -> if fun x y then 1 else 0)

        jumpIfTrue = jump (/= 0) pc paramA paramB
        jumpIfFalse = jump (== 0) pc paramA paramB
        relativeTo = base + paramA

decode :: Int -> (Opcode, (Mode, Mode, Mode))
decode word = (opcode, (mode, mode2, mode3))
    where
        opcode = toEnum (word `div` 1 `rem` 100) :: Opcode
        [mode, mode2, mode3] = [toEnum (word `div` 10^p `rem` 10) | p <- [2,3,4]] :: [Mode]

fetch :: Mode -> RelativeBase -> Int -> Mem -> Int
fetch Immediate _ a _ = a
fetch mode base a mem = value
    where
        addr = getEffectiveAddress mode base a
        value = if addr < length mem then mem !! addr else 0

jump :: (Int -> Bool) -> PC -> Int -> Int -> PC
jump op pc paramA paramB = if op paramA then paramB else pc+3

input' :: Mode -> RelativeBase -> Int -> (Inputs, Mem) -> (Inputs, Mem)
input' mode base a ((x:xs), mem) = (xs, store mode base a x mem)

output' :: Int -> Outputs -> Outputs
output' v1 outputs = outputs ++ [v1]

store :: Mode -> RelativeBase -> Addr -> Int -> Mem -> Mem
store mode base a v mem = output
    where effectiveAddress = getEffectiveAddress mode base a
          output = before ++ [v] ++ after
          before = take effectiveAddress mmem
          after = drop (effectiveAddress+1) mmem
          mmem = if effectiveAddress >= length mem then mem ++ (replicate (1 + effectiveAddress - length mem) 0) else mem

getEffectiveAddress :: Mode -> RelativeBase -> Addr -> Int
getEffectiveAddress Position _ a = a
getEffectiveAddress Relative base a = base + a
getEffectiveAddress Immediate _ _ = error "cannot store in immediate value"
