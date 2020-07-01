import Debug.Trace
import Data.Bool
import Data.List (permutations, maximum)
type PC = Int
type Mem = [Int]
type Outputs = [Int]
type Inputs = [Int]

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

data StepResult
    = Done (Mem, Outputs)
    | NeedsMoreInput Outputs (Inputs -> StepResult)

showSR :: StepResult -> String
showSR (NeedsMoreInput _ _) = "NeedsMoreInput"
showSR (Done _) = "Done"

instance Show StepResult where
    show = showSR

initialMem :: Mem
--initialMem  = [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26, 27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]
initialMem = [3,8,1001,8,10,8,105,1,0,0,21,42,67,76,89,110,191,272,353,434,99999,3,9,102,2,9,9,1001,9,2,9,1002,9,2,9,1001,9,2,9,4,9,99,3,9,1001,9,4,9,102,4,9,9,101,3,9,9,1002,9,2,9,1001,9,4,9,4,9,99,3,9,102,5,9,9,4,9,99,3,9,1001,9,3,9,1002,9,3,9,4,9,99,3,9,102,3,9,9,101,2,9,9,1002,9,3,9,101,5,9,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,99]

phaseSettings :: [[Int]]
phaseSettings = permutations [5..9]

runPhase :: [Int] -> Int
runPhase [a, b, c, d, e]  = oe
    where 
        (_, [oa]) = run [a, 0] 
        (_, [ob]) = run [b, oa]
        (_, [oc]) = run [c, ob]
        (_, [od]) = run [d, oc]
        (_, [oe]) = run [e, od]

runPhases2 :: [Int]
runPhases2 = maximum $ map runPhase2 phaseSettings

runPhase2 :: [Int] -> Outputs
runPhase2 [a, b, c, d, e]  = loop ka0 kb0 kc0 kd0 ke0 [0]
    where 
        NeedsMoreInput _ ka0 = initializeWithPhase a
        NeedsMoreInput _ kb0 = initializeWithPhase b
        NeedsMoreInput _ kc0 = initializeWithPhase c
        NeedsMoreInput _ kd0 = initializeWithPhase d
        NeedsMoreInput _ ke0 = initializeWithPhase e

        loop :: (Inputs -> StepResult)
             -> (Inputs -> StepResult)
             -> (Inputs -> StepResult)
             -> (Inputs -> StepResult)
             -> (Inputs -> StepResult)
             -> Inputs
             -> Outputs
        loop ka kb kc kd ke input
            | NeedsMoreInput oa ka' <- traceShowId $ ka input
            , NeedsMoreInput ob kb' <- traceShowId $ kb oa
            , NeedsMoreInput oc kc' <- traceShowId $ kc ob
            , NeedsMoreInput od kd' <- traceShowId $ kd oc
            , NeedsMoreInput oe ke' <- traceShowId $ ke od
                = loop ka' kb' kc' kd' ke' oe
        loop ka kb kc kd ke input
            | oa <- outputs (ka input)
            , ob <- outputs (kb oa)
            , oc <- outputs (kc ob)
            , od <- outputs (kd oc)
            , oe <- outputs (ke od)
                = oe

        outputs :: StepResult -> Outputs
        outputs (Done (_, o)) = o
        outputs (NeedsMoreInput o _) = o

        initializeWithPhase :: Int -> StepResult
        initializeWithPhase phase =
                case runIncremental of
                    NeedsMoreInput _ k -> k [phase]
                    Done _ -> error "unexpected state"

run :: Inputs -> (Mem, Outputs)
run inputs = case step 0 initialMem inputs [] of
        Done r -> r
        NeedsMoreInput _ _ -> error "ran out of inputs"

runIncremental :: StepResult
runIncremental = step 0 initialMem [] []

step :: PC -> Mem -> Inputs -> Outputs -> StepResult
step pc mem inputs outputs = if ins == halted then Done (mem, outputs) else 
     case opcode of
            Add         -> step (pc+4)      add      inputs outputs
            Multiply    -> step (pc+4)      multiply inputs outputs
            Input       -> if null inputs 
                            then NeedsMoreInput outputs (\newInput -> step pc mem newInput []) 
                            else step (pc+2) mem' inputs' outputs
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
        (opcode, (ma,mb,mc)) = decode ins
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


