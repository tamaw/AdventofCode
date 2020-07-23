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

initialMem :: Mem
-- initialMem = [104,1125899906842624,99]
-- initialMem = [1102,34915192,34915192,7,4,7,99,0]
-- initialMem = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
initialMem = [1102,34463338,34463338,63,1007,63,34463338,63,1005,63,53,1102,3,1,1000,109,988,209,12,9,1000,209,6,209,3,203,0,1008,1000,1,63,1005,63,65,1008,1000,2,63,1005,63,904,1008,1000,0,63,1005,63,58,4,25,104,0,99,4,0,104,0,99,4,17,104,0,99,0,0,1102,0,1,1020,1102,1,38,1015,1102,37,1,1003,1102,21,1,1002,1102,34,1,1017,1101,39,0,1008,1102,1,20,1007,1101,851,0,1022,1102,1,1,1021,1101,24,0,1009,1101,0,26,1005,1101,29,0,1019,1101,0,866,1027,1101,0,260,1025,1102,33,1,1014,1101,0,36,1006,1102,1,25,1018,1102,1,669,1028,1101,0,27,1016,1101,0,23,1012,1102,35,1,1004,1102,1,31,1011,1101,0,664,1029,1101,32,0,1010,1101,0,22,1000,1102,873,1,1026,1102,1,848,1023,1102,265,1,1024,1101,0,28,1013,1101,30,0,1001,109,6,2107,31,-5,63,1005,63,201,1001,64,1,64,1106,0,203,4,187,1002,64,2,64,109,4,21107,40,39,1,1005,1011,219,1106,0,225,4,209,1001,64,1,64,1002,64,2,64,109,-1,2102,1,0,63,1008,63,24,63,1005,63,247,4,231,1106,0,251,1001,64,1,64,1002,64,2,64,109,9,2105,1,6,4,257,1105,1,269,1001,64,1,64,1002,64,2,64,109,-18,2108,19,2,63,1005,63,289,1001,64,1,64,1106,0,291,4,275,1002,64,2,64,109,23,21108,41,41,-8,1005,1015,313,4,297,1001,64,1,64,1106,0,313,1002,64,2,64,109,-19,2101,0,-4,63,1008,63,23,63,1005,63,333,1106,0,339,4,319,1001,64,1,64,1002,64,2,64,109,9,1206,7,357,4,345,1001,64,1,64,1105,1,357,1002,64,2,64,109,-15,2108,22,2,63,1005,63,375,4,363,1105,1,379,1001,64,1,64,1002,64,2,64,109,10,1208,-7,30,63,1005,63,397,4,385,1106,0,401,1001,64,1,64,1002,64,2,64,109,-7,1201,8,0,63,1008,63,27,63,1005,63,421,1106,0,427,4,407,1001,64,1,64,1002,64,2,64,109,-4,1202,3,1,63,1008,63,22,63,1005,63,449,4,433,1105,1,453,1001,64,1,64,1002,64,2,64,109,15,21108,42,40,4,1005,1016,469,1105,1,475,4,459,1001,64,1,64,1002,64,2,64,109,1,21101,43,0,0,1008,1013,43,63,1005,63,501,4,481,1001,64,1,64,1105,1,501,1002,64,2,64,109,-17,1207,10,35,63,1005,63,521,1001,64,1,64,1105,1,523,4,507,1002,64,2,64,109,7,2107,23,6,63,1005,63,545,4,529,1001,64,1,64,1105,1,545,1002,64,2,64,109,3,1201,0,0,63,1008,63,36,63,1005,63,571,4,551,1001,64,1,64,1105,1,571,1002,64,2,64,109,1,21107,44,45,7,1005,1014,593,4,577,1001,64,1,64,1106,0,593,1002,64,2,64,109,7,1205,6,609,1001,64,1,64,1106,0,611,4,599,1002,64,2,64,109,-14,1202,4,1,63,1008,63,32,63,1005,63,635,1001,64,1,64,1106,0,637,4,617,1002,64,2,64,109,30,1205,-9,651,4,643,1105,1,655,1001,64,1,64,1002,64,2,64,109,-4,2106,0,2,4,661,1106,0,673,1001,64,1,64,1002,64,2,64,109,-5,21101,45,0,-8,1008,1013,42,63,1005,63,697,1001,64,1,64,1106,0,699,4,679,1002,64,2,64,109,-10,1207,-6,27,63,1005,63,721,4,705,1001,64,1,64,1105,1,721,1002,64,2,64,109,-11,2101,0,6,63,1008,63,36,63,1005,63,743,4,727,1106,0,747,1001,64,1,64,1002,64,2,64,109,3,2102,1,-2,63,1008,63,33,63,1005,63,767,1105,1,773,4,753,1001,64,1,64,1002,64,2,64,109,18,1206,0,789,1001,64,1,64,1106,0,791,4,779,1002,64,2,64,109,-11,1208,-5,23,63,1005,63,807,1106,0,813,4,797,1001,64,1,64,1002,64,2,64,109,-5,21102,46,1,10,1008,1015,46,63,1005,63,835,4,819,1105,1,839,1001,64,1,64,1002,64,2,64,109,11,2105,1,7,1106,0,857,4,845,1001,64,1,64,1002,64,2,64,109,14,2106,0,-3,1001,64,1,64,1106,0,875,4,863,1002,64,2,64,109,-22,21102,47,1,5,1008,1013,48,63,1005,63,899,1001,64,1,64,1106,0,901,4,881,4,64,99,21102,1,27,1,21102,915,1,0,1105,1,922,21201,1,65718,1,204,1,99,109,3,1207,-2,3,63,1005,63,964,21201,-2,-1,1,21102,1,942,0,1105,1,922,22101,0,1,-1,21201,-2,-3,1,21102,957,1,0,1106,0,922,22201,1,-1,-2,1105,1,968,21201,-2,0,-2,109,-3,2105,1,0]

run :: (Mem, Outputs)
run = step 0 0 initialMem [2] []

step :: PC -> RelativeBase -> Mem -> Inputs -> Outputs -> (Mem, Outputs)
step pc base mem inputs outputs = if ins == halted then (mem, outputs) else 
     case opcode of
            Add         -> step (pc+4)      base        (op (+))  inputs    outputs
            Multiply    -> step (pc+4)      base        (op (*))  inputs    outputs
            Input       -> step (pc+2)      base        mem'      inputs'   outputs
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
