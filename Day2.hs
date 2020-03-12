
input :: [Int]
-- input = [1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,9,19,1,19,5,23,1,13,23,27,1,27,6,31,2,31,6,35,2,6,35,39,1,39,5,43,1,13,43,47,1,6,47,51,2,13,51,55,1,10,55,59,1,59,5,63,1,10,63,67,1,67,5,71,1,71,10,75,1,9,75,79,2,13,79,83,1,9,83,87,2,87,13,91,1,10,91,95,1,95,9,99,1,13,99,103,2,103,13,107,1,107,10,111,2,10,111,115,1,115,9,119,2,119,6,123,1,5,123,127,1,5,127,131,1,10,131,135,1,135,6,139,1,10,139,143,1,143,6,147,2,147,13,151,1,5,151,155,1,155,5,159,1,159,2,163,1,163,9,0,99,2,14,0,0]
input = [1,12,2,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,9,19,1,19,5,23,1,13,23,27,1,27,6,31,2,31,6,35,2,6,35,39,1,39,5,43,1,13,43,47,1,6,47,51,2,13,51,55,1,10,55,59,1,59,5,63,1,10,63,67,1,67,5,71,1,71,10,75,1,9,75,79,2,13,79,83,1,9,83,87,2,87,13,91,1,10,91,95,1,95,9,99,1,13,99,103,2,103,13,107,1,107,10,111,2,10,111,115,1,115,9,119,2,119,6,123,1,5,123,127,1,5,127,131,1,10,131,135,1,135,6,139,1,10,139,143,1,143,6,147,2,147,13,151,1,5,151,155,1,155,5,159,1,159,2,163,1,163,9,0,99,2,14,0,0]
-- input = [1,9,10,3,2,3,11,0,99,30,40,50]
--input = [1,1,1,4,99,5,6,0,99]
--input = [1,0,0,0,99]
--input = [2,3,0,3,99]
--input = [2,4,4,5,99,0]

type Mem = [Int]

data Opcode = NOP | Add | Multiple deriving (Enum, Eq, Show)
data State = Continue | Halted deriving Eq
data Operand = Int

step :: [Int] -> Int -> [Int]
step input pc = if result == Halted then output else step output (pc+4)
    where
        result = if (input !! pc) == 99 then Halted else Continue
        ins = decode $ fetch input pc
        output = if result == Halted then input else exec input ins

fetch :: [Int] -> Int -> [Int]
fetch input pc = take 4 $ drop pc input

decode :: [Int] -> (Opcode, Int, Int, Int)
decode input = (ir, a0, a1, a2)
    where
        ir = toEnum (input !! 0) :: Opcode
        a0 = input !! 1
        a1 = input !! 2
        a2 = input !! 3

exec :: [Int] -> (Opcode, Int, Int, Int) -> [Int]
exec input (Add, a0, a1, a2) = memmap input (a2, (input !! a0) + (input !! a1))
exec input (Multiple, a0, a1, a2) = memmap input (a2, (input !! a0) * (input !! a1))

memmap :: [Int] -> (Int, Int) -> [Int]
memmap input (a, v) = output
    where output = before ++ [v] ++ after
          before = take a input
          after = drop (a+1) input

run :: [Int]
run = step input 0
