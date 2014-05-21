


--Data types:

data VariableName = "X"|..|"Z"
type Variable     = ( VariableName , Int )
type ProgramState = [Variable]

data arithmetic_expression = N Int | V Variable | left_side_expression :+ left_side_expression | left_side_expression :- left_side_expression | left_side_expression :/ left_side_expression 
data AssigmentInstruction = Variable := left_side_expression

data Instruction = AssigmentInstruction
type Program = [Instruction]

-- Variable manipulation functions:
----------------------------------

-- Writes a value on a variable, updating the program state
write_variable:: ProgramState -> VariableName -> Int -> ProgramState
write_variable state variable value = map (\(x,y) -> if x = variable then (x,value) else (x,y)) state

-- Reads the value of a variable
read_variable:: ProgramState -> VariableName -> Int
read_variable state (n,v) = (\(x,y) -> y) (last filter (\(x,y) -> n = x) state)

-- Expression evaluation functions:
-----------------------------------

-- Arithmetic expression evaluation
evaluate_arithmetic_expression:: arithmetic_expression -> Int


-- Instruction execution functions:
-----------------------------------

-- Assigment instruction:
execute_assigment:: ProgramState -> AssigmentInstruction -> ProgramState
execute_assigment state instruction = write_variable state (get_assigment_variable instruction) (evaluate_arithmetic_expression (get_assigment_expression instruction)) 

-- Program execution:
execute_program:: ProgramState -> Program -> ProgramState
execute_program init_state program = foldl execute_assigment program





