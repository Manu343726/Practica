--Data types:

type VariableName = String
type Variable     = ( VariableName , Int )
type ProgramState = [Variable]

data ArithmeticExpression = I Int | V VariableName | ArithmeticExpression :+ ArithmeticExpression | ArithmeticExpression :- ArithmeticExpression | ArithmeticExpression :* ArithmeticExpression | ArithmeticExpression :/ ArithmeticExpression 
data BooleanExpression = B Bool | BooleanExpression :&& BooleanExpression | BooleanExpression :|| BooleanExpression | ArithmeticExpression :== ArithmeticExpression | ArithmeticExpression :/= ArithmeticExpression
data AssigmentInstruction = VariableName := ArithmeticExpression

type Instruction = AssigmentInstruction
type Program = [Instruction]

-- Variable manipulation functions:
----------------------------------

-- Writes a value on a variable, updating the program state
write_variable :: ProgramState -> VariableName -> Int -> ProgramState
write_variable state variable value = map (\(x,y) -> if( x == variable ) then (x,value) else (x,y)) state

-- Reads the value of a variable
read_variable :: ProgramState -> VariableName -> Int
read_variable state variable = (\(x,y) -> y) (last (filter (\(x,y) -> variable == x) state))

-- Adds a variable to the current scope
declare_variable :: ProgramState -> Variable -> ProgramState
declare_variable state variable = state ++ [variable]

-- Checks if a variable is defined in the current scope
check_variable_scope :: ProgramState -> VariableName -> Bool
check_variable_scope state variable_name = any (\(name,value) -> name == variable_name) state

-- Assigns a value to a variable
assign_variable :: ProgramState -> VariableName -> Int -> ProgramState
assign_variable state name value 
       | check_variable_scope state name = write_variable state name value
       | otherwise                       = declare_variable state (name,value)

-- Expression evaluation functions:
-----------------------------------

-- Arithmetic expression evaluation
evaluate_arithmetic_expression :: ProgramState -> ArithmeticExpression -> Int
evaluate_arithmetic_expression state (I n)        = n
evaluate_arithmetic_expression state (V v)        = read_variable state v
evaluate_arithmetic_expression state (lhs :+ rhs) = (evaluate_arithmetic_expression state lhs) + (evaluate_arithmetic_expression state rhs)
evaluate_arithmetic_expression state (lhs :- rhs) = (evaluate_arithmetic_expression state lhs) - (evaluate_arithmetic_expression state rhs)
evaluate_arithmetic_expression state (lhs :* rhs) = (evaluate_arithmetic_expression state lhs) * (evaluate_arithmetic_expression state rhs)
evaluate_arithmetic_expression state (lhs :/ rhs) = (evaluate_arithmetic_expression state lhs) `div` (evaluate_arithmetic_expression state rhs)

-- Boolean expression evaluation
evaluate_boolean_expression :: ProgramState -> BooleanExpression  -> Bool
evaluate_boolean_expression state (B ex)        = ex
evaluate_boolean_expression state (lhs :&& rhs) = (evaluate_boolean_expression state lhs) && (evaluate_boolean_expression state rhs)
evaluate_boolean_expression state (lhs :|| rhs) = (evaluate_boolean_expression state lhs) || (evaluate_boolean_expression state rhs)
evaluate_boolean_expression state (lhs :== rhs) = (evaluate_arithmetic_expression state lhs) == (evaluate_arithmetic_expression state rhs)
evaluate_boolean_expression state (lhs :/= rhs) = (evaluate_arithmetic_expression state lhs) /= (evaluate_arithmetic_expression state rhs)
evaluate_boolean_expression state (lhs :>= rhs) = (evaluate_arithmetic_expression state lhs) >= (evaluate_arithmetic_expression state rhs)
evaluate_boolean_expression state (lhs :<= rhs) = (evaluate_arithmetic_expression state lhs) <= (evaluate_arithmetic_expression state rhs)
evaluate_boolean_expression state (lhs :> rhs)  = (evaluate_arithmetic_expression state lhs) > (evaluate_arithmetic_expression state rhs)
evaluate_boolean_expression state (lhs :< rhs)  = (evaluate_arithmetic_expression state lhs) < (evaluate_arithmetic_expression state rhs)



-- Instruction execution functions:
-----------------------------------

-- Extracts the target variable of an assigment instruction 
get_assigment_variable :: AssigmentInstruction -> VariableName
get_assigment_variable (name := expression) = name

-- Extracts the left side expression of an assigment instruction
get_assigment_expression :: AssigmentInstruction -> ArithmeticExpression
get_assigment_expression (name := expression) = expression

-- Assigment instruction execution:
execute_assigment :: ProgramState -> AssigmentInstruction -> ProgramState
execute_assigment state instruction = assign_variable state (get_assigment_variable instruction) (evaluate_arithmetic_expression state (get_assigment_expression instruction)) 

-- Program execution:
execute_program :: ProgramState -> Program -> ProgramState
execute_program init_state program = foldl execute_assigment init_state program
