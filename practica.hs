


--Data types:

data VariableName = "X"|..|"Z"
type Variable     = ( VariableName , Int )
type ProgramState = [Variable]

--Variable manipulation functions:

-- Writes a value on a variable, updating the program state
write_variable:: ProgramState -> VariableName -> Int -> ProgramState
write_variable state variable value = map (\(x,y) -> if x = variable then (x,value) else (x,y)) state

-- Reads the value of a variable
read_variable:: ProgramState -> VariableName -> Int
read_variable state (n,v) = (\(x,y) -> y) (last filter (\(x,y) -> n = x) state)