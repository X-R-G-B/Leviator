
getType :: Type -> String
getType Int32 = "Int32"
getType _ = Nothing

type Type = String

type TypeValue = Int32 | Bool | String

data Value = Var String | StaticValue TypeValue | Function FuncCall


-- Function

type Var = (Symbol, Type)

type ReturnType = Type | Void

type FuncPrototype = (Symbol, [Var], ReturnType)

type FuncDeclaration = (FuncPrototype, [Instruction])


-- condition

type Condition = (Value, [Instruction], [Instruction])

-- Instruction

type FuncCall = (Symbol, [Value])

type VarDeclaration = (Var, Value)

type VarAssignation = (Symbol, Value)

data Instruction = Function FuncCall | Return Value | Declaration VarDeclaration | Assignation VarAssignation | Cond Condition
