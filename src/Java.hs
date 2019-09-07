module Java where

newtype ClassName = ClassName String    
newtype MethodName = MethodName String

data Type = IntType | BooleanType | DoubleType | ClassType ClassName

data Call = Call Expression MethodName [Expression]

data Expression

class PrettyString a where
    prettyString :: a -> String