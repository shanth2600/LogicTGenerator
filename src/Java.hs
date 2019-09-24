module Java where

import Data.List

data Variable = Variable String deriving Show
data MethodName = MethodName String deriving Show
data ClassName = ClassName String deriving Show 

data Type = IntType | BooleanType | DoubleType | ClassType ClassName deriving Show

data Call = Call Expression MethodName [Expression] deriving Show

data Expression = IntLiteral Int
                | BooleanLiteral Bool
                | DoubleLiteral Double
                | StringLiteral String
                | NewExpression ClassName [Expression] 
                | CallExpression Call
                deriving Show

data Statement = VariableDeclaration Type Variable Expression
               | CallStatement Call
               deriving Show

data JavaTest = JavaTest [Statement]

instance Show JavaTest where
    show (JavaTest statements) =
        "public class test {\n" ++
        "    public static void main(String[] args) {\n" ++
        "   " ++
        intercalate "\n " (map show statements) ++
        "   }\n" ++
        "{\n"

data ClassSpec = ClassSpec ClassName [ClassName] [ConstructorSpec] [MethodSpec]
data ConstructorSpec = ConstructorSpec [Type]
data MethodSpec = MethodSpec MethodName [Type] (Maybe Type)