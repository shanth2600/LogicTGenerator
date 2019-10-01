module Java where

import Lib
import Data.List
import Control.Monad.Logic
import qualified Data.Map.Strict as M

data Variable = Variable String deriving Show
data MethodName = MethodName String deriving Show
data ClassName = ClassName String deriving (Show, Eq, Ord)

data Type = IntType | BooleanType | DoubleType | ClassType ClassName deriving Show

data Call = Call Expression MethodName [Expression] deriving Show

data Expression = VariableExpression Variable
                | IntLiteral Int
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

-------------------------------- Syntax

type Env = M.Map Variable Type
type Spec = [ClassSpec]

min_num_statements = 1
max_num_statements = 1

default_spec = [
    ClassSpec (ClassName "Object") [] [ConstructorSpec []] [],
    ClassSpec (ClassName "CharSequence") [(ClassName "Object")] [] [MethodSpec (MethodName "length") [] (Just IntType)],
    ClassSpec 
        (ClassName "String") 
        [(ClassName "Object"), (ClassName "CharSequence")] 
        [ConstructorSpec []] 
        [(MethodSpec (MethodName "concat")) [ClassType (ClassName "String")] (Just (ClassType (ClassName "String")))],
    ClassSpec (
        ClassName "java.util.ArrayList") 
        [(ClassName "Object")] [ConstructorSpec []] 
        [
            (MethodSpec (MethodName "size") [] (Just IntType)),
            (MethodSpec (MethodName "add") [ClassType (ClassName "Object")] (Just BooleanType)),
            (MethodSpec (MethodName "remove") [ClassType (ClassName "Object")] (Just BooleanType))]]


javaMain bound = countResults (makeTest default_spec bound)


toHierarchy :: Spec -> M.Map ClassName [ClassName]
toHierarchy spec = M.fromList $ map (\(ClassSpec name extendsImp _ _) -> (name, extendsImp)) spec

allParents :: Spec -> ClassName -> Logic ClassName
allParents spec of' = case M.lookup of' hierarchy of
    (Just directParents) -> member directParents
    Nothing              -> mzero
    where 
        hierarchy = toHierarchy spec

parentChild :: Spec -> ClassName -> ClassName -> Bool
parentChild spec parent child = case M.lookup child hierarchy of
    (Just directParents) -> if elem parent directParents
        then True
        else undefined -- also something here
    Nothing              -> False
    where 
        hierarchy = toHierarchy spec

typesCompatibleParenChild :: Spec -> Type -> Type -> Bool
typesCompatibleParenChild spec parent child = case (parent, child) of
    (_,_)                                       -> True -- somethings wrong here
    (ClassType parentName, ClassType childName) -> parentChild spec parentName childName
    otherwise                                   -> False

makeExpressionsOfTypes :: Spec -> Int -> Env -> [Type] -> Logic [Expression]
makeExpressionsOfTypes spec size env types = case types of
    [] -> return []
    (head:tail) -> do
        (expression, expressionType) <- makeExpression spec size env
        if typesCompatibleParenChild spec head expressionType
            then do
                rest <- makeExpressionsOfTypes spec size env tail
                return $ [expression] ++ rest
            else mzero


makeExpression :: Spec -> Int -> Env -> Logic (Expression, Type)
makeExpression spec size env
    | (size <= 0) = member baseCases
    | otherwise   = do
        (ClassSpec className _ constructors _) <- member spec
        (ConstructorSpec paramTypes) <- member constructors
        params <- makeExpressionsOfTypes spec (size - 1) env paramTypes
        return (NewExpression className params, ClassType className)
    where baseCases = [(IntLiteral 0, IntType),
            (BooleanLiteral False, BooleanType),
            (StringLiteral "foo", ClassType (ClassName "String"))] ++
            map (\(l, r) -> ((VariableExpression l), r)) (M.toList env)

makeCall :: Spec -> Int -> Env -> Bool -> Logic (Call, Maybe Type)
makeCall spec size env voidOk = do
    (base, ClassType(myClassName)) <- makeExpression spec size env
    methodClassName <- (return myClassName) `interleave` (allParents spec myClassName)
    (ClassSpec _ _ _ methods) <- return $ head $ filter (\(ClassSpec name _ _ _) -> name == methodClassName) spec
    (MethodSpec methodName paramTypes returnType) <- member methods
    if True
        then do
            params <- makeExpressionsOfTypes spec size env paramTypes
            return $ (Call base methodName params, returnType)
        else mzero

makeStatement :: Spec -> Int -> Env -> Int -> Logic (Statement, Env)
makeStatement spec size env statementNum = do
    (initializer, initializerType) <- makeExpression spec size env
    variable <- return $ Variable ("x" ++ (show statementNum))
    variableDeclarationTuple <- return $ ((VariableDeclaration initializerType variable initializer), env)
    (call, _) <- makeCall spec size env True
    callStatementTuple <- return $ (CallStatement call, env)
    member [variableDeclarationTuple, callStatementTuple]
    

makeStatements :: Spec ->  Int -> Env -> Int -> Logic [Statement]
makeStatements spec size env statementNum
    | (statementNum < 0) = mzero
    | statementNum == 0  = return []
    | otherwise          = do
        guard (statementNum > 0)
        (statement, newEnv) <- makeStatement spec size env statementNum
        rest <- makeStatements spec size newEnv (statementNum - 1)
        return $ [statement] ++ rest

makeTest :: Spec -> Int -> Logic JavaTest
makeTest spec size = do
    numStatements <- inInclusiveRange min_num_statements max_num_statements
    statements <- makeStatements spec size M.empty numStatements
    return $ JavaTest statements