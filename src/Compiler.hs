module Compiler(compile) where

import AbsInstant
import Data.Map

type LocSize = Integer
type StackSize = Integer
type Locals = (Map Ident Integer, LocSize)

newLoc :: Ident -> Locals -> Locals
newLoc id (map, size) =
    if member id map then (map, size)
    else (insert id (size + 1) map, size + 1)

exists :: Ident -> Locals -> Bool
exists id (map, _) = member id map

number :: Ident -> Locals -> Integer
number id (map, _) = map ! id

type Code = ShowS

pushConst :: Integer -> Code
pushConst val =
    if val == -1 then showString "iconst_m1"
    else if val >= 0 && val <= 5 then showString "iconst_" . shows val
    else if val >= -128 && val <= 127 then showString "bipush " . shows val
    else if val >= -32768 && val <= 32767 then showString "sipush " . shows val
    else showString "ldc " . shows val

loadVar :: Ident -> Locals -> Code
loadVar id loc =
    let num = number id loc in
        if num <= 3 then showString "iload_" . shows num
        else showString "iload " . shows num

evalE :: Exp -> Locals -> Either String (StackSize, Code)

evalE (ExpVar id) loc =
    if exists id loc then Right (1, loadVar id loc . showChar '\n')
    else Left "undefined variable"

evalE (ExpLit val) loc = Right (1, pushConst val . showChar '\n')

evalE (ExpAdd exp1 exp2) loc =
    case (evalE exp1 loc, evalE exp2 loc) of
        (Left error, _) -> Left error
        (Right _, Left error) -> Left error
        (Right (size1, code1), Right (size2, code2)) ->
            if size1 == size2 then Right (size1 + 1, code1 . code2 . showString "iadd\n")
            else if size1 > size2 then Right (size1, code1 . code2 . showString "iadd\n")
            else Right (size2, code2 . code1 . showString "iadd\n")

evalE (ExpSub exp1 exp2) loc =
    case (evalE exp1 loc, evalE exp2 loc) of
        (Left error, _) -> Left error
        (Right _, Left error) -> Left error
        (Right (size1, code1), Right (size2, code2)) ->
            if size1 == size2 then Right (size1 + 1, code1 . code2 . showString "isub\n")
            else if size1 > size2 then Right (size1, code1 . code2 . showString "isub\n")
            else Right (size2, code2 . code1 . showString "swap\nisub\n")

evalE (ExpMul exp1 exp2) loc =
    case (evalE exp1 loc, evalE exp2 loc) of
        (Left error, _) -> Left error
        (Right _, Left error) -> Left error
        (Right (size1, code1), Right (size2, code2)) ->
            if size1 == size2 then Right (size1 + 1, code1 . code2 . showString "imul\n")
            else if size1 > size2 then Right (size1, code1 . code2 . showString "imul\n")
            else Right (size2, code2 . code1 . showString "imul\n")

evalE (ExpDiv exp1 exp2) loc =
    case (evalE exp1 loc, evalE exp2 loc) of
        (Left error, _) -> Left error
        (Right _, Left error) -> Left error
        (Right (size1, code1), Right (size2, code2)) ->
            if size1 == size2 then Right (size1 + 1, code1 . code2 . showString "idiv\n")
            else if size1 > size2 then Right (size1, code1 . code2 . showString "idiv\n")
            else Right (size2, code2 . code1 . showString "swap\nidiv\n")

printExp :: Code -> Code
printExp code = showString "getstatic java/lang/System/out Ljava/io/PrintStream;\n"
    . code . showString "invokevirtual java/io/PrintStream/println(I)V\n"

storeVar :: Ident -> Locals -> Code
storeVar id loc =
    let num = number id loc in
        if num <= 3 then showString "istore_" . shows num
        else showString "istore " . shows num

evalS :: Stmt -> Locals -> Either String (StackSize, Locals, Code)

evalS (SExp exp) loc =
    case (evalE exp loc) of
        Left error -> Left error
        Right (size, code) ->
            Right (size + 1, loc, printExp code)

evalS (SAss id exp) loc =
    case (evalE exp loc) of
        Left error -> Left error
        Right (size, code) ->
            let loc' = newLoc id loc in
                Right (size, loc', code . storeVar id loc' . showChar '\n')

evalP :: Program -> (StackSize, Locals, Code) -> Either String (StackSize, Locals, Code)

evalP prog (size, loc, code) = case prog of
    Prog [] -> Right (size, loc, code)
    Prog (stmt : rest) -> case evalS stmt loc of
        Left error -> Left error
        Right (size', loc', code') -> evalP (Prog rest) (max size size', loc', code . code')

prologue :: String -> StackSize -> LocSize -> Code
prologue className stackSize locSize =
    showString ".class public " . showString className . showString "\n\
    \.super java/lang/Object\n\n\
    \.method public <init>()V\n\
    \    aload_0\n\
    \    invokespecial java/lang/Object/<init>()V\n\
    \    return\n\
    \.end method\n\n\
    \.method public static main([Ljava/lang/String;)V\n\
    \.limit locals " . shows locSize . showString "\n\
    \.limit stack " . shows stackSize . showString "\n"

epilogue :: String
epilogue = "return\n.end method\n"

compile :: String -> Program -> Either String String
compile className prog = case evalP prog (0, (empty, 0), showString "") of
    Left error -> Left error
    Right (stackSize, (_, locSize), code) ->
        Right ((prologue className stackSize (locSize + 1) . code) epilogue)
