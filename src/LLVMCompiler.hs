module LLVMCompiler(compile) where

import AbsInstant
import Data.Map

type Locals = Map Ident String
type Next = Integer
type Result = String
type Code = ShowS

register :: Integer -> String
register n = showChar '%' (show n)

testExp = (ExpSub (ExpLit 1) (ExpAdd (ExpMul (ExpLit 2) (ExpSub (ExpLit 3) (ExpLit 4))) (ExpLit 5)))
testEvalE :: Exp -> String
testEvalE exp = case evalE exp empty 0 of
    Left error -> error
    Right (res, next, code) -> code ""

testStmt = SAss (Ident "x") (ExpSub (ExpLit 1) (ExpAdd (ExpMul (ExpLit 2) (ExpSub (ExpLit 3) (ExpLit 4))) (ExpLit 5)))
testEvalS :: Stmt -> String
testEvalS stmt = case evalS stmt empty 0 of
    Left error -> error
    Right (loc, next, code) -> code ""

evalE :: Exp -> Locals -> Next -> Either String (Result, Next, Code)

evalE (ExpVar id) loc next =
    if member id loc then Right (loc ! id, next, showString "")
    else Left "undefined variable"

evalE (ExpLit val) loc next = Right (show val, next, showString "")

evalE (ExpAdd exp1 exp2) loc next =
    case evalE exp1 loc next of
        Left error -> Left error
        Right (res1, next1, code1) ->
            case evalE exp2 loc next1 of
                Left error -> Left error
                Right (res2, next2, code2) ->
                    let code = code1 . code2 . showString (register next2) . showString " = add i32 " . showString res1 . showString ", " . showString res2 . showChar '\n' in
                        Right (register next2, next2 + 1, code)
                    
evalE (ExpSub exp1 exp2) loc next =
    case evalE exp1 loc next of
        Left error -> Left error
        Right (res1, next1, code1) ->
            case evalE exp2 loc next1 of
                Left error -> Left error
                Right (res2, next2, code2) ->
                    let code = code1 . code2 . showString (register next2) . showString " = sub i32 " . showString res1 . showString ", " . showString res2 . showChar '\n' in
                        Right (register next2, next2 + 1, code)

evalE (ExpMul exp1 exp2) loc next =
    case evalE exp1 loc next of
        Left error -> Left error
        Right (res1, next1, code1) ->
            case evalE exp2 loc next1 of
                Left error -> Left error
                Right (res2, next2, code2) ->
                    let code = code1 . code2 . showString (register next2) . showString " = mul i32 " . showString res1 . showString ", " . showString res2 . showChar '\n' in
                        Right (register next2, next2 + 1, code)

evalE (ExpDiv exp1 exp2) loc next =
    case evalE exp1 loc next of
        Left error -> Left error
        Right (res1, next1, code1) ->
            case evalE exp2 loc next1 of
                Left error -> Left error
                Right (res2, next2, code2) ->
                    let code = code1 . code2 . showString (register next2) . showString " = sdiv i32 " . showString res1 . showString ", " . showString res2 . showChar '\n' in
                        Right (register next2, next2 + 1, code)

evalS :: Stmt -> Locals -> Next -> Either String (Locals, Next, Code)

evalS (SExp exp) loc next =
    case evalE exp loc next of
        Left error -> Left error
        Right (res, next', code) ->
            let code' = code . showString "call void @printInt(i32 " . showString res . showString ")\n" in
                Right (loc, next', code')

evalS (SAss id exp) loc next =
    case evalE exp loc next of
        Left error -> Left error
        Right (res, next', code) -> Right (insert id res loc, next', code)

evalP :: Program -> (Locals, Next, Code) -> Either String (Locals, Next, Code)

evalP prog (loc, next, code) = case prog of
    Prog [] -> Right (loc, next, code)
    Prog (stmt : rest) -> case evalS stmt loc next of
        Left error -> Left error
        Right (loc', next', code') -> evalP (Prog rest) (loc', next', code . code')

prologue :: Code
prologue = showString
    "@dnl = internal constant [4 x i8] c\"%d\\0A\\00\"\n\n\
    \declare i32 @printf(i8*, ...)\n\n\
    \define void @printInt(i32 %x) {\n\
    \    %t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0\n\
    \    call i32 (i8*, ...) @printf(i8* %t0, i32 %x)\n\
    \    ret void\n\
    \}\n\n\
    \define i32 @main() {\n\
    \entry:\n"

epilogue :: String
epilogue = "    ret i32 0\n}"

compile :: Program -> Either String String
compile prog = case evalP prog (empty, 0, showString "") of
    Left error -> Left error
    Right (_, _, code) -> Right ((prologue . code) epilogue)
