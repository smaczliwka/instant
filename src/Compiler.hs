import AbsInstant
import Data.Set

type Locals = Set Ident
type StackSize = Int

testExp = ExpSub (ExpVar (Ident "e")) (ExpAdd (ExpMul (ExpVar (Ident "b")) (ExpSub (ExpVar (Ident "c")) (ExpLit 3))) (ExpVar (Ident "d")))

countE :: Exp -> Locals -> Either String StackSize

countE (ExpVar id) loc =
    if member id loc then Right 1
    else Left "undefined variable"

countE (ExpLit val) loc = Right 1

countE (ExpAdd exp1 exp2) loc =
    case (countE exp1 loc, countE exp2 loc) of
        (Left error, _) -> Left error
        (Right _, Left error) -> Left error
        (Right val1, Right val2) ->
            if val1 == val2 then Right (val1 + 1)
            else Right (max val1 val2)

countE (ExpSub exp1 exp2) loc =
    case (countE exp1 loc, countE exp2 loc) of
        (Left error, _) -> Left error
        (Right _, Left error) -> Left error
        (Right val1, Right val2) ->
            if val1 == val2 then Right (val1 + 1)
            else Right (max val1 val2)

countE (ExpMul exp1 exp2) loc =
    case (countE exp1 loc, countE exp2 loc) of
        (Left error, _) -> Left error
        (Right _, Left error) -> Left error
        (Right val1, Right val2) ->
            if val1 == val2 then Right (val1 + 1)
            else Right (max val1 val2)

countE (ExpDiv exp1 exp2) loc =
    case (countE exp1 loc, countE exp2 loc) of
        (Left error, _) -> Left error
        (Right _, Left error) -> Left error
        (Right val1, Right val2) ->
            if val1 == val2 then Right (val1 + 1)
            else Right (max val1 val2)

countS :: Stmt -> Locals -> Either String (StackSize, Locals)

countS (SExp exp) loc =
    case (countE exp loc) of
        Left error -> Left error
        Right val -> Right (val, loc)

countS (SAss id exp) loc =
    case (countE exp loc) of
        Left error -> Left error
        Right val -> Right (val, insert id loc)

countP :: Program -> (StackSize, Locals) -> Either String (StackSize, Locals)

countP prog (size, loc) = case prog of
    Prog [] -> Right (size, loc)
    Prog (stmt : rest) -> case countS stmt loc of
        Left error -> Left error
        Right (size', loc') -> countP (Prog rest) (max size size', loc')
