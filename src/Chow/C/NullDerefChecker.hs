module Chow.C.NullDerefChecker where

import           Data.List                         (foldl')
import           Data.Map                          as Map (Map, delete, elemAt,
                                                           empty, filter,
                                                           insert, insertWith,
                                                           intersectionWith,
                                                           lookup, null, union,
                                                           unionWith,
                                                           unionsWith, (!),
                                                           (\\))
import           Data.Maybe
import           Text.Parsec                       (SourcePos)
import           Text.ParserCombinators.Parsec.Pos

import           Chow.C.Ast
import           Chow.C.Parser
import           Chow.Common

-- Four kinds of operations update PtrState:
-- 1. assignment: IsUnknown
-- 2. if condition: IsNull or IsNotNull
-- 3. null ptr deref: IsNotNull
-- 4. merge points: IsNotNull, IsBoth, IsUnknown
--
-- e.g.)
-- if (!ptr) {
--  .. <-- ptr IsNull
--  ptr->elem <-- ptr Deref (alarm)
-- } else {
--  .. <-- ptr IsNotNull
-- }
-- .. <- ptr IsBoth
-- ptr = XX <-- ptr IsUnknown
-- printf("%d %d\n", ptr->elem1,
--        ptr ?
--        ptr->e1 <-- ptr IsNotNull (alarm)
--        : 0);
data PtrState
  = Deref
  | IsNull
  | IsNotNull
  | IsUnknown
  | IsBoth
  deriving (Eq, Show)

type VarName = String

type VarInfo = (VarName, SourcePos)

type VarMap = Map VarName VarInfo

type PtrSet = Map VarInfo (PtrState, SourcePos)

addVarDecl :: Expr -> VarMap -> VarMap
addVarDecl expr varMap =
  case expr of
    VarDecl ctype expr -> addVarDecls expr varMap
    _                  -> varMap
  where
    addVarDecls expr' varMap' =
      case expr' of
        Comma expr1 expr2 ->
          addVarDecls expr1 varMap' `union` addVarDecls expr2 varMap'
        Assign _ expr'' _ -> addVarDecls expr'' varMap'
        UnOp _ expr'' -> addVarDecls expr'' varMap'
        Var pos name -> insert name (name, pos) varMap'
        _ -> varMap'

getVarInfo :: String -> VarMap -> VarInfo
getVarInfo name varMap = fromMaybe (name, dummyPos) $ Map.lookup name varMap
  where
    dummyPos = newPos "" 0 0

-- Merge rules
mergeState state1 state2 =
  case (state1, state2) of
    (Deref, _)             -> Deref
    (IsBoth, x)            -> x
    (IsUnknown, _)         -> IsUnknown
    (IsNull, IsNotNull)    -> IsBoth
    (IsNull, IsNull)       -> IsNull
    (IsNotNull, IsNotNull) -> IsNotNull
    (a, b)                 -> mergeState b a

combinePtrStat (status1, pos1) (status2, pos2) =
  (mergeState status1 status2, max pos1 pos2)

unionPtrSet = unionWith combinePtrStat

unionsPtrSet = unionsWith combinePtrStat

intersectionPtrSet = intersectionWith combinePtrStat

insertPtrSet = insertWith combinePtrStat

data CheckResult
  = Update (VarMap, PtrSet)
  | Continue PtrSet
  | Found Report
  | Stop
  | Reset

emptyPtrSet = Map.empty

(|>>=) :: CheckResult -> (PtrSet -> CheckResult) -> CheckResult
(|>>=) x f =
  case x of
    Found report     -> Found report
    Continue ptrSet1 -> f ptrSet1

(|>>) :: CheckResult -> CheckResult -> CheckResult
(|>>) x y = x |>>= (\ptrSet1 -> y |>>= (Continue . unionPtrSet ptrSet1))

-- TODO: fancy report
newReport var notNullPos belief2 pos2 =
  Found
    ( notNullPos
    , "[Detect]\n" ++
      var ++
      " is dereferenced in " ++
      show notNullPos ++
      "\n" ++
      var ++ " is believed to be '" ++ show belief2 ++ "' in " ++ show pos2)

-- TODO: pointers from complex expressions
checkPtrAccess :: Expr -> VarMap -> PtrSet -> CheckResult
checkPtrAccess expr varMap = checkPtrAccess' expr
  where
    checkPtrAccess' expr ptrSet =
      case expr of
        MemberPtrAccess _ ptrExpr _ -> insertAllPtrs ptrExpr
        UnOp DeRef ptrExpr -> insertAllPtrs ptrExpr
        ArrayAccess _ ptrExpr expr ->
          insertAllPtrs ptrExpr |>> checkPtrAccess' expr emptyPtrSet
        Call name args -> foldl' findAcc (checkPtrAccess' name ptrSet) args
        MemberAccess _ obj _ -> checkPtrAccess' obj ptrSet
        Comma expr1 expr2 -> findAndUnion expr1 expr2
        TernaryCond cond expr1 expr2 ->
          inspectAndMerge' cond (Just expr1, Just expr2)
        BinOp And expr1 expr2 -> inspectAndMerge' expr1 (Just expr2, Nothing)
        BinOp Or expr1 expr2 -> inspectAndMerge' expr1 (Nothing, Just expr2)
        BinOp _ expr1 expr2 -> findAndUnion expr1 expr2
        Assign NoBinOp v expr ->
          findAndUnion v expr |>>= (Continue . delete (getVarInfo' (show v)))
        Assign _ expr1 expr2 -> findAndUnion expr1 expr2
        VarDecl ctype expr -> searchRHSs expr
        _ -> Continue ptrSet
      where
        inspectAndMerge' cond branches =
          inspectAndMerge varMap ptrSet cond branches (flip checkPtrAccess')
        findAndUnion expr1 expr2 =
          checkPtrAccess' expr1 ptrSet |>> checkPtrAccess' expr2 ptrSet
        findAcc (Found r) _ = Found r
        findAcc (Continue acc) x =
          unionIfContinue acc $ checkPtrAccess' x ptrSet
        unionIfContinue _ (Found report)   = Found report
        unionIfContinue ps1 (Continue ps2) = Continue $ unionPtrSet ps1 ps2
        searchRHSs expr =
          case expr of
            Comma expr1 expr2 -> searchRHSs expr1 |>> searchRHSs expr2
            Assign _ _ expr   -> checkPtrAccess' expr ptrSet
            _                 -> Continue ptrSet
        insertAllPtrs expr =
          case expr of
            Var pos x -> trySetDeref (getVarInfo' x) pos
            Comma _ expr -> insertAllPtrs expr
            var@(ArrayAccess pos expr' _) ->
              trySetDeref (getVarInfo' (show var)) pos |>> insertAllPtrs expr'
            var@(MemberPtrAccess pos expr' _) ->
              trySetDeref (getVarInfo' (show var)) pos |>> insertAllPtrs expr'
            UnOp op expr -> insertAllPtrs expr
            _ -> Continue ptrSet
        trySetDeref v pos =
          case Map.lookup v ptrSet of
            Just (IsNotNull, _) -> Continue ptrSet
            Just (Deref, _)     -> Continue ptrSet
            Just (IsUnknown, _) -> Continue $ insert v (Deref, pos) ptrSet
            Nothing             -> Continue $ insert v (Deref, pos) ptrSet
            Just (belief, pos') -> newReport (fst v) pos belief pos'
        getVarInfo' name = getVarInfo name varMap

findAssumptions :: Expr -> Bool -> VarMap -> PtrSet
-- Find guaranteed pointer states
-- e.g.)
--   if (ptr == NULL) { <--
--     .. ptr is NULL here ..
--   } else {
--     .. ptr is not NULL here ..
--   }
findAssumptions cond isTrue varMap = find' cond isTrue emptyPtrSet
  where
    find' cond' isTrue' ptrSet =
      case cond' of
        BinOp Eq expr (Var rpos "NULL") -> find' expr (not isTrue') ptrSet
        BinOp Eq (Var lpos "NULL") expr -> find' expr (not isTrue') ptrSet
        BinOp Neq expr (Var rpos "NULL") -> find' expr isTrue' ptrSet
        BinOp Neq (Var lpos "NULL") expr -> find' expr isTrue' ptrSet
        Var pos v -> setAssumption v pos
        v@(ArrayAccess _ (Var pos _) _) -> setAssumption (show v) pos
        v@(MemberPtrAccess _ (Var pos _) _) -> setAssumption (show v) pos
        v@(MemberAccess _ (Var pos _) _) -> setAssumption (show v) pos
        Comma _ x -> find' x isTrue' ptrSet
        UnOp Not x -> find' x (not isTrue') ptrSet
        BinOp And x y
          | not isTrue' ->
            intersectionPtrSet (find' x False ptrSet) (find' y False ptrSet)
        BinOp And x y -> find' x isTrue' ptrSet `union` find' y isTrue' ptrSet
        BinOp Or x y
          | not isTrue' -> find' x False ptrSet `union` find' y False ptrSet
        BinOp Or x y ->
          intersectionPtrSet (find' x isTrue' ptrSet) (find' y isTrue' ptrSet)
        _ -> ptrSet
      where
        setAssumption v pos =
          insertPtrSet
            (getVarInfo v varMap)
            ( if isTrue'
                then IsNotNull
                else IsNull
            , pos)
            ptrSet

isCheckingNullPtr :: VarMap -> PtrSet -> Expr -> CheckResult
-- Check if a dereferred pointer is compared by NULL in a given boolean
-- expression.
-- e.g.)
--   *ptr = XX;
--   if (ptr == NULL) <--
--     ..
isCheckingNullPtr varMap ptrSet = isCheckingNullPtr'
  where
    isCheckingNullPtr' cond =
      case cond of
        BinOp op cond' (Var _ "NULL")
          | op == Eq || op == Neq -> isCheckingNullPtr' cond'
        BinOp op (Var _ "NULL") cond'
          | op == Eq || op == Neq -> isCheckingNullPtr' cond'
        Var pos v
          | isDeref v -> newReport' v pos
        v@(ArrayAccess pos _ _)
          | isDeref (show v) -> newReport' (show v) pos
        v@(MemberPtrAccess p _ _)
          | isDeref (show v) -> newReport' (show v) p
        v@(MemberAccess pos _ _)
          | isDeref (show v) -> newReport' (show v) pos
        Comma _ x -> isCheckingNullPtr' x
        UnOp Not x -> isCheckingNullPtr' x
        BinOp op x y
          | op == And || op == Or ->
            isCheckingNullPtr' x |>> isCheckingNullPtr' y
        TernaryCond cond' expr1 expr2 -> isCheckingNullPtr' cond'
        _ -> Continue emptyPtrSet
      where
        newReport' v = newReport v (snd (ptrSet ! getVarInfo v varMap)) IsBoth
        isDeref v =
          case Map.lookup (getVarInfo v varMap) ptrSet of
            Just (stat, _) -> stat == Deref
            _              -> False

inspectAndMerge ::
     Show a
  => VarMap
  -> PtrSet
  -> Expr
  -> (Maybe a, Maybe a)
  -> (PtrSet -> a -> CheckResult)
  -> CheckResult
inspectAndMerge varMap ptrSet cond (thenbr, elsebr) runFn =
  isCheckingNullPtr varMap ptrSet cond |>> checkPtrAccess cond varMap ptrSet |>>= \ptrSet' ->
    unionCheckResult
      (runIfJust (thencond `union` ptrSet') thenbr)
      (runIfJust (elsecond `union` ptrSet') elsebr)
  where
    runIfJust ps (Just br) = runFn ps br
    runIfJust ps Nothing   = Continue ps
    thencond = findAssumptions cond True varMap
    elsecond = findAssumptions cond False varMap
    unionCheckResult x y =
      case (x, y) of
        (Continue x, Continue y) -> Continue $ mergePtrs ptrSet [x, y]
        (Found report, _)        -> Found report
        (_, Found report)        -> Found report
        (Continue x, _)          -> Continue $ mergePtrs ptrSet [x]
        (_, Continue x)          -> Continue $ mergePtrs ptrSet [x]
    mergePtrs initPtrSet newPtrSets = allPtrs \\ removedPtrs
      where
        allPtrs = unionsPtrSet (initPtrSet' : newPtrSets')
        removedPtrs = foldl' findRemovedPtrs emptyPtrSet newPtrSets'
        findRemovedPtrs acc x = unionPtrSet acc $ initPtrSet' \\ x
        initPtrSet' = Map.filter ((/= IsUnknown) . fst) initPtrSet
        newPtrSets' = map (Map.filter ((/= IsUnknown) . fst)) newPtrSets

iterateStmts :: Bool -> VarMap -> PtrSet -> [Stmt] -> CheckResult
iterateStmts isSwitch varMap ptrSet = iterateStmts' varMap ptrSet []
  where
    iterateStmts' curVarMap curPtrSet ptrSets [] =
      Continue $ unionsPtrSet (curPtrSet : ptrSets)
    iterateStmts' curVarMap curPtrSet ptrSets (stmt:stmts) =
      case checkNullDeref isSwitch curVarMap curPtrSet stmt of
        Found report -> Found report
        Continue newPtrSet -> iterateStmts' curVarMap newPtrSet ptrSets stmts
        Update (newVarMap, newPtrSet) ->
          iterateStmts' newVarMap newPtrSet ptrSets stmts
        Reset -> iterateStmts' varMap ptrSet (curPtrSet : ptrSets) stmts
        -- Stop | isSwitch -> mergePtrs emptyPtrSet (curPtrSet : ptrSets)
        Stop -> Stop

checkNullDeref :: Bool -> VarMap -> PtrSet -> Stmt -> CheckResult
checkNullDeref isSwitch varMap ptrSet stmt =
  case stmt of
    CompoundStmt stmts -> iterateStmts isSwitch varMap ptrSet stmts
    IfStmt _ cond thbr elbr ->
      inspectAndMerge
        varMap
        ptrSet
        cond
        (Just thbr, elbr)
        (checkNullDeref False varMap)
    WhileStmt _ _ thenbr -> checkNullDeref True varMap ptrSet thenbr
    DoWhileStmt _ _ thenbr -> checkNullDeref True varMap ptrSet thenbr
    ForStmt _ _ _ _ thenbr -> checkNullDeref True varMap ptrSet thenbr
    SwitchStmt _ _ thenbr -> checkNullDeref True varMap ptrSet thenbr
    Stmt varDecl@(VarDecl _ _) ->
      checkPtrAccess varDecl varMap ptrSet |>>= \ptrSet' ->
        Update (addVarDecl varDecl varMap, ptrSet')
    Stmt expr -> checkPtrAccess expr varMap ptrSet
    ReturnStmt _ -> Stop
    BreakStmt
      | isSwitch -> Reset
    BreakStmt -> Stop
    ContinueStmt -> Stop
    GotoStmt _ -> Stop
    LabelStmt _ -> Reset
    CaseStmt _ _ -> Reset
    CaseDefaultStmt _ -> Reset
    _ -> Continue ptrSet

nullDerefChecker :: S -> [Report]
nullDerefChecker (Function pos name args body) =
  case checkNullDeref False varMap emptyPtrSet body of
    Found report -> [report]
    _            -> []
  where
    varMap = foldl' findVarDecls Map.empty args
    findVarDecls acc x = addVarDecl x acc
