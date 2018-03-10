module Chow.C.Ast where

import Text.Parsec
import Data.List

import Chow.Token

data Expr = WildCard [TokenPos]
          | Call Expr [Expr]
          | TernaryCond Expr Expr Expr
          | ArrayAccess SourcePos Expr Expr
          | MemberAccess SourcePos Expr Expr
          | MemberPtrAccess SourcePos Expr Expr
          | Comma Expr Expr
          | UnOp UnaryOperator Expr
          | BinOp BinaryOperator Expr Expr
          | Assign BinaryOperator Expr Expr
          | Var SourcePos String
          | Value SourcePos Const
          | VarDecl CType Expr
          deriving (Eq)

data CType = LongType | UnsignedLongType
           | LongLongType | UnsignedLongLongType
           | IntType | UnsignedIntType
           | ShortType | UnsignedShortType
           | CharType | UnsignedCharType
           | DoubleType | LongDoubleType | FloatType
           | VoidType
           | StructType String
           | PtrType CType
           | RefType CType
           | ConstType CType
           deriving (Show, Eq)

data UnaryOperator = Plus | Minus | Negate | Not
                   | PostInc | PostDec | PreInc | PreDec
                   | DeRef | AddrOf | SizeOf | TypeCast CType
                   deriving (Eq)

data BinaryOperator = Add | Sub | Mul | Div | Mod
                    | Eq | Neq | Gt | Gte | Lt | Lte
                    | BitShiftLeft | BitShiftRight
                    | BitAnd | BitXor | BitOr
                    | And | Or | NoBinOp
                    deriving (Eq)

data Stmt = CompoundStmt [Stmt]
          | Stmt Expr
          | Line [TokenPos]
          | IfStmt SourcePos Expr Stmt (Maybe Stmt)
          | WhileStmt SourcePos Expr Stmt
          | DoWhileStmt SourcePos Expr Stmt
          | ForStmt SourcePos Expr Expr Expr Stmt
          | SwitchStmt SourcePos Expr Stmt
          | CaseStmt SourcePos Expr
          | CaseDefaultStmt SourcePos
          | GotoStmt String
          | LabelStmt String
          | ContinueStmt
          | BreakStmt
          | ReturnStmt Expr
          deriving (Eq)

data S = Function SourcePos String [Expr] Stmt

instance Show UnaryOperator where
  show Plus = "+"
  show Minus = "-"
  show Negate = "~"
  show Not = "!"
  show PostInc = "(++)"
  show PostDec = "(--)"
  show PreInc = "++"
  show PreDec = "--"
  show DeRef = "*"
  show AddrOf = "&"
  show SizeOf = "sizeof"
  show (TypeCast ctype) = "(" ++ show ctype ++ ")"

instance Show BinaryOperator where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Mod = "%"
  show Eq = "=="
  show Neq = "!="
  show Gt = ">"
  show Gte = ">="
  show Lt = "<"
  show Lte = "<="
  show BitShiftLeft = "<<"
  show BitShiftRight = ">>"
  show BitAnd = "&"
  show BitXor = "^"
  show BitOr = "|"
  show And = "&&"
  show Or = "||"
  show NoBinOp = ""

instance Show Expr where
  show (WildCard toks) = show $ map fst toks
  show (Call id args) = "Call " ++ show id ++ "(" ++ show args ++ ")"
  show (TernaryCond cond trueExpr falseExpr) =
    show cond ++ " ? " ++ show trueExpr ++ " : " ++ show falseExpr
  show (ArrayAccess pos id idx) = show id ++ "[" ++ show idx ++ "]"
  show (MemberAccess pos expr1 expr2) = show expr1 ++ "." ++ show expr2
  show (MemberPtrAccess pos expr1 expr2) = show expr1 ++ "->" ++ show expr2
  show (Comma expr1 expr2) = show expr1 ++ ", " ++ show expr2
  show (UnOp uop expr) = show uop ++ show expr
  show (BinOp bop expr1 expr2) =
    show expr1 ++ " " ++ show bop ++ " " ++ show expr2
  show (Assign bop expr1 expr2) =
    show expr1 ++ " " ++ show bop ++ "= " ++ show expr2
  show (Var pos v) = v
  show (Value pos v) = show v
  show (VarDecl ctype expr) = show ctype ++ " " ++ show expr

indent depth = concat (replicate (depth*2) " ")

prettyShowStmt depth ContinueStmt = "Continue"
prettyShowStmt depth BreakStmt = "Break"
prettyShowStmt depth (ReturnStmt expr) = "Return " ++ show expr
prettyShowStmt depth (Stmt expr) = show expr
prettyShowStmt depth (CaseStmt pos expr) = "Case " ++ show expr ++ ":"
prettyShowStmt depth (CaseDefaultStmt pos) = "CaseDefault:"
prettyShowStmt depth (GotoStmt label) = "goto " ++ label
prettyShowStmt depth (LabelStmt label) = label ++ ": "

prettyShowStmt depth (IfStmt pos cond thenbr elsebr) =
  "if (" ++ show cond ++ ")" ++ "\n"
  ++ prettyShowStmt depth thenbr
  ++ (case elsebr of
        Just (CompoundStmt blk) ->
          "\n" ++ indent depth ++ "else\n"
          ++ prettyShowStmt depth (CompoundStmt blk)
        Just blk ->
          "\n" ++ indent depth ++ "else " ++ prettyShowStmt depth blk -- IfStmt
        Nothing -> "")

prettyShowStmt d (CompoundStmt stmts) =
  indent d ++ "{\n" ++ indent (d+1)
  ++ intercalate ("\n" ++ indent (d+1)) (map (prettyShowStmt (d+1)) stmts)
  ++ "\n" ++ indent d ++ "}"

prettyShowStmt depth (Line toks) = show $ map fst toks

prettyShowStmt depth (WhileStmt pos cond block) =
  "while (" ++ show cond ++ ")" ++ "\n"
  ++ prettyShowStmt depth block

prettyShowStmt depth (DoWhileStmt pos cond block) =
  "do\n" ++ prettyShowStmt depth block ++ " while (" ++ show cond ++ ")"

prettyShowStmt depth (ForStmt pos init cond inc block) =
  "for ("
  ++ show init ++ "; "
  ++ show cond ++ "; "
  ++ show inc ++ ")\n"
  ++ prettyShowStmt depth block

prettyShowStmt depth (SwitchStmt pos cond block) =
  "switch (" ++ show cond ++ ")\n" ++ prettyShowStmt depth block

instance Show Stmt where
  show = prettyShowStmt 0

instance Show S where
  show (Function pos id args body) =
    "\nFunction <" ++ id ++ "> " ++ show pos ++ " \n"
    ++ "Args: " ++ show args ++ "\n"
    ++ show body ++ "\n"
