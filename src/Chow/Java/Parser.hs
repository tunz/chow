module Chow.Cpp.Parser where

import           Control.Applicative                    ((*>), (<$), (<$>),
                                                         (<*), (<*>))
import           Data.Functor.Identity
import           Data.Tuple                             (swap)
import           Text.Parsec                            (ParsecT)
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Expr
import           Text.ParserCombinators.Parsec.Language
import           Text.ParserCombinators.Parsec.Token    as T

import           Chow.Java.Ast
import           Chow.Token

-- Utilities
parseWildcards :: S -> S
parseWildcards (Function pos id args body) =
  Function pos id parsedArgs parsedBody
  where
    parsedArgs = map tryDeclParser args
    parsedBody = parseExprsInStmt body

parseExprsInStmt :: Stmt -> Stmt
parseExprsInStmt stmt =
  case stmt of
    CompoundStmt s -> CompoundStmt $ map parseExprsInStmt s
    Line wc -> Stmt $ tryDeclOrExprParser $ WildCard wc
    IfStmt pos expr thenbr (Just elsebr) ->
      IfStmt
        pos
        (tryExprParser expr)
        (parseExprsInStmt thenbr)
        (Just $ parseExprsInStmt elsebr)
    IfStmt pos expr thenbr Nothing ->
      IfStmt pos (tryExprParser expr) (parseExprsInStmt thenbr) Nothing
    WhileStmt pos expr thenbr ->
      WhileStmt pos (tryExprParser expr) (parseExprsInStmt thenbr)
    DoWhileStmt pos expr thenbr ->
      DoWhileStmt pos (tryExprParser expr) (parseExprsInStmt thenbr)
    ForStmt pos init cond inc thenbr ->
      ForStmt
        pos
        (tryDeclOrExprParser init)
        (tryExprParser cond)
        (tryExprParser inc)
        (parseExprsInStmt thenbr)
    ForRangeStmt pos decl iterable thenbr ->
      ForRangeStmt
        pos
        (tryDeclParser decl)
        (tryExprParser iterable)
        (parseExprsInStmt thenbr)
    SwitchStmt pos expr blk ->
      SwitchStmt pos (tryExprParser expr) (parseExprsInStmt blk)
    CaseStmt pos expr -> CaseStmt pos $ tryExprParser expr
    ReturnStmt expr -> ReturnStmt $ tryExprParser expr
    rest -> rest

-- Declaration Parsers
tryDeclOrExprParser :: Expr -> Expr
tryDeclOrExprParser (WildCard wc) =
  case parse declParser "" wc of
    Left err   -> tryExprParser $ WildCard wc
    Right expr -> expr
tryDeclOrExprParser rest = rest

tryDeclParser :: Expr -> Expr
tryDeclParser (WildCard wc) =
  case parse declParser "" wc of
    Left err   -> WildCard wc
    Right expr -> expr
tryDeclParser rest = rest

declParser :: TkParser Expr
declParser =
  VarDecl <$>
  (typeDeclParser <*
   lookAhead
     ((tok "*" >> return Nothing) <|> (tok "&" >> return Nothing) <|>
      (getId >> return Nothing))) <*>
  exprParser

isCType s = try $ tok s

pCType :: CType -> [String] -> TkParser CType
pCType ctype keywords = choice (map isCType keywords) >> return ctype

classIdParser :: TkParser String
classIdParser = buildExpressionParser ciTable getId <?> "class id expression"
  where
    ciTable =
      [ [postfixChain templateTypeParser]
      , [binary "::" (\x y -> x ++ "::" ++ y) AssocLeft]
      ]
    templateTypeParser = do
      rest <- map unwrapSymOrIde <$> betweenTok "<" ">"
      return $ \e -> e ++ "<" ++ concat rest ++ ">"
      where
        unwrapSymOrIde (x, _) =
          case x of
            Symbol s -> s
            Ide s    -> s

typeDeclParser :: TkParser CType
typeDeclParser =
  buildExpressionParser tcTable typeParser <?> "type cast expression"
  where
    tcTable =
      [[prefix "const" ConstType]]

-- Note: Actually, char and signed char can be different
typeParser :: TkParser CType
typeParser =
  pCType CharType ["char"] <|>
  pCType ShortType ["short"] <|>
  pCType IntType ["int"] <|>
  pCType LongType ["long"] <|>
  pCType FloatType ["float"] <|>
  pCType DoubleType ["double"] <|>
  pCType BoolType ["boolean"] <|>
  (ClassType <$> classIdParser)

-- Expression Parsers
tryExprParser :: Expr -> Expr
tryExprParser (WildCard wc) =
  case parse exprParser "" wc of
    Left err   -> WildCard wc
    Right expr -> expr
tryExprParser rest = rest

exprParser :: TkParser Expr
exprParser = buildExpressionParser table term <?> "expression"

typeCastParser = do
  tok "("
  t <- typeParser
  tok ")"
  return $ \e -> UnOp (TypeCast t) e

callParser = do
  args <- splitArgs <$> betweenTok "(" ")"
  return $ \e -> Call e (map tryExprParser args)

arrayAccessParser = do
  pos <- getPosition
  idx <- betweenTok "[" "]"
  return $ \e -> ArrayAccess pos e (tryExprParser (WildCard idx))

ternaryCondParser = do
  tok "?"
  trueExpr <- exprParser
  tok ":"
  falseExpr <- exprParser
  return $ \e -> TernaryCond e trueExpr falseExpr

term =
  tok "(" *> exprParser <* tok ")" <|> Var <$> getPosition <*> classIdParser <|>
  uncurry Value <$> getPConst <?> "simple expression"

table =
  [ [ postfixChain callParser -- XXX: ??
    , postfixChain arrayAccessParser
    , binaryPos "." MemberAccess AssocLeft
    ]
  , [ postfix "++" (UnOp PostInc)
    , postfix "--" (UnOp PostDec)
    ]
  , [ prefix "++" (UnOp PreInc)
    , prefix "--" (UnOp PreDec)
    , prefix "!" (UnOp Not)
    , prefix "~" (UnOp Negate)
    , prefix "+" (UnOp Plus)
    , prefix "-" (UnOp Minus)
    ]
  , [ prefixChain (try typeCastParser)
    , prefix "new" (UnOp ObjectCreate)
    ]
  , [ binary "*" (BinOp Mul) AssocLeft
    , binary "/" (BinOp Div) AssocLeft
    , binary "%" (BinOp Mod) AssocLeft
    ]
  , [binary "+" (BinOp Add) AssocLeft, binary "-" (BinOp Sub) AssocLeft]
  , [ binary ">>>" (BinOp BitZeroFillShiftRight) AssocLeft ]
  , [ binary "<<" (BinOp BitShiftLeft) AssocLeft
    , binary ">>" (BinOp BitShiftRight) AssocLeft
    ]
  , [ binary "<" (BinOp Lt) AssocLeft
    , binary "<=" (BinOp Lte) AssocLeft
    , binary ">=" (BinOp Gte) AssocLeft
    , binary ">" (BinOp Gt) AssocLeft
    , prefix "instanceof" (BinOp InstanceOf) AssocLeft
    ]
  , [binary "==" (BinOp Eq) AssocLeft, binary "!=" (BinOp Neq) AssocLeft]
  , [binary "&" (BinOp BitAnd) AssocLeft]
  , [binary "^" (BinOp BitXor) AssocLeft]
  , [binary "|" (BinOp BitOr) AssocLeft]
  , [binary "&&" (BinOp And) AssocLeft]
  , [binary "||" (BinOp Or) AssocLeft]
  , [postfixChain ternaryCondParser]
  , [ binary "=" (Assign NoBinOp) AssocRight
    , binary "+=" (Assign Add) AssocRight
    , binary "-=" (Assign Sub) AssocRight
    , binary "*=" (Assign Mul) AssocRight
    , binary "/=" (Assign Div) AssocRight
    , binary "%=" (Assign Mod) AssocRight
    , binary "<<=" (Assign BitShiftLeft) AssocRight
    , binary ">>=" (Assign BitShiftRight) AssocRight
    , binary "&=" (Assign BitAnd) AssocRight
    , binary "^=" (Assign BitXor) AssocRight
    , binary "|=" (Assign BitOr) AssocRight
    ]
  , [binary "," Comma AssocLeft]
  , [postfix ";" id]
  ]

binaryPos name fun =
  Infix
    (do pos <- getPosition
        tok name
        return $ fun pos)

binary name fun =
  Infix
    (do tok name
        return fun)

prefixChain p = Prefix . chainl1 p $ return (.)

postfixChain p = Postfix . chainl1 p $ return (flip (.))

prefix name fun =
  prefixChain
    (do tok name
        return fun)

postfix name fun =
  postfixChain
    (do tok name
        return fun)

-- Control Flow Parsers
functionParser :: TkParser S
functionParser = do
  typeDeclParser
  pos <- getPosition
  id <- fnNameParser -- TODO: handle XXX::fname
  args <- splitArgs <$> betweenTok "(" ")"
  body <- fnBodyParser
  return $ Function pos id args body
  where
    fnNameParser = try (getId *> tok "::" *> fnNameParser) <|> getId

ifParser :: TkParser Stmt
ifParser = do
  position <- pTok "if"
  cond <- WildCard <$> betweenTok "(" ")"
  thBr <- blockParser
  elseBr <- optionMaybe elseParser
  return $ IfStmt position cond thBr elseBr

elseParser :: TkParser Stmt
elseParser = do
  tok "else"
  ifParser <|> blockParser

whileParser :: TkParser Stmt
whileParser = do
  position <- pTok "while"
  cond <- WildCard <$> betweenTok "(" ")"
  block <- blockParser
  return $ WhileStmt position cond block

doWhileParser :: TkParser Stmt
doWhileParser = do
  position <- pTok "do"
  block <- blockParser
  tok "while"
  cond <- WildCard <$> betweenTok "(" ")"
  tok ";"
  return $ DoWhileStmt position cond block

forParser :: TkParser Stmt
forParser = do
  position <- pTok "for"
  (init:cond:inc:_) <- splitConds <$> betweenTok "(" ")"
  block <- blockParser
  return $ ForStmt position (WildCard init) (WildCard cond) (WildCard inc) block

forRangeParser :: TkParser Stmt
forRangeParser = do
  position <- pTok "for"
  (decl, iterable) <- splitDeclIter <$> betweenTok "(" ")"
  block <- blockParser
  return $ ForRangeStmt position (WildCard decl) (WildCard iterable) block
  where
    splitDeclIter toks =
      case break (\(t, pos) -> t == Symbol ":") toks of
        (f, x:xs) -> (f, xs)
        (f, [])   -> (f, [])

switchParser :: TkParser Stmt
switchParser = do
  position <- pTok "switch"
  cond <- WildCard <$> betweenTok "(" ")"
  block <- blockParser
  return $ SwitchStmt position cond block

caseParser :: TkParser Stmt
caseParser = do
  position <- pTok "case"
  cond <- skipToTokNot ":"
  return $ CaseStmt position (WildCard cond)

caseDefaultParser :: TkParser Stmt
caseDefaultParser = do
  position <- pTok "default"
  tok ":"
  return $ CaseDefaultStmt position

returnParser :: TkParser Stmt
returnParser = do
  tok "return"
  expr <- skipToTok ";"
  return $ ReturnStmt (WildCard expr)

breakParser :: TkParser Stmt
breakParser = tok "break" >> tok ";" >> return BreakStmt

continueParser :: TkParser Stmt
continueParser = tok "continue" >> tok ";" >> return ContinueStmt

gotoParser :: TkParser Stmt
gotoParser = tok "goto" *> getId <* tok ";" >>= \e -> return (GotoStmt e)

labelParser :: TkParser Stmt
labelParser = try $ getId <* tok ":" >>= \e -> return (LabelStmt e)

blockParser :: TkParser Stmt
blockParser = bracesBlockParser <|> stmtBlockParser
  where
    bracesBlockParser = do
      toks <- betweenTok "{" "}"
      case parse stmtsParser "" toks of
        Left err    -> fail "Fail to parse a block"
        Right stmts -> return stmts
    stmtBlockParser = CompoundStmt . (: []) <$> stmtParser

fnBodyParser :: TkParser Stmt
fnBodyParser = do
  toks <- betweenTok "{" "}"
  case parse stmtsParser "" toks of
    Left err    -> fail "Fail to parse a function body"
    Right stmts -> return stmts

stmtsParser :: TkParser Stmt
stmtsParser = CompoundStmt <$> manyTill stmtParser eof

stmtParser :: TkParser Stmt
stmtParser =
  ifParser <|> doWhileParser <|> whileParser <|> try forParser <|>
  try forRangeParser <|>
  switchParser <|>
  caseParser <|>
  caseDefaultParser <|>
  returnParser <|>
  breakParser <|>
  continueParser <|>
  gotoParser <|>
  labelParser <|>
  try (Line <$> skipToTok ";") <|>
  Line <$> manyTill anyTok eof <?> "stmt"

-- Utilities
splitConds :: [TokenPos] -> [[TokenPos]]
splitConds [] = []
splitConds toks = first : splitConds rest
  where
    (first, rest) =
      case break (\(t, pos) -> t == Symbol ";") toks of
        (f, x:xs) -> (f, xs)
        (f, [])   -> (f, [])

-- We need to handle the following cases:
-- fn1(fn2(arg1, arg2), arg3)
-- fn(something<t1, t2>(arg1), arg2)
splitArgs :: [TokenPos] -> [Expr]
splitArgs [] = []
splitArgs toks = WildCard first : splitArgs rest
  where
    (first, rest) = findFirst [] (0, 0) toks
    findFirst acc (_, _) [] = (reverse acc, [])
    findFirst acc (paren, lt) (t:ts) =
      case t of
        (Symbol "(", _) -> findFirst (t : acc) (paren + 1, lt) ts
        (Symbol "<", _) -> findFirst (t : acc) (paren, lt + 1) ts
        (Symbol ")", _) -> findFirst (t : acc) (paren - 1, lt) ts
        (Symbol ">", _) -> findFirst (t : acc) (paren, lt - 1) ts
        (Symbol ",", _)
          | paren /= 0 || lt /= 0 -> findFirst (t : acc) (paren, lt) ts
        (Symbol ",", _) -> (reverse acc, ts)
        _ -> findFirst (t : acc) (paren, lt) ts

-- TODO(tunz): optimize this func?
unwrap :: String -> String -> [TokenPos] -> [TokenPos]
unwrap first last toks =
  case unwrapFirst toks of
    Just ts -> unwrapLast [] ts
    Nothing -> toks
  where
    unwrapFirst ((t, pos):ts) =
      if t == Symbol first
        then Just ts
        else Nothing
    unwrapLast acc [(t, pos)] =
      if t == Symbol last
        then reverse acc
        else toks
    unwrapLast acc (t:ts) = unwrapLast (t : acc) ts
    unwrapLast acc [] = toks

skipTo :: TkParser [TokenPos] -> TkParser [TokenPos]
skipTo p = p <|> ((:) <$> anyTok <*> skipTo p)

skipToTok :: String -> TkParser [TokenPos]
skipToTok t = (: []) <$> tok t <|> ((:) <$> anyTok <*> skipToTok t)

skipToTokNot :: String -> TkParser [TokenPos]
skipToTokNot t = [] <$ tok t <|> ((:) <$> anyTok <*> skipToTokNot t)

advance _ _ ((_, pos):_) = pos
advance pos _ []         = pos

anyTok :: TkParser TokenPos
anyTok = tokenPrim show advance Just

betweenTok first last = unwrap first last <$> betweenTok' first last
  where
    betweenTok' first last = (:) <$> tok first <*> balance first last
    balance f l =
      skipTo $ (: []) <$> tok l <|> (++) <$> betweenTok' f l <*> balance f l

tok :: String -> TkParser TokenPos
tok x = tokenPrim show advance testTok
  where
    testTok (t, pos) =
      if Symbol x == t || Ide x == t
        then Just (t, pos)
        else Nothing

pTok :: String -> TkParser SourcePos
pTok x = tokenPrim show advance testTok
  where
    testTok (t, pos) =
      if Symbol x == t
        then Just pos
        else Nothing

getPId :: TkParser (SourcePos, String)
getPId = tokenPrim show advance testTok
  where
    testTok (Ide id, pos) = Just (pos, id)
    testTok _             = Nothing

getPConst :: TkParser (SourcePos, Const)
getPConst = tokenPrim show advance testTok
  where
    testTok (Const val, pos) = Just (pos, val)
    testTok _                = Nothing

getSym :: TkParser String
getSym = tokenPrim show advance testTok
  where
    testTok (Symbol id, _) = Just id
    testTok _              = Nothing

getId :: TkParser String
getId = tokenPrim show advance testTok
  where
    testTok (Ide id, _) = Just id
    testTok _           = Nothing

getConst :: TkParser Const
getConst = tokenPrim show advance testTok
  where
    testTok (Const val, _) = Just val
    testTok _              = Nothing

-- Tokenization
parsePos :: Parser Token -> Parser TokenPos
parsePos p = fmap swap $ (,) <$> getPosition <*> p

pToken :: Parser TokenPos
pToken =
  choice
    [ parsePos $ Symbol <$> pSymbol
    , parsePos $ Const . Str <$> try cStringLiteral
    , parsePos $ Const . Chr <$> try (T.charLiteral lexer)
    , parsePos $ Ide <$> T.identifier lexer
    , parsePos $ Symbol <$> pOperator
    , parsePos $ Const . IntNum <$> try (T.integer lexer)
    , parsePos $ Const . FloatNum <$> try (T.float lexer)
    , parsePos $ Symbol <$> T.operator lexer
    ]

pTokens :: Parser [TokenPos]
pTokens = ws *> manyTill (pToken <* ws) eof

-- Grammar specification for C++
keywords =
  [ "abstract"
  , "assert"
  , "boolean"
  , "break"
  , "byte"
  , "case"
  , "catch"
  , "char"
  , "class"
  , "const"
  , "continue"
  , "default"
  , "do"
  , "double"
  , "else"
  , "enum"
  , "extends"
  , "final"
  , "finally"
  , "float"
  , "for"
  , "goto"
  , "if"
  , "implements"
  , "import"
  , "instanceof"
  , "int"
  , "interface"
  , "long"
  , "native"
  , "new"
  , "package"
  , "private"
  , "protected"
  , "public"
  , "return"
  , "short"
  , "static"
  , "strictfp"
  , "super"
  , "switch"
  , "synchronized"
  , "this"
  , "throw"
  , "throws"
  , "transient"
  , "try"
  , "void"
  , "volatile"
  , "while"
  ]

ops =
  [ "<<="
  , ">>="
  , "..."
  , "++"
  , "--"
  , "=="
  , "!="
  , ">="
  , "<="
  , "&&"
  , "||"
  , "<<"
  , "+="
  , "-="
  , "*="
  , "%="
  , "/="
  , "&="
  , "^="
  , "|="
  , "->"
  , "::"
  , "="
  , ">"
  , "<"
  , "-"
  , "+"
  , "*"
  , "{"
  , "}"
  , "["
  , "]"
  , "("
  , ")"
  , ","
  , ":"
  , ","
  , ";"
  , "/"
  , "%"
  , "&"
  , "|"
  , "~"
  , "!"
  , "^"
  , "."
  ]

isSymbol s =
  try $ string s <* notFollowedBy alphaNum <* notFollowedBy (char '_')

pSymbol = choice $ map isSymbol keywords

isOperator s = try $ string s

pOperator = choice $ map isOperator ops

-- FIXME: this is simple but dumb
cStringLiteral = char '"' *> manyTill stringChar (char '"')
  where
    stringChar =
      try (string "\\\"" >> return '"') <|> try (string "\\\\" >> return '\\') <|>
      anyChar

cDef :: LanguageDef st
cDef =
  emptyDef
  { commentStart = "/*"
  , commentEnd = "*/"
  , commentLine = "//"
  , identStart = letter <|> char '_'
  , identLetter = alphaNum <|> oneOf "_"
  , reservedOpNames = ops
  , reservedNames = keywords
  }

lexer :: T.GenTokenParser String u Identity
lexer = T.makeTokenParser cDef

lexeme, parens :: ParsecT String u Identity a -> ParsecT String u Identity a
lexeme = T.lexeme lexer

parens = T.parens lexer

symbol :: String -> ParsecT String u Identity String
symbol = T.symbol lexer

reserved, reservedOp :: String -> ParsecT String u Identity ()
reserved = T.reserved lexer

reservedOp = T.reservedOp lexer

ws :: ParsecT String u Identity ()
ws = T.whiteSpace lexer

natural :: ParsecT String u Identity Integer
natural = T.natural lexer

semi, identifier :: ParsecT String u Identity String
semi = T.semi lexer

identifier = T.identifier lexer
