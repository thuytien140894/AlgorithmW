module Parser (
  parseExpr
  ) where 

  import Lexer
  import Syntax
  import Types 
  import ParseUtils

  import Text.Parsec
  import Text.Parsec.String (Parser)
  import qualified Text.Parsec.Expr as Ex
  import Data.Functor.Identity

  -- if statement
  conditional :: Parser Expr
  conditional = do
    reserved "if"
    cond <- expr
    reserved "then"
    tr <- expr
    reserved "else"
    fl <- expr
    return $ If cond tr fl

  -- abstraction
  lambda :: Parser Expr
  lambda = do
    reservedOp "\\" >> whiteSpace
    arg <- identifier -- if there is type specified, parse it; else return Dyn
    dot 
    body <- expr
    let ty = Dyn
    let t = fixBinding body arg 0
    let t' = updateVarType t arg ty
    let boundVars = arg : getBoundVar body
    let freeVars = getFreeVar body boundVars
    let t'' = fixFreeBinding t' freeVars boundVars
    return $ Lambda ty t'' boundVars

  -- variable
  var :: Parser Expr
  var = Var (-1) TUnit <$> identifier -- the variable is first parsed as free

  -- Constants
  true, false, zero :: Parser Expr
  true  = reserved "true" >> return Tru
  false = reserved "false" >> return Fls
  zero  = reserved "0" >> return Zero

  -- apply two terms that are separated by a space
  app :: Parser Expr
  app = do
    terms <- sepBy1 expr' whiteSpace 
    return $ applyFromLeft terms

  -- parse an application which consists a sequence of terms
  expr :: Parser Expr
  expr = app 

  -- Prefix operators
  prefixTable :: Ex.OperatorTable String () Identity Expr
  prefixTable = [
      [
        Ex.Prefix $ reserved "succ"   >> return Succ
      , Ex.Prefix $ reserved "pred"   >> return Pred
      , Ex.Prefix $ reserved "iszero" >> return IsZero
      ]
    ]

  -- parse an arithmetic expression such as succ, pred, and iszero
  expr' :: Parser Expr
  expr' = Ex.buildExpressionParser prefixTable expr''

  -- parse Expr enclosed in parenthesis
  parenExpr :: Parser Expr
  parenExpr = parens expr

  -- parse individual terms
  expr'' :: Parser Expr
  expr'' = parenExpr
      <|> true
      <|> false
      <|> zero
      <|> var
      <|> lambda
      <|> conditional

  -- remove the initial whitespace, line comments, and block comments 
  -- the parser only removes white spaces after the tokens
  removeWhiteSpace :: Parser Expr
  removeWhiteSpace = whiteSpace >> expr 

  -- parse a string
  parseExpr :: String -> Either ParseError Expr
  parseExpr = parse removeWhiteSpace "" 

