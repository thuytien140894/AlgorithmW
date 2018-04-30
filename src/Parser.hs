module Parser 
    ( parseExpr
    ) where 

    import Lexer
    import ParseUtils
    import Syntax
    import Type 

    import Data.Functor.Identity
    import Text.Parsec
    import qualified Text.Parsec.Expr as Ex
    import Text.Parsec.String (Parser)

    -- | Parse an if statement
    conditional :: Parser Expr
    conditional = do
        reserved "if"
        cond <- expr
        reserved "then"
        tr <- expr
        reserved "else"
        fl <- expr
        return $ If cond tr fl

    -- | Parse an abstraction
    lambda :: Parser Expr
    lambda = do
        reservedOp "\\" >> whiteSpace
        arg <- identifier -- if there is type specified, parse it; else return Dyn
        dot 
        body <- expr
        let t = fixBinding body arg 0
        let boundVars = arg : getBoundVar body
        let freeVars = getFreeVar body boundVars
        let t' = fixFreeBinding t freeVars boundVars
        return $ Lambda t' boundVars

    -- | Parse a variable
    var :: Parser Expr
    var = Var (-1) <$> identifier -- the variable is first parsed as free

    -- | Parse constants
    true, false, zero :: Parser Expr
    true  = reserved "true" >> return Tru
    false = reserved "false" >> return Fls
    zero  = reserved "0" >> return Zero

    -- | Apply two terms that are separated by a space
    app :: Parser Expr
    app = applyFromLeft <$> sepBy1 expr' whiteSpace

    -- | Parse an application which consists a sequence of terms
    expr :: Parser Expr
    expr = app 

    -- | Prefix operators
    prefixTable :: Ex.OperatorTable String () Identity Expr
    prefixTable = [ [ Ex.Prefix $ reserved "succ"   >> return Succ
                    , Ex.Prefix $ reserved "pred"   >> return Pred
                    , Ex.Prefix $ reserved "iszero" >> return IsZero
                    ]
                  ]

    -- | Parse an arithmetic expression such as succ, pred, and iszero
    expr' :: Parser Expr
    expr' = Ex.buildExpressionParser prefixTable expr''

    -- | Parse individual terms
    expr'' :: Parser Expr
    expr'' = parens expr
        <|> true
        <|> false
        <|> zero
        <|> var
        <|> lambda
        <|> conditional

    -- | Parse a string
    parseExpr :: String -> Either ParseError Expr
    parseExpr = parse (whiteSpace >> expr) "" 

