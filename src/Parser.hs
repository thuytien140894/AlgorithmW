module Parser 
    ( parseExpr
    ) where 

    import Lexer
    import Syntax

    import Data.Functor.Identity
    import Text.Parsec
    import qualified Text.Parsec.Expr as Ex
    import Text.Parsec.String (Parser)

    -- | Application.
    apply :: Expr -> Expr -> Expr
    apply Unit t2 = t2
    apply t1 t2   = App t1 t2

    -- | Recursively apply terms from the left.
    applyFromLeft :: [Expr] -> Expr
    applyFromLeft = foldl apply Unit

    -- | Parse an if statement.
    conditional :: Parser Expr
    conditional = do
        reserved "if"
        cond <- expr
        reserved "then"
        tr <- expr
        reserved "else"
        fl <- expr
        return $ If cond tr fl

    -- | Parse a let expression.
    letExpr :: Parser Expr 
    letExpr = do 
        reserved "let"
        x <- identifier
        reservedOp "=" 
        val <- expr
        reserved "in"
        body <- expr
        return $ Let x val body

    -- | Parse an abstraction.
    lambda :: Parser Expr
    lambda = do
        reservedOp "\\" 
        x <- identifier 
        dot 
        body <- expr
        return $ Lambda x body

    -- | Parse a variable.
    var :: Parser Expr
    var = Var <$> identifier 

    -- | Parse constants.
    true, false, zero :: Parser Expr
    true  = reserved "true" >> return Tru
    false = reserved "false" >> return Fls
    zero  = reserved "0" >> return Zero

    -- | Apply two terms that are separated by a space.
    app :: Parser Expr
    app = applyFromLeft <$> sepBy1 expr' whiteSpace

    -- | Parse an application which consists a sequence of terms.
    expr :: Parser Expr
    expr = app 

    -- | Prefix operators.
    prefixTable :: Ex.OperatorTable String () Identity Expr
    prefixTable = [ [ Ex.Prefix $ reserved "succ"   >> return Succ
                    , Ex.Prefix $ reserved "pred"   >> return Pred
                    , Ex.Prefix $ reserved "iszero" >> return IsZero
                    ]
                  ]

    -- | Parse an arithmetic expression such as succ, pred, and iszero.
    expr' :: Parser Expr
    expr' = Ex.buildExpressionParser prefixTable expr''

    -- | Parse individual terms.
    expr'' :: Parser Expr
    expr'' = parens expr
        <|> true
        <|> false
        <|> zero
        <|> var
        <|> lambda
        <|> conditional
        <|> letExpr

    -- | Parse a string.
    parseExpr :: String -> Either ParseError Expr
    parseExpr = parse (whiteSpace >> expr) "" 