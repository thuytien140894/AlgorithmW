module Lexer where 

    import Text.Parsec
    import Text.Parsec.Language (emptyDef)
    import Text.Parsec.String (Parser)
    import qualified Text.Parsec.Token as Tok
    
    -- | Language definition
    langDef :: Tok.LanguageDef ()
    langDef = Tok.LanguageDef
        { Tok.commentStart    = ""  -- The language doesn't support block comments
        , Tok.commentEnd      = ""
        , Tok.commentLine     = "//"
        , Tok.nestedComments  = False
        , Tok.identStart      = letter 
        , Tok.identLetter     = alphaNum 
        , Tok.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
        , Tok.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
        , Tok.reservedNames   = [ "true"
                                , "false"
                                , "if"
                                , "then"
                                , "else"
                                , "succ"
                                , "pred"
                                , "iszero"
                                , "zero"
                                , "Bool"
                                , "Nat"
                                , "let"
                                , "in" 
                                ]
        , Tok.reservedOpNames = [ "succ"
                                , "pred"
                                , "iszero" 
                                ]
        , Tok.caseSensitive   = True
        }

    -- | Create a lexer
    lexer :: Tok.TokenParser ()
    lexer = Tok.makeTokenParser langDef

    -- | Parse p enclosed in parenthesis, returning the value of p
    parens :: Parser a -> Parser a
    parens = Tok.parens lexer

    -- | Parse dot
    dot :: Parser String
    dot = Tok.dot lexer

    -- | Parse a reserved name.
    reserved :: String -> Parser ()
    reserved = Tok.reserved lexer

    -- | Parse an operator.
    reservedOp :: String -> Parser ()
    reservedOp = Tok.reservedOp lexer

    -- | Parse an identifier.
    identifier :: Parser String
    identifier = Tok.identifier lexer 

    -- | Parse a white space.
    whiteSpace :: Parser ()
    whiteSpace = Tok.whiteSpace lexer