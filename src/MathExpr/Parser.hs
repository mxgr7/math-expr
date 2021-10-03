
module MathExpr.Parser where

import Chronos
import MathExpr.Transformations
import MathExpr.Types
import Text.Parsec
import qualified Data.Time.Calendar as D
import Text.Parsec.Expr
import Text.Parsec.Token
import Yahp hiding ((<|>), Product, Sum, Prefix, Infix, and, sum)

mathLangDef :: Monad m => GenLanguageDef String u m
{-# INLINABLE mathLangDef #-}
mathLangDef    = LanguageDef
               { commentStart   = "/*"
               , commentEnd     = "*/"
               , commentLine    = "#"
               , nestedComments = True
               , identStart     = letter <|> char '_'
               , identLetter    = alphaNum <|> oneOf "_'"
               , opStart        = opP
               , opLetter       = opP
               , reservedOpNames= ops
               , reservedNames  = ["true","false"] <> ops
               , caseSensitive  = False
               }
  where ops = fmap unarySymbols unaryPrecedence <> foldMap (toList . binarySymbols) (foldMap snd binaryPrecedence)
        opP = oneOf ":!#$%&*+./<=>?@\\^|-~"


lexer :: Monad m => GenTokenParser String u m
{-# INLINABLE lexer #-}
lexer = makeTokenParser mathLangDef

expr :: Monad m => ParsecT String u m Expr
{-# INLINABLE expr #-}
expr = buildExpressionParser table term <?> "expression"

term :: Monad m => ParsecT String u m Expr
{-# INLINABLE term #-}
term = parens expr
       <|> VarE . VarName . toS <$> identifier
       <|> LiteralE <$> literal
       <|> SetE <$> braces (commaSep expr)
       <|> UnaryE Iverson <$> squares expr
       <?> "term"
  where TokenParser{..} = lexer
        literal = Text.Parsec.try (DateL <$> dateParserYmd)
          <|> NumberL . either fromInteger id <$> naturalOrFloat
          <|> StringL . toS <$> stringLiteral
          <|> BoolL <$> (True <$ reserved "true"
                          <|> False <$ reserved "false")

number :: Monad m => Int -> ParsecT String u m Int
number size = do
  digits <- count size digit
  let n = foldl (\x d -> 10*x + digitToInt d) 0 digits
  seq n $ pure n

dateParserYmd :: Monad m => ParsecT String u m Day
dateParserYmd = do
  y <- number 4 
  char '-'
  m <- number 2
  char '-'
  d <- number 2
  pure $ fromBaseDay $ D.fromGregorian (toInteger y) m d


table :: Monad m => [[Operator String u m Expr]]
{-# INLINABLE table #-}
table = [[ Prefix $ UnaryE op <$ reservedOp (unarySymbols op) | op <- unaryPrecedence]]
  <> fmap (fmap (\(assoc, op) ->
                   Infix (BinaryE op <$ choice (reservedOp <$> toList (binarySymbols op))) assoc) . sequence) binaryPrecedence
  where TokenParser{..} = lexer
  -- where bin 

unaryPrecedence :: [UnaryOperator]
unaryPrecedence = [Not, Negate]

binaryPrecedence :: [(Assoc, [BinaryOperator])]
binaryPrecedence = fmap3 Base [(AssocLeft, [Product,Quotient])

         ,(AssocLeft, [Sum, Difference])

         ,(AssocNone, [Less ,LessEqual ,Greater ,GreaterEqual])

         ,(AssocNone, [Equal ,NotEqual])

         ,(AssocRight, [And])

         ,(AssocRight, [Xor])

         ,(AssocRight, [Or])

         ,(AssocRight, [Intersection])

         ,(AssocRight, [Union, SetDifference])
           
         ,(AssocNone, [Element, NotElement])]
  <> fmap3 Extra
         [(AssocNone,   [StepHeight])

         ,(AssocRight,  [StepConcat])

         ,(AssocNone,   [StepFunction])
         ]


parseExpr :: String -> Either ParseError Expr
{-# INLINABLE parseExpr #-}
parseExpr = parse fullExprParser ""

fullExprParser :: Monad m => ParsecT String u m Expr
{-# INLINABLE fullExprParser #-}
fullExprParser = ws *> expr <* (ws >> eof)
  where ws = whiteSpace lexer

parseAndDesugarExpr :: Error m => String -> m DesugaredExpr
{-# INLINABLE parseAndDesugarExpr #-}
parseAndDesugarExpr = either (throwError . shot) desugarStepFunctions . parseExpr


