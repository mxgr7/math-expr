
module MathExpr.Types where

import           Data.HashMap.Strict ()
import qualified Data.Text as T
import           Yahp hiding (Sum, Product)
import           Chronos

type Error m = MonadError Text m

newtype VarName = VarName { unVarName :: Text }
  deriving (Show, Read, Eq, Hashable)

data Expr' a = VarE VarName
          | LiteralE Literal
          | UnaryE UnaryOperator (Expr' a)
          | BinaryE (BinaryOperator' a) (Expr' a) (Expr' a)
          | SetE [Expr' a]
  deriving (Read, Eq)

data UnaryOperator = Not | Negate | Iverson
  deriving (Bounded, Enum, Show, Read, Eq)

data BinaryOperator' a = Base BaseOperator | Extra a
  deriving (Show, Read, Eq)

data BaseOperator = Sum | Difference | Product | Quotient                             -- arithmetics
                    | And | Or | Xor                                                    -- boolean
                    | Element | NotElement | Intersection | Union | SetDifference       -- sets
                    | Less | LessEqual | Equal | NotEqual | Greater | GreaterEqual      -- comparisons
  deriving (Show, Read, Eq)


data StepFunctionsOperators = StepFunction | StepConcat | StepHeight                            -- step functions
  deriving (Bounded, Enum, Show, Read, Eq)

type Expr = Expr' StepFunctionsOperators
type BinaryOperator = BinaryOperator' StepFunctionsOperators
type DesugaredExpr = Expr' Void

data Literal = BoolL Bool
             | NumberL Double
             | StringL Text
             | DateL Day
  deriving (Show, Read, Eq)

instance HasBinarySymbols a => Show (Expr' a) where
  show = toS . pretty

pretty :: HasBinarySymbols a => Expr' a -> Text
pretty = \case
  VarE          v -> unVarName v
  LiteralE      l -> case l of
    BoolL    b -> shot b
    NumberL  b -> shot b
    StringL  b -> shot b
    DateL    b -> shot b
  SetE          l -> "{" <> (T.intercalate ", " $ pretty <$> toList l) <> "}"
  UnaryE op e  -> case op of
    Not         -> unarySymbols op <> parens e
    Negate      -> unarySymbols op <> parens e
    Iverson     -> "[" <> pretty e <> "]"
  BinaryE op e1 e2 -> parens e1 <> " " <> head (binarySymbols op) <> " " <> parens e2
  where parens          e = case e of
          UnaryE Iverson _ -> wop
          UnaryE _ _    -> withp
          BinaryE _ _ _ -> withp
          _ -> wop
          where (wop, withp) = (pretty e, "(" <> wop <> ")")

class HasBinarySymbols a where
  extraBinarySymbols :: IsString t => a -> NonEmpty t

instance HasBinarySymbols Void where
  extraBinarySymbols = \case
    
instance HasBinarySymbols StepFunctionsOperators where
  extraBinarySymbols = \case
    StepFunction              -> "?"  :| []
    StepHeight                -> ":"  :| []
    StepConcat                -> ";"  :| []
  

binarySymbols :: (IsString a, HasBinarySymbols b) => BinaryOperator' b -> NonEmpty a
{-# INLINABLE binarySymbols #-}
binarySymbols (Base op) = case op of
              Product                   -> "*"  :| []
              Quotient                  -> "/"  :| []
              Sum                       -> "+"  :| []
              Difference                -> "-"  :| []
              Less                      -> "<"  :| []
              LessEqual                 -> "≤"  :| ["<="]
              Greater                   -> ">"  :| []
              GreaterEqual              -> "≥" :| [">="]
              Equal                     -> "="  :| []
              NotEqual                  -> "≠"  :| ["/="]
              And                       -> "&"  :| []
              Xor                       -> "^"  :| []
              Or                        -> "|"  :| []
              Intersection              -> "∩"  :| ["cap"]
              Union                     -> "∪"  :| ["cup"]
              SetDifference             -> "\\" :| []
              Element                   -> "∈"  :| ["in"]
              NotElement                -> "∉"  :| ["notIn"]
binarySymbols (Extra op) = extraBinarySymbols op



unarySymbols :: IsString a => UnaryOperator -> a
{-# INLINABLE unarySymbols #-}
unarySymbols = \case
  Not           -> "~"
  Negate        -> "-"
  Iverson       -> "Iverson"

-- * Helpers

numE :: Double -> Expr' e
numE             = LiteralE . NumberL

prodE :: Eq a => Expr' a -> Expr' a -> Expr' a
prodE a b | a == oneE                   = b
          | b == oneE                   = a
          | a == nullE || b == nullE    = nullE
          | True                        = BinaryE (Base Product) a b


-- pattern NullE = LiteralE (NumberL 0)
-- pattern OneE = LiteralE (NumberL 1)

nullE :: Expr' e
nullE = numE 0

oneE :: Expr' e
oneE = numE 1

iversonE :: Expr' a -> Expr' a
iversonE         = UnaryE Iverson

andE :: Expr' a -> Expr' a -> Expr' a
andE             = BinaryE $ Base And

greaterEqualE :: Expr' a -> Expr' a -> Expr' a
greaterEqualE    = BinaryE $ Base GreaterEqual

lessE :: Expr' a -> Expr' a -> Expr' a
lessE            = BinaryE $ Base Less

quotientE :: Expr' a -> Expr' a -> Expr' a
quotientE             = BinaryE $ Base Quotient

negateE :: Expr' a -> Expr' a
negateE             = UnaryE Negate

differenceE :: Expr' a -> Expr' a -> Expr' a
differenceE             = BinaryE $ Base Difference

sumE :: Expr' a -> Expr' a -> Expr' a
sumE             = BinaryE $ Base Sum

variableNames :: Expr' a -> [VarName]
variableNames = \case
  VarE          v -> [v]
  LiteralE      _ -> []
  SetE          l -> foldMap variableNames l
  UnaryE _ e  -> variableNames e
  BinaryE _ e1 e2 -> on (<>) variableNames e1 e2
