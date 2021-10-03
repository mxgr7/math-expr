
module MathExpr.Transformations where

import qualified Data.List.NonEmpty as E
import           MathExpr.Types
import           Yahp hiding (Sum, Product)

-- * Step Functions

desugarStepFunctions :: forall m . Error m => Expr -> m DesugaredExpr
{-# INLINABLE desugarStepFunctions #-}
desugarStepFunctions = \case
  UnaryE o e -> UnaryE o <$> desugarStepFunctions e
  BinaryE (Base op) e1 e2 -> BinaryE (Base op) <$> desugarStepFunctions e1 <*> desugarStepFunctions e2
  BinaryE (Extra op) f1 rest1 -> case op of
    StepConcat -> err op
    StepHeight -> err op
    StepFunction -> let
      g :: Maybe Text -> Expr -> m StepFunctionSteps
      g n = \case
        BinaryE (Extra StepConcat) e1 e2 -> (<>) <$> f n e1 <*> g Nothing e2
        r -> f (msum [n, Just "last"]) r
      f :: Maybe Text -> Expr -> m StepFunctionSteps
      f _ (BinaryE (Extra StepHeight) e1 e2) = (\x y -> pure (Just x, Just y)) <$> desugarStepFunctions e1 <*> desugarStepFunctions e2
      f (Just "first") r = (\x -> pure (Nothing, Just x)) <$> desugarStepFunctions r
      f (Just "last") r = (\x -> pure (Just x, Nothing)) <$>  desugarStepFunctions r 
      f _ r = throwError $ "Expected " <> bs StepHeight <> "-expression in the middle of a step function, got " <> shot r
      in stepFunctionToExpr <$> desugarStepFunctions f1 <*> g (Just "first") rest1
    
  SetE          s -> SetE <$> mapM desugarStepFunctions s
  VarE          v -> pure $ VarE v
  LiteralE      l -> pure $ LiteralE      l
  where err op = throwError $ bs op <> " has to be inside of step function"
        bs = shot . toList . binarySymbols @Text . Extra
        bs :: StepFunctionsOperators -> Text


-- | list of (lower bound; values)-pairs
type StepFunctionSteps = NonEmpty (Maybe DesugaredExpr, Maybe DesugaredExpr)

-- | Turn a step function description into a sum of products of the
-- form `height * [condition]`
stepFunctionToExpr :: DesugaredExpr -- ^ the expression to compare
                   -> StepFunctionSteps -> DesugaredExpr
stepFunctionToExpr comp steps = maybe (numE 0) (foldr1 sumE) $ nonEmpty
                                $ catMaybes $ zipWith g (toList steps) $ (E.tail steps) <> [(Nothing, Nothing)]
  where g (_, Nothing) _ = Nothing
        g (lb, Just h) (ub, _) = Just $ maybe h (prodE h . iversonE . foldr1 andE)
                                 $ nonEmpty $ catMaybes [greaterEqualE comp <$> lb, lessE comp <$> ub]
        g :: (Maybe DesugaredExpr, Maybe DesugaredExpr) -> (Maybe DesugaredExpr, a) -> Maybe DesugaredExpr
          
-- * Derivatives

-- | λ> either putStrLn print $ derivative (VarName "a") =<< parseAndDesugarExpr "a * (b? 3:4)"
--   4.0 * [b ≥ 3.0]
derivative :: Error m => VarName -> DesugaredExpr -> m DesugaredExpr
derivative var ex = case recurse ex of
  NoOccurence           -> pure nullE
  NonDifferentiable   t -> throwError $ "Non differentiable: " <> t
  Derivative          d -> pure d
  where recurse = \case
          VarE v2       -> bool NoOccurence (Derivative oneE) $ v2 == var
          LiteralE _    -> NoOccurence
          UnaryE op ex -> let d = recurse ex
                              nond = nonDifferentiable (shot op) d in case op of
            Not         -> nond
            Iverson     -> nond
            Negate      -> fmap (UnaryE Negate) d
          BinaryE (Base op) ex1 ex2 -> let d1 = recurse ex1
                                           d2 = recurse ex2
                                           diff = derivativeOp differenceE negateE
                                           sum = derivativeOp sumE id
                                           prodSum f = f (prodE ex2 <$> d1) (prodE ex1 <$> d2)
                                           nond = nonDifferentiableBinary (shot op) d1 d2 in case op of
            Sum                         -> sum d1 d2
            Difference                  -> diff d1 d2
            Product                     -> prodSum sum
            Quotient                    -> case d2 of
              NoOccurence               -> flip quotientE ex2 <$> d1
              NonDifferentiable _       -> d2
              _                         -> flip quotientE (prodE ex2 ex2) <$> (prodSum diff)
            Less                        -> nond
            LessEqual                   -> nond
            Greater                     -> nond
            GreaterEqual                -> nond
            Equal                       -> nond
            NotEqual                    -> nond
            And                         -> nond
            Xor                         -> nond
            Or                          -> nond
            Intersection                -> nond
            Union                       -> nond
            SetDifference               -> nond
            Element                     -> nond
            NotElement                  -> nond
            
          BinaryE (Extra op) _ _ -> case op of
          
          SetE s -> if all hasNoOccurence $ recurse <$> s then NoOccurence
                    else NonDifferentiable "Set"

data Derivative e = NoOccurence | NonDifferentiable Text | Derivative e
  deriving (Functor, Eq, Show)

type DerivativeExpr = Derivative DesugaredExpr

hasNoOccurence = \case
  NoOccurence -> True
  _ -> False

derivativeOp :: (Expr' a -> Expr' a -> Expr' a) -> (Expr' a -> Expr' a)
  -> Derivative (Expr' a) -> Derivative (Expr' a) -> Derivative (Expr' a)
derivativeOp _  f NoOccurence x = f <$> x
derivativeOp _  _ x NoOccurence = x
derivativeOp _  _ x@(NonDifferentiable _) _ = x
derivativeOp _  _ _ x@(NonDifferentiable _) = x
derivativeOp op _  (Derivative a) b = op a <$> b

nonDifferentiableBinary reason a b = case (a,b) of
  (NoOccurence, NoOccurence) -> NoOccurence
  _ -> NonDifferentiable reason

nonDifferentiable reason = \case
  NoOccurence -> NoOccurence
  _ -> NonDifferentiable reason


