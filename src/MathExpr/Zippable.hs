{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}

module MathExpr.Zippable where


import           Chronos
import           Control.Monad.Writer (tell, runWriter, WriterT(..))
import qualified Data.HashMap.Strict as HM
import           Data.SOP (I(..), unI, (:.:)(..), unComp)
import qualified Data.Set as S
import           Data.Time.Calendar (fromGregorian)
import qualified Data.Vector as V
import           MathExpr.Parser
import           MathExpr.Transformations
import           MathExpr.Types
import           Yahp hiding (Sum, Product)

-- * Zippable class

class Functor f => Zippable f where
  zipWith_  :: (a -> b -> c)                -> f a -> f b -> f c
  default zipWith_  :: (Generic (f a), Rep (f a) ~ D1 d (C1 c1 (S1 s (Rec0 a)))) =>
    (Generic (f b), Rep (f b) ~ D1 d (C1 c1 (S1 s (Rec0 b)))) =>
    (Generic (f c), Rep (f c) ~ D1 d (C1 c1 (S1 s (Rec0 c)))) =>
    (a -> b -> c)                -> f a -> f b -> f c
  zipWith_ f a = rewrapSingleConstructor . f (unwrapSingleConstructor a) . unwrapSingleConstructor


-- * Value type

data ValueDyn f   = StringV (f Text)
                  | NumberV (f Double)
                  | BoolV   (f Bool)
                  | DateV   (f Day)

valueDynZip :: (Typeable f, Typeable g, Error m)
  => Text -> (forall a . IsDynValue a => f a -> g a -> b) -> ValueDyn f -> ValueDyn g -> m b
valueDynZip msg f a b = case (a, b) of
    (StringV    x, StringV     y) -> pure $ f x y
    (NumberV    x, NumberV     y) -> pure $ f x y
    (BoolV      x, BoolV       y) -> pure $ f x y
    (DateV      x, DateV       y) -> pure $ f x y
    _ -> throwError $ "'" <> msg <> "' requires two operands of the same type, got: "
              <> valueDynType "" a <> " and  " <> valueDynType "" b
  

data Value f = ConstValue (ValueDyn I) | DynValue (ValueDyn f) | SetValue (ValueDyn Set) | EmptySet

mapValueDyn :: (forall a . IsDynValue a => f a -> g a) -> ValueDyn f -> ValueDyn g
mapValueDyn g = withValueDyn $ toDynValue . g
{-# INLINABLE mapValueDyn #-}

withValueDyn :: (forall a . IsDynValue a => f a -> b) -> ValueDyn f -> b
withValueDyn f = \case
  StringV     s -> f s
  NumberV     s -> f s
  BoolV       s -> f s
  DateV       s -> f s
{-# INLINABLE withValueDyn #-}

fromScalarValue :: (Typeable f, IsDynValue a, Error m) => (forall a . a -> f a) -> Value f -> m (f a)
fromScalarValue pure = \case
  ConstValue    x       -> pure <$> fromConstValue x
  DynValue      x       -> fromDynValue x
  SetValue      l       -> throwError $ "Expected scalar, got set of size " <> shot (withValueDyn length l)
  EmptySet              -> throwError $ "Expected scalar, got empty set"
{-# INLINABLE fromScalarValue #-}

deriving instance Eq (ValueDyn Set)
deriving instance Ord (ValueDyn Set)
deriving instance Eq (ValueDyn I)
deriving instance Ord (ValueDyn I)

mapValue :: (forall a . IsDynValue a => f a -> g a) -> Value f -> Value g
{-# INLINABLE mapValue #-}
mapValue g = \case
  DynValue      v  -> DynValue $ mapValueDyn g v
  ConstValue    v  -> ConstValue v
  SetValue      v  -> SetValue v
  EmptySet         -> EmptySet

type ContextSatifies a f = (Functor f, a (f Day), a (f Text), a (f Double), a (f Bool))

showValueDyn :: ContextSatifies Show f => ValueDyn f -> Text
{-# INLINABLE showValueDyn #-}
showValueDyn = \case
  StringV     s -> shot s
  NumberV     s -> shot s
  BoolV       s -> shot s
  DateV       s -> shot s

showValue :: ContextSatifies Show f  => Value f -> Text
{-# INLINABLE showValue #-}
showValue = \case
  ConstValue    v  -> showValueDyn v
  DynValue      v  -> showValueDyn v
  SetValue      v  -> showValueDyn $ mapValueDyn toList v
  EmptySet         -> "[]"

instance ContextSatifies Show f  => Show (Value f) where
  show = toS . showValue



valueType :: forall f . Typeable f => Value f -> Text
{-# INLINABLE valueType #-}
valueType = \case
  ConstValue    v -> valueDynType "" v
  DynValue      v -> valueDynType (" " <> shot (typeRep $ Proxy @f))v
  SetValue      v -> "{" <> valueDynType "" v <> "}"
  EmptySet        -> "∅"

valueDynType :: forall f . Typeable f => Text -> ValueDyn f -> Text
{-# INLINABLE valueDynType #-}
valueDynType suf = (<> suf) . withValueDyn (shot . typeOf)

class (Typeable a, Ord a, Eq a) => IsDynValue a where
  toDynValue            ::                              f a          -> ValueDyn f
  fromDynValue          :: (Error m, Typeable f) =>     ValueDyn f   -> m (f a)

toConstValue :: IsDynValue a => a -> ValueDyn I
{-# INLINABLE toConstValue #-}
toConstValue    = toDynValue . I

fromConstValue :: (IsDynValue b, Error m) => ValueDyn I -> m b
{-# INLINABLE fromConstValue #-}
fromConstValue  = fmap2 unI fromDynValue

typeError :: (Error m, Typeable f) => Text -> ValueDyn f -> m a
{-# INLINABLE typeError #-}
typeError exp got = throwError $ "Expected '" <> exp <> "', got: " <> valueDynType "" got

instance IsDynValue Text    where
  toDynValue = StringV
  fromDynValue v = case v of { StringV s -> pure s; _ -> typeError "String" v }

instance IsDynValue Day    where
  toDynValue = DateV
  fromDynValue v = case v of { DateV s -> pure s; _ -> typeError "Day" v }

instance IsDynValue Double    where
  toDynValue = NumberV
  fromDynValue v = case v of { NumberV s -> pure s; _ -> typeError "Number" v }

instance IsDynValue Bool    where
  toDynValue = BoolV
  fromDynValue v = case v of { BoolV s -> pure s; _ -> typeError "Bool" v }

constV :: IsDynValue a => a -> Value f
constV   = ConstValue    . toConstValue
{-# INLINABLE constV #-}

dynV :: IsDynValue a => f a -> Value f
{-# INLINABLE dynV #-}
dynV     = DynValue      . toDynValue

mapUnary :: (IsDynValue a, IsDynValue b, Error m, Typeable f, Functor f)
  => Text -> (a -> b) -> Value f -> m (Value f)
{-# INLINABLE mapUnary #-}
mapUnary opName g v = case v of
  ConstValue    a -> constV       . g            <$> fromConstValue a
  DynValue      a -> dynV         . fmap g       <$> fromDynValue a
  _ -> throwError $ "'" <> shot opName <> "' not supported on Sets"

mapBinary2 :: (IsDynValue b, Error m, Typeable f, Zippable f)
  => BaseOperator -> (forall a . IsDynValue a => a -> a -> b) -> Value f -> Value f -> m (Value f)
mapBinary2 opName g v1 v2 = case (v1, v2) of
  (ConstValue   a1, ConstValue  a2)     -> constV <$> valueDynZip msg (on g unI) a1 a2
  (ConstValue   a1, DynValue    a2)     -> dynV <$>  valueDynZip msg (fmap . g . unI) a1 a2
  (DynValue     a1, ConstValue  a2)     -> dynV <$> valueDynZip msg (fmap . (flip g) . unI) a2 a1
  (DynValue     a1, DynValue    a2)     -> dynV <$> valueDynZip msg (zipWith_ g) a1 a2
  _                                     -> throwError $ "'" <> msg <> "' not supported on Sets"
  where msg = shot opName
{-# INLINABLE mapBinary2 #-}

mapBinary :: (IsDynValue a1, IsDynValue a2, IsDynValue b, Error m, Typeable f, Zippable f)
  => BaseOperator -> (a1 -> a2 -> b) -> Value f -> Value f -> m (Value f)
mapBinary opName g v1 v2 = case (v1, v2) of
  (ConstValue   a1, ConstValue  a2)     -> fmap2 constV g <$> fromConstValue a1 <*> fromConstValue a2
  (ConstValue   a1, DynValue    a2)     -> fmap dynV . fmap . g  <$> fromConstValue a1 <*> fromDynValue a2
  (DynValue     a1, ConstValue  a2)     -> fmap dynV . fmap . flip g <$> fromConstValue a2 <*> fromDynValue a1
  (DynValue     a1, DynValue    a2)     -> fmap dynV . zipWith_ g <$> fromDynValue a1 <*> fromDynValue a2
  _                                     -> throwError $ "'" <> shot opName <> "' not supported on Sets"
{-# INLINABLE mapBinary #-}

setBinary :: (Typeable f, Error m) => BaseOperator -> (forall a . IsDynValue a => Set a -> Set a -> Set a)
  -> Value f -> Value f -> m (Value f)
setBinary opName g v1 v2 = case (v1, v2) of
  (SetValue   a1, SetValue  a2)     -> SetValue <$> valueDynZip (shot opName) (fmap2 toDynValue g) a1 a2
  _ -> err
  where err = throwError $ "'" <> shot opName <> "' requires two Sets, got: "
              <> valueType v1 <> " and  " <> valueType v2
{-# INLINABLE setBinary #-}

element' :: forall f f2 m . (Error m, Typeable f, Typeable f2, Functor f) => Bool -> Value f -> Value f2 -> m (Value f)
element' negate v s = case s of
  EmptySet      -> pure $ ConstValue $ BoolV $ I negate
  SetValue s2   -> withValueDyn mapU s2
  _ -> err
  where err = throwError $ "'∈' requires two Value and Set of the same type, got: "
              <> valueType v <> " and  " <> valueType s
        mapU :: (IsDynValue a, Ord a) => Set a -> m (Value f)
        mapU x = mapUnary "∈ {}" (xor negate . flip S.member x) v
{-# INLINABLE element' #-}
  

requireConst :: (HasBinarySymbols a, Error m) => Expr' a -> Value f -> m (ValueDyn I)
requireConst e = \case
  ConstValue    c -> pure c
  _ -> throwError $ "Only constants are allowed in sets, got: " <> pretty e
{-# INLINABLE requireConst #-}

fromConstValueList :: Error m => [ValueDyn I] -> m (Value f)
fromConstValueList = \case
  [] -> pure EmptySet
  (h:t) -> withValueDyn (\(I v) -> (\r -> SetValue $ toDynValue $ S.fromList $ v:r) <$> mapErrors fromConstValue t) h
{-# INLINABLE fromConstValueList #-}

-- * Zippable instances
instance Zippable Identity where
instance Zippable I where

instance Monoid a => Zippable ((,) a) where zipWith_ = liftA2 

instance Zippable ((->) a) where zipWith_ = liftA2 

instance Zippable Maybe where zipWith_ = liftA2 

instance Zippable (Either e) where zipWith_ = liftA2 

instance Zippable [] where zipWith_ = zipWith

instance Zippable V.Vector where zipWith_ = V.zipWith

zipWithUnwrapped2 :: (Zippable f, Zippable g) => (forall x . f (g x) -> h x)
     -> (forall x . h x -> f (g x)) -> (a -> b -> c) -> h a -> h b -> h c
zipWithUnwrapped2 wrap unwrap f a b = wrap $ zipWith_ (zipWith_ f) (unwrap a) $ unwrap b

instance Zippable m => Zippable (ExceptT e m) where
  zipWith_ = zipWithUnwrapped2 ExceptT runExceptT

instance Zippable m => Zippable (ReaderT r m) where
  zipWith_ = zipWithUnwrapped2 ReaderT runReaderT

instance (Zippable f, Zippable g) => Zippable (f Data.SOP.:.: g) where
  zipWith_ = zipWithUnwrapped2 Comp unComp

instance (Monoid w, Zippable m) => Zippable (WriterT w m) where
  zipWith_ = zipWithUnwrapped2 (WriterT . fmap swap) (fmap swap . runWriterT)


-- * Evaluation

type VarValues m f = VarName -> m (Value f)

eval :: forall f m . (Error m, Typeable f, Zippable f) => VarValues m f -> DesugaredExpr -> m (Value f)
eval vars expr = case expr of 
  VarE          v -> vars v
  LiteralE      l -> pure $ ConstValue $ case l of
    BoolL    b  -> BoolV        $ I b
    NumberL  b  -> NumberV      $ I b
    StringL  b  -> StringV      $ I b
    DateL    b  -> DateV        $ I b
  SetE          l -> fromConstValueList =<< mapM (\e -> requireConst e =<< recurse e) l
  UnaryE op e  -> recurse e >>= let opt = shot op in case op of
    Not         -> mapUnary opt not
    Negate      -> mapUnary opt $ negate @Double
    Iverson     -> mapUnary opt $ bool 0 (1 :: Double)
  BinaryE (Extra op) _ _ -> case op of
  BinaryE (Base op) e1 e2 -> case op of
            Sum                 -> eval2 $ \a b -> case a of
              NullV     -> pure b
              _         -> case b of
                NullV   -> pure a
                _       -> mapBinary @Double op (+) a b
            Difference  -> eval2 $ \a b -> case a of
              NullV     -> mapUnary (shot op) (negate @Double) b
              _         -> case b of
                NullV   -> pure a
                _       -> mapBinary @Double op (-) a b
            Product     -> eval2 $ \a b -> case a of
              NullV     -> pure nullV
              OneV      -> pure b
              _         -> case b of
                NullV   -> pure nullV
                OneV    -> pure a
                _       -> mapBinary @Double op    (*) a b
            Quotient    -> eval2 $ \a b -> case a of
              NullV     -> pure nullV
              _         -> case b of
                OneV    -> pure a
                _       -> mapBinary @Double op    (/) a b
            And         -> eval2 $ \a b -> case a of
              FalseV    -> pure falseV
              TrueV     -> pure b
              _         -> case b of
                FalseV  -> pure falseV
                TrueV   -> pure a
                _       -> mapBinary op    (&&) a b
            Or          -> eval2 $ \a b -> case a of
              FalseV    -> pure b
              TrueV     -> pure trueV
              _         -> case b of
                FalseV  -> pure a
                TrueV   -> pure trueV
                _       -> mapBinary op    (||) a b
            Xor         -> eval2 $ \a b -> case a of
              TrueV     -> mapUnary (shot op) not b
              FalseV    -> pure b
              _         -> case b of
                TrueV   -> mapUnary (shot op) not a
                FalseV  -> pure a
                _       -> mapBinary @Bool op    xor a b
            Less                -> mb2    (<)
            LessEqual           -> mb2    (<=)
            Equal               -> mb2    (==)
            NotEqual            -> mb2    (/=)
            Greater             -> mb2    (>)
            GreaterEqual        -> mb2    (>=)
            Intersection        -> eval2 $ setBinary op $ S.intersection
            Union               -> eval2 $ setBinary op $ S.union
            SetDifference       -> eval2 $ setBinary op $ S.difference
            Element             -> eval2 $ element' False
            NotElement          -> eval2 $ element' True
    where mb2 g = do {a <- recurse e1; b <- recurse e2; mapBinary2 op g a b}
          mb2 :: IsDynValue b => (forall a . IsDynValue a => a -> a -> b) -> m (Value f)

          eval2 f = do {a <- recurse e1; b <- recurse e2; f a b}

  where recurse = eval vars
{-# INLINABLE eval #-}
            
parseAndEval :: (Error m, Typeable f, Zippable f) => VarValues m f -> String -> m (Value f)
parseAndEval vars = chain (eval vars) . either (throwError . shot) desugarStepFunctions . parseExpr
{-# INLINABLE parseAndEval #-}


exampleVars :: [(Text, Value ([] Data.SOP.:.: (->) Double))]
exampleVars = [("a", dynV $ Comp [(+10), (+ (-2)), negate])
              ,("b", dynV $ Comp [("v" <>) . shot])
              ,("t", constV @Text "v8.0")
              ,("d", constV @Double 2.5)]

-- | 
-- λ>  runExample "a+a" 3
-- ZipList {getZipList = [26.0,2.0,-6.0]}
runExample :: MonadIO m => String -> Double -> m ()
runExample x y = either putStrLn putStrLn $ showValue . mapValue (fmap ($ y) . unComp)
  <$> parseAndEval (flip lookupThrow $ HM.fromList $ first VarName <$> exampleVars) x 


-- | 
-- λ>  runExample2 "(b cap c) + a"
-- (Left "'Intersection' requires two Sets of the same type, got: Number and  Number",["b","c"])
-- 
-- λ> runExample2 "d ? 2022-03-01:2"
-- (Right I 0.0,["d"])
runExample2 :: String -> (Either Text (Value I), [Text])
runExample2 = runWriter . runExceptT . parseAndEval (\v -> constV (fromBaseDay $ fromGregorian 2022 1 1) <$ tell [unVarName v])


pattern FalseV  = ConstValue (BoolV (I False))
pattern TrueV  = ConstValue (BoolV (I True))
pattern NullV  = ConstValue (NumberV (I 0))
pattern OneV   = ConstValue (NumberV (I 1))

falseV :: Value f
falseV  = constV False

trueV :: Value f
trueV   = constV True

nullV :: Value f
nullV   = constV @Double 0

oneV :: Value f
oneV    = constV @Double 1

infV :: Value f
infV    = constV @Double $ 1.0/0.0

