{-# LANGUAGE TypeApplications, FlexibleContexts, MultiParamTypeClasses, TemplateHaskell, TypeFamilies, FlexibleInstances #-}

module Main where

import Control.Effect
import Control.Effect.Reader
import Control.Effect.State
import Control.Lens.Wrapped
import Control.Lens.TH
import Test.Hspec

import Control.Effect.Lens

data Context = Context
  { _amount :: Int
  , _sweatshirt :: Bool
  } deriving (Eq, Show)

initial :: Context
initial = Context 0 False

makeLenses ''Context

stateTest :: (Member (State Context) sig, Carrier sig m, Monad m) => m Int
stateTest = do
  initial <- use amount
  assign amount (initial + 1)
  assign sweatshirt True
  use amount

newtype Foo = Foo { _unFoo :: Int } deriving (Eq, Show)

makeWrapped ''Foo

newtype Bar = Bar { _unBar :: Float } deriving (Eq, Show)

makeWrapped ''Bar

doubleStateTest :: (Member (State Bar) sig, Member (State Foo) sig, Carrier sig m, Monad m) => m Int
doubleStateTest = do
  assign @Foo _Wrapped 5
  assign @Bar _Wrapped 30.5
  pure 50

readerTest :: (Member (Reader Context) sig, Carrier sig m, Monad m) => m Int
readerTest = succ <$> view amount

spec :: Spec
spec = describe "use/assign" $ do
  it "should modify stateful variables" $ do
    let result = run $ runState initial stateTest
    result `shouldBe` (Context 1 True, 1)

  it "works in the presence of polymorphic lenses with -XTypeAnnotations" $ do
    let result = run $ runState (Bar 5) $ runState (Foo 500) doubleStateTest
    result `shouldBe` (Bar 30.5, (Foo 5, 50))

  it "should read from an environment" $ do
    let result = run $ runReader initial readerTest
    result `shouldBe` 1

main :: IO ()
main = hspec $ describe "Control.Effect.Lens" spec
