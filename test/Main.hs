{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, TypeApplications, TypeFamilies #-}

module Main where

import Control.Algebra
import Control.Carrier.Reader
import Control.Carrier.State.Strict
import Lens.Micro
import Test.Hspec

import Control.Effect.Lens

data Context = Context
  { _amount     :: Int
  , _sweatshirt :: Bool
  } deriving (Eq, Show)

amount :: Lens' Context Int
amount = lens _amount (\c a -> c { _amount = a })

sweatshirt :: Lens' Context Bool
sweatshirt = lens _sweatshirt (\c s -> c { _sweatshirt = s })

initial :: Context
initial = Context 0 False

stateTest :: (Has (State Context) sig m) => m Int
stateTest = do
  initial <- use amount
  assign amount (initial + 1)
  assign sweatshirt True
  use amount

class PolymorphicLens t a | t -> a where
  wrapped :: Lens' t a

newtype Foo = Foo { _unFoo :: Int } deriving (Eq, Show)

instance PolymorphicLens Foo Int where
  wrapped = lens _unFoo (\_ i -> Foo i)

newtype Bar = Bar { _unBar :: Float } deriving (Eq, Show)

instance PolymorphicLens Bar Float where
  wrapped = lens _unBar (\_ i -> Bar i)

doubleStateTest :: (Has (State Bar) sig m, Has (State Foo) sig m) => m Int
doubleStateTest = do
  assign @Foo wrapped 5
  assign @Bar wrapped 30.5
  pure 50

readerTest :: (Has (Reader Context) sig m) => m Int
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
