{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, TemplateHaskell #-}

module Main where

import Control.Effect
import Control.Effect.State
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

readerTest :: (Member (State Context) sig, Carrier sig m, Monad m) => m Int
readerTest = succ <$> view amount

spec :: Spec
spec = describe "use/assign" $ do
  it "should modify stateful variables" $ do
    let result = run $ runState initial stateTest
    result `shouldBe` (Context 1 True, 1)

  it "should read from an environment" $ do
    let result = run $ runReader initial readerTest
    result `shouldBe` 1

main :: IO ()
main = hspec $ describe "Control.Effect.Lens" spec
