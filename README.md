# fused-effects-lens

This package provides an interface to the [`lens`](https://github.com/ekmett/lens) library that is compatible with [`fused-effects`](https://github.com/robrix/fused-effects). The standard formulation of `lens` combinators for operating in `MonadState` contexts—`use`, `.=`, et al—rely on `mtl` for `MonadState` and `MonadReader`, which is not applicable to `Reader` and `State` effects.

This package is meant to be used alongside `lens`, like so:

``` haskell
import Control.Lens hiding (view, use, assign)
import Control.Effect.Lens (view, use, assign)
```

## Example

Given a `Context` type that we will use in a `State` effect:

``` haskell
data Context = Context
  { _amount :: Int
  , _disabled :: Bool
  } deriving (Eq, Show)
  
makeLenses ''Context
```

We can can use the `use` combinators to extract a lens target from the current state, and `assign` to write to a field of that state:

``` haskell
stateTest :: (Member (State Context) sig, Carrier sig m, Monad m) => m Int
stateTest = do
  initial <- use amount
  assign amount (initial + 1)
  assign disabled True
  use amount
```

You can find a more complete example, including one that works with multiple `State` constraints in a single computation, in the `test` directory.

## License

BSD3, like `fused-effects`.
