# fused-effects-lens

This package provides an interface to the [lens](github.com/ekmett/lens) library that is compatible with [fused-effects](github.com/robrix/fused-effects). The standards formulation of `lens` combinators for operating in `MonadState` contexts—`use`, `.=`, et al—rely on `mtl` for `MonadState` and `MonadReader`, which is not applicable to `Reader` and `State` effects.

This package is meant to be used alongside `lens`, like so:

``` haskell
import Control.Lens.Getter hiding (view, use)
import Control.Lens.Setter hiding (assign)
import Control.Effect.Lens (view, use, assign)
```
