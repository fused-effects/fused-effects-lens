{-# LANGUAGE FlexibleContexts, RankNTypes #-}
-- | Provides combinators for the lens-based manipulation of state and
-- context types provided by the fused-effects library, similar to
-- those provided for mtl-based monad transformers.
module Control.Effect.Lens
  ( -- * Reader accessors
    Control.Effect.Lens.view
  , views
    -- * State getters/setters
  , use
  , uses
  , assign
  , modifying
    -- * Infix operators
  , (.=)
  , (?=)
  , (%=)
  , (<~)
    -- * Mathematical operators
  , (+=)
  , (-=)
  , (*=)
  , (//=)
  ) where

import Control.Algebra
import Control.Effect.Reader as Reader
import Control.Effect.State as State
import Lens.Micro as Lens
import Lens.Micro.Extras as Lens

-- | View the value pointed to by a @Getter@, 'Lens', 'Traversal', or
-- @Fold@ corresponding to the 'Reader' context of the given monadic
-- carrier.
view :: forall r a sig m . (Has (Reader.Reader r) sig m) => Getting a r a -> m a
view l = Reader.asks (Lens.view l)
{-# INLINE view #-}

-- | View a function of the value pointed to by a @Getter@ or 'Lens',
-- or the result of folding over all the results of a @Fold@ or
-- 'Traversal', when applied to the 'Reader' context of the given
-- monadic carrier.
--
-- As with standard @view@ from @lens@, you can use this function in prefix
-- form in a pure context, thanks to the 'Algebra' instance for @->@.
views :: forall s a b sig m . (Has (Reader.Reader s) sig m) => Getting a s a -> (a -> b) -> m b
views l f = fmap f (Reader.asks (Lens.view l))
{-# INLINE views #-}

-- | Extract the target of a 'Lens' or @Getter@, or use a summary of a
-- @Fold@ or 'Traversal' that points to a monoidal value.
use :: forall s a sig m . (Has (State.State s) sig m) => Getting a s a -> m a
use l = State.gets (Lens.view l)
{-# INLINE use #-}

-- | Use a function of the target of a 'Lens' or @Getter@ in the
-- current state, or use a summary of a @Fold@ or 'Traversal' that
-- points to a monoidal value.
uses :: forall s a b f sig . (Has (State.State s) sig f) => Getting a s a -> (a -> b) -> f b
uses l f = fmap f (State.gets (Lens.view l))
{-# INLINE uses #-}

-- | Replace the target of a 'Lens' (or all the targets of a @Setter@
-- or 'Traversal') within the current monadic state, irrespective of
-- the old value.
--
-- This is a prefix version of '.='.
assign :: forall s a b sig m . (Has (State.State s) sig m) => ASetter s s a b -> b -> m ()
assign l b = State.modify (Lens.set l b)
{-# INLINE assign #-}

-- | Map over the target of a 'Lens', or all of the targets of a @Setter@
-- or 'Traversal', in the current monadic state.
--
-- This is a prefix version of '%='.
modifying :: forall s a b sig m . (Has (State.State s) sig m) => ASetter s s a b -> (a -> b) -> m ()
modifying l f = State.modify (Lens.over l f)
{-# INLINE modifying #-}

infix 4 .=, %=, ?=, +=, -=, *=, //=
infixr 2 <~

-- | Replace the target of a 'Lens' (or all the targets of a @Setter@
-- or 'Traversal') within the current monadic state, irrespective of
-- the old value.
--
-- This is an infix version of 'assign'.
(.=) :: forall s a b sig m . (Has (State.State s) sig m) => ASetter s s a b -> b -> m ()
(.=) = assign
{-# INLINE (.=) #-}

-- | Replace the target of a Lens or all of the targets of a @Setter@ or
-- 'Traversal' in our monadic state with Just a new value, irrespective
-- of the old.
(?=) :: forall s a b sig m . (Has (State.State s) sig m) => ASetter s s a (Maybe b) -> b -> m ()
setter ?= item = setter .= Just item
{-# INLINE (?=) #-}

-- | Run a monadic action, and set all of the targets of a 'Lens', @Setter@
-- or 'Traversal' to its result.
(<~) :: forall s a b sig m . (Has (State s) sig m) => ASetter s s a b -> m b -> m ()
setter <~ act = act >>= assign setter
{-# INLINE (<~) #-}

-- | Map over the target of a 'Lens', or all of the targets of a @Setter@
-- or 'Traversal', in the current monadic state.
--
-- This is an infix version of 'modifying'.
(%=) :: forall s a b sig m . (Has (State.State s) sig m) => ASetter s s a b -> (a -> b) -> m ()
(%=) = modifying
{-# INLINE (%=) #-}

-- | Modify the target(s) of a 'Lens', @Iso@, @Setter@ or 'Traversal' by adding a value.
(+=) :: forall s a sig m . (Has (State.State s) sig m, Num a) => ASetter' s a -> a -> m ()
l += v = State.modify (l +~ v)
{-# INLINE (+=) #-}

-- | Modify the target(s) of a 'Lens', @Iso@, @Setter@ or 'Traversal' by subtracting a value.
(-=) :: forall s a sig m . (Has (State.State s) sig m, Num a) => ASetter' s a -> a -> m ()
l -= v = State.modify (l -~ v)
{-# INLINE (-=) #-}

-- | Modify the target(s) of a 'Lens', @Iso@, @Setter@ or 'Traversal' by subtracting a value.
(*=) :: forall s a sig m . (Has (State.State s) sig m, Num a) => ASetter' s a -> a -> m ()
l *= v = modifying l (* v)
{-# INLINE (*=) #-}

-- | Modify the target(s) of a 'Lens', @Iso@, @Setter@ or 'Traversal' by dividing a value.
(//=) :: forall s a sig m . (Has (State.State s) sig m, Fractional a) => ASetter' s a -> a -> m ()
l //= v = modifying l (/ v)
{-# INLINE (//=) #-}
