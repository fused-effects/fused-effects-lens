{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module Control.Effect.Lens
  ( view
  , views
  , use
  , uses
  , assign
  , (.=)
  , modifying
  , (%=)
  ) where

import Control.Effect
import qualified Control.Effect.State as State
import qualified Control.Effect.Reader as Reader
import qualified Control.Lens as Lens
import Control.Lens.Getter (Getting)
import Control.Lens.Setter (ASetter)

-- | View the value pointed to by a 'Control.Lens.Getter.Getter',
-- 'Control.Lens.Iso.Iso' or 'Control.Lens.Lens.Lens' or the result of
-- folding over all the results of a 'Control.Lens.Fold.Fold' or
-- 'Control.Lens.Traversal.Traversal' corresponding to the 'Reader'
-- context of the given monadic carrier.
view :: forall r a sig m . (Member (Reader r) sig, Carrier sig m, Functor m) => Getting a r a -> m a
view l = Reader.asks (Lens.view l)
{-# INLINE view #-}

-- | View a function of the value pointed to by a
-- 'Control.Lens.Getter.Getter', 'Control.Lens.Iso.Iso' or
-- 'Control.Lens.Lens.Lens' or the result of folding over all the
-- results of a 'Control.Lens.Fold.Fold' or
-- 'Control.Lens.Traversal.Traversal' corresponding to the 'Reader'
-- context of the given monadic carrier.
-- 
-- This is slightly more general in lens itself, but should suffice for our purposes.
views :: forall s a b sig m . (Member (Reader s) sig, Carrier sig m, Functor m) => Getting a s a -> (a -> b) -> m b
views l f = fmap f (Reader.asks (Lens.view l))
{-# INLINE views #-}

-- | Extract the target of a 'Control.Lens.Lens',
-- 'Control.Lens.Iso.Iso', or 'Control.Lens.Getter.Getter' from the,
-- or use a summary of a 'Control.Lens.Fold.Fold' or
-- 'Control.Lens.Traversal.Traversal' that points to a monoidal value.
use :: forall s a sig m . (Member (State s) sig, Carrier sig m, Monad m) => Getting a s a -> m a
use l = State.gets (Lens.view l)
{-# INLINE use #-}

-- | Use a function of the target of a 'Control.Lens.Lens.Lens',
-- 'Control.Lens.Iso.Iso', or 'Control.Lens.Getter.Getter' in the
-- current state, or use a summary of a 'Control.Lens.Fold.Fold' or
-- 'Control.Lens.Traversal.Traversal' that points to a monoidal value.
uses :: forall s a b f sig . (Carrier sig f, Functor f, Member (State s) sig) => Getting a s a -> (a -> b) -> f b
uses l f = fmap f (State.gets (Lens.view l))
{-# INLINE uses #-}

-- | Replace the target of a 'Control.Lens.Lens.Lens' or all of the
-- targets of a 'Control.Lens.Setter.Setter' or
-- 'Control.Lens.Traversal.Traversal' in our monadic state with a new
-- value, irrespective of the old.
--
-- This is a prefix version of '.='.
assign :: forall s a b sig m . (Member (State s) sig, Carrier sig m, Monad m) => ASetter s s a b -> b -> m ()
assign l b = State.modify (Lens.set l b)
{-# INLINE assign #-}

-- | Replace the target of a 'Control.Lens.Lens.Lens' or all of the
-- targets of a 'Control.Lens.Setter.Setter' or
-- 'Control.Lens.Traversal.Traversal' in our monadic state with a new
-- value, irrespective of the old.
--
-- This is an infix version of 'assign'.
infixr 4 .=
(.=) :: forall s a b sig m . (Member (State s) sig, Carrier sig m, Monad m) => ASetter s s a b -> b -> m ()
(.=) = assign
{-# INLINE (.=) #-}

-- | Map over the target of a 'Control.Lens.Lens.Lens' or all of the
-- targets of a 'Control.Lens.Setter.Setter' or
-- 'Control.Lens.Traversal.Traversal' in our monadic state.
--
-- This is a prefix version of '%='.
modifying :: forall s a b sig m . (Member (State s) sig, Carrier sig m, Monad m) => ASetter s s a b -> (a -> b) -> m ()
modifying l f = State.modify (Lens.over l f)
{-# INLINE modifying #-}

-- | Map over the target of a 'Control.Lens.Lens.Lens' or all of the
-- targets of a 'Control.Lens.Setter.Setter' or
-- 'Control.Lens.Traversal.Traversal' in our monadic state.
--
-- This is an infix version of 'modifying'.
infixr 4 %=
(%=) :: forall s a b sig m . (Member (State s) sig, Carrier sig m, Monad m) => ASetter s s a b -> (a -> b) -> m ()
(%=) = modifying
{-# INLINE (%=) #-}
