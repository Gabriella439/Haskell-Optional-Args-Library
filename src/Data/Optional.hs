{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}

{-| Use the `Optional` type for optional function arguments.  For example:

> import Data.Optional
>
> greet :: Optional String -> String
> greet (Specific name) = "Hello, " ++ name
> greet  Default        = "Hello"

>>> greet (Specific "John")
"Hello, John"
>>> greet Default
"Hello"

    The `Optional` type overloads as many Haskell literals as possible so that
    you do not need to wrap values in `Specific`.  For example, if you enable
    the `OverloadedStrings` extension you can use a naked string literal
    instead:

>>> :set -XOverloadedStrings
>>> greet "John"
"Hello, John"

    The `Optional` type also implements `Num` and `Fractional`, so you can
    use numeric literals in place of `Optional` values:

> birthday :: Optional Int -> String
> birthday (Specific age) = "You are " ++ show age ++ " years old!"
> birthday  Default       = "You are one year older!"

>>> birthday 20
"You are 20 years old!"
>>> birthday Default
"You are one year older!"

    You can use `empty` as a short-hand for `Default`:

>>> import Control.Applicative
>>> greet empty
"Hello"
>>> birthday empty
"You are one year older!"

    You can also use `pure` as a short-hand for `Specific`:

>>> greet (pure "John")
"Hello, John"
>>> birthday (pure 20)
"You are 20 years old!"

-}

module Data.Optional where

import Control.Applicative (Applicative(..), Alternative(..), liftA2)
import Control.Monad (MonadPlus(..))
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import Data.Monoid (Monoid(..))
import Data.String (IsString(..))

-- | A function argument that has a `Default` value
data Optional a = Default | Specific a
    deriving (Eq, Functor, Foldable, Traversable, Show)

instance Applicative Optional where
    pure = Specific

    Specific f <*> Specific x = Specific (f x)
    _          <*> _          = Default

instance Monad Optional where
    return = Specific

    Default    >>= _ = Default
    Specific x >>= f = f x

instance Alternative Optional where
    empty = Default

    Default <|> x = x
    x       <|> _ = x

instance MonadPlus Optional where
    mzero = empty
    mplus = (<|>)

instance Monoid a => Monoid (Optional a) where
    mempty = pure mempty

    mappend = liftA2 mappend

instance IsString a => IsString (Optional a) where
    fromString str = pure (fromString str)

instance Num a => Num (Optional a) where
    fromInteger n = pure (fromInteger n)

    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)

    negate = fmap negate
    abs    = fmap abs
    signum = fmap signum

instance Fractional a => Fractional (Optional a) where
    fromRational n = pure (fromRational n)

    recip = fmap recip

    (/) = liftA2 (/)
