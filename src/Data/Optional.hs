{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}

-- | Use the `Optional` type for optional function arguments.  For example:
-- 
-- > import Data.Optional
-- >
-- > greet :: Optional String -> String
-- > greet (Specific name) = "Hello, " ++ name
-- > greet  Default        = "Hello"
-- 
-- >>> greet (Specific "John")
-- "Hello, John"
-- >>> greet Default
-- "Hello"
-- 
--     The `Optional` type overloads as many Haskell literals as possible so
--     that you do not need to wrap values in `Specific`.  For example, if you
--     enable the `OverloadedStrings` extension you can use a naked string
--     literal instead:
-- 
-- >>> :set -XOverloadedStrings
-- >>> greet "John"
-- "Hello, John"
-- 
--     The `Optional` type also implements `Num` and `Fractional`, so you can
--     use numeric literals in place of `Optional` values:
-- 
-- > birthday :: Optional Int -> String
-- > birthday (Specific age) = "You are " ++ show age ++ " years old!"
-- > birthday  Default       = "You are one year older!"
-- 
-- >>> birthday 20
-- "You are 20 years old!"
-- >>> birthday Default
-- "You are one year older!"
-- 
--     The `IsString`, `Num`, and `Fractional` instances are recursive, so you
--     can wrap your types in a more descriptive newtype and derive `IsString`,
--     `Num` or `Fractional`:
-- 
-- > {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- > 
-- > import Data.Optional
-- > import Data.String (IsString)
-- > 
-- > newtype Name = Name { getName :: String } deriving (IsString)
-- > 
-- > greet :: Optional Name -> String
-- > greet (Specific name) = "Hello, " ++ getName name
-- > greet  Default        = "Hello"
-- > 
-- > newtype Age = Age { getAge :: Int } deriving (Num)
-- > 
-- > birthday :: Optional Age -> String
-- > birthday (Specific age) = "You are " ++ show (getAge age) ++ " years old!"
-- > birthday  Default       = "You are one year older!"
-- 
--     ... and you would still be able to provide naked numeric or string
--     literals:
-- 
-- >>> :set -XOverloadedStrings
-- >>> greet "John"
-- "Hello, John"
-- >>> birthday 20
-- "You are 20 years old!"
-- 
--     You can use `empty` as a short-hand for a `Default` argument:
-- 
-- >>> greet empty
-- "Hello"
-- >>> birthday empty
-- "You are one year older!"
-- 
--     You can also use `pure` as a short-hand for a `Specific` argument:
-- 
-- >>> greet (pure "John")
-- "Hello, John"
-- >>> birthday (pure 20)
-- "You are 20 years old!"

module Data.Optional (
    -- * Optional
      Optional(..)

    -- * Re-exports
    , empty
    , pure
    ) where

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
