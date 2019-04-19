{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module CGX.Prelude
  ( module X
#if __GLASGOW_HASKELL__ <= 800
  , fromLeft
  , fromRight
#endif
  , mk
  , ps
  , psu
  ) where

import qualified Data.Text as Text
import qualified Prelude

import Prelude as X
  ( Bounded(..)
  , Enum(..)
  , iterate
  , maxBound
  )

import Control.Applicative as X
  ( Applicative(..)
  , (<$)
  , (<$>)
  , (<*)
  , (*>)
  , pure
  )

import Control.Monad as X
  ( Functor
  , Monad
  , forever
  , fmap
  , liftM
  , return
  , when
  , (>>=)
  , (=<<)
  , (>>)
  )

import Control.Monad.State as X
  ( State
  , get
  , put
  , runState
  )

import Control.Exception.Base as X
  ( SomeException(..)
  , handle
  )

import Control.Concurrent as X

import Debug.Trace as X
  ( trace
  , traceIO
  , traceShow
  )

import Safe as X
  ( headMay
  , abort
  )

import Data.Eq as X
import Data.Ord as X
import Data.Monoid as X
import Data.Traversable as X
import Data.Foldable as X hiding
  ( foldr1
  , foldl1
  , maximum
  , maximumBy
  , minimum
  , minimumBy
  )

import Data.Int as X
import Data.Bits as X
import Data.Word as X
import Data.Bool as X hiding (bool)
import Data.Char as X (Char)
import Data.Maybe as X hiding (fromJust)
import Data.Either as X
--import Data.Complex as X

import Data.Function as X
  ( id
  , const
  , (.)
  , ($)
  , flip
  , fix
  , on
  )

import Data.Tuple as X
import Data.List as X
  ( filter
  , intersect
  , iterate
  , reverse
  , take
  , takeWhile
  , (\\)
  , (++)
  )

import Data.Text as X
  ( Text
  , pack
  )
import qualified Data.Text.Lazy
import qualified Data.Text.IO
import Data.String.Conv as X
  ( toS
  )
import Data.IntMap as X (IntMap)

import Data.Tagged as X

import GHC.IO as X (IO)
import GHC.Num as X
import GHC.Real as X
import GHC.Float as X
import GHC.Generics as X
import GHC.Show as X
import GHC.Exts as X
  ( Constraint
  , Ptr
  , FunPtr
  , the
  )

#if __GLASGOW_HASKELL__ <= 800
fromLeft :: a -> Either a b -> a
fromLeft _ (Left a) = a
fromLeft a _        = a

fromRight :: b -> Either a b -> b
fromRight _ (Right b) = b
fromRight b _         = b
#endif

mk :: a -> Tagged b a
mk = X.Tagged

psu :: Show a => Tagged b a -> Text
psu = Text.pack . show . X.untag

ps :: Show a => a -> Text
ps = Text.pack . show
