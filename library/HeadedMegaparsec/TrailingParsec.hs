module HeadedMegaparsec.TrailingParsec
(
  TrailingParsec,
  -- * Execution
  toHeadedParsec,
  toParsec,
  -- * Transformation
  label,
  dbg,
  filter,
  -- * Construction
  parse,
  parseHeaded,
  endHead,
)
where

import HeadedMegaparsec.Prelude hiding (try, head, tail, filter)
import HeadedMegaparsec.HeadedParsec (HeadedParsec)
import Control.Applicative.Combinators
import Text.Megaparsec (Parsec, Stream)
import qualified HeadedMegaparsec.Megaparsec as Megaparsec
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Debug as Megaparsec
import qualified Text.Megaparsec.Char as MegaparsecChar
import qualified Text.Megaparsec.Char.Lexer as MegaparsecLexer
import qualified HeadedMegaparsec.HeadedParsec as HeadedParsec


{-|
Accumulates the sequently composed alternatives,
alternating between all combinations when executed.
-}
newtype TrailingParsec err strm a = TrailingParsec [HeadedParsec err strm a]

deriving instance Functor (TrailingParsec err strm)

instance (Ord err, Stream strm) => Applicative (TrailingParsec err strm) where
  pure a = TrailingParsec [pure a]
  (<*>) = ap

instance (Ord err, Stream strm) => Selective (TrailingParsec err strm) where
  select = selectA

instance (Ord err, Stream strm) => Monad (TrailingParsec err strm) where
  return = pure
  (>>=) (TrailingParsec l1) k2 = TrailingParsec $ do
    hp1 <- l1
    return $ do
      a <- HeadedParsec.wrapToHead hp1
      toHeadedParsec (k2 a)

instance (Ord err, Stream strm) => Alternative (TrailingParsec err strm) where
  empty = TrailingParsec []
  (<|>) (TrailingParsec l1) (TrailingParsec l2) = TrailingParsec (l1 <> l2)

instance (Ord err, Stream strm) => MonadPlus (TrailingParsec err strm) where
  mzero = empty
  mplus = (<|>)

instance (Ord err, Stream strm) => MonadFail (TrailingParsec err strm) where
  fail = TrailingParsec . fail


-- * Execution
-------------------------

toHeadedParsec :: (Ord err, Stream strm) => TrailingParsec err strm a -> HeadedParsec err strm a
toHeadedParsec (TrailingParsec l) = asum l

toParsec :: (Ord err, Stream strm) => TrailingParsec err strm a -> Parsec err strm a
toParsec = HeadedParsec.toParsec . toHeadedParsec


-- * Helpers
-------------------------

mapHeadedParsec fn (TrailingParsec l) = TrailingParsec (fmap fn l)


-- * Transformation
-------------------------

{-|
Label a headed parser.
Works the same way as megaparsec's `Megaparsec.label`.
-}
label :: (Ord err, Stream strm) => String -> TrailingParsec err strm a -> TrailingParsec err strm a
label label = mapHeadedParsec (HeadedParsec.label label)

{-|
Make a parser print debugging information when evaluated.
The first parameter is a custom label.

This function is a wrapper around `Megaparsec.dbg`.
It generates two debugging entries: one for head and one for tail.
-}
dbg :: (Ord err, Megaparsec.ShowErrorComponent err, Stream strm, Show a) => String -> TrailingParsec err strm a -> TrailingParsec err strm a
dbg label = mapHeadedParsec (HeadedParsec.dbg label)

{-|
Filter the results of parser based on a predicate,
failing with a parameterized message.
-}
filter :: (Ord err, Stream strm) => (a -> String) -> (a -> Bool) -> TrailingParsec err strm a -> TrailingParsec err strm a
filter err pred = mapHeadedParsec (HeadedParsec.filter err pred)


-- *
-------------------------

{-|
Lift a megaparsec parser.
-}
parse :: (Ord err, Stream strm) => Parsec err strm a -> TrailingParsec err strm a
parse = parseHeaded . HeadedParsec.parse

{-|
Lift a headed parser.
-}
parseHeaded :: (Ord err, Stream strm) => HeadedParsec err strm a -> TrailingParsec err strm a
parseHeaded = TrailingParsec . pure


-- * Control
-------------------------

{-|
Make all the following parsers compose as tail.
-}
endHead :: (Stream strm) => TrailingParsec err strm ()
endHead = TrailingParsec [HeadedParsec.endHead]
