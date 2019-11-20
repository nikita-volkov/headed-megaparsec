module HeadedMegaparsec
(
  -- * Types
  HeadedParsec,
  -- * Execution
  toParsec,
  -- * Construction
  head,
  body,
)
where

import HeadedMegaparsec.Prelude hiding (try, head, body)
import Text.Megaparsec hiding (some, endBy1, someTill, sepBy1, sepEndBy1)
import Text.Megaparsec.Char
import Control.Applicative.Combinators
import qualified Text.Megaparsec.Char.Lexer as Lexer
import qualified HeadedMegaparsec.Megaparsec as Megaparsec

{- $setup

>>> :set -XApplicativeDo

-}

-- * Types
-------------------------

{-|
Headed parser.

Provides for composition between consecutive megaparsec `try` blocks.

>>> :{
  let
    select :: HeadedParsec Void Text (Maybe [Either Char Int], Maybe Int)
    select = do
      head (string' "select")
      _targets <- optional (head space1 *> targets)
      _limit <- optional (head space1 *> limit)
      return (_targets, _limit)
      where
        targets = sepBy1 target commaSeparator
        target =
          head (Left <$> char '*') <|>
          head (Right <$> Lexer.decimal)
        commaSeparator = head (space *> char ',' *> space)
        limit =
          head (string' "limit" *> space1) *>
          body Lexer.decimal
    test :: Text -> IO ()
    test = parseTest (toParsec select <* eof)
:}

>>> test "select limit"
-}
newtype HeadedParsec err strm a = HeadedParsec (Parsec err strm (Either a (Parsec err strm a)))


-- * Instances
-------------------------

-- ** HeadedParsec
-------------------------

instance Functor (HeadedParsec err strm) where
  fmap fn (HeadedParsec p) = HeadedParsec (fmap (bimap fn (fmap fn)) p)

instance (Ord err, Stream strm) => Applicative (HeadedParsec err strm) where
  pure = HeadedParsec . pure . Left
  (<*>) = apS

instance (Ord err, Stream strm) => Selective (HeadedParsec err strm) where
  select (HeadedParsec p1) (HeadedParsec p2) = HeadedParsec $ do
    junction1 <- p1
    case junction1 of
      Left eitherAOrB -> case eitherAOrB of
        Right b -> return (Left b)
        Left a -> do
          junction2 <- p2
          case junction2 of
            Left aToB -> return (Left (aToB a))
            Right bodyP2 -> return (Right (fmap ($ a) bodyP2))
      Right bodyP1 -> return $ Right $ do
        eitherAOrB <- bodyP1
        case eitherAOrB of
          Right b -> return b
          Left a -> do
            junction2 <- p2
            case junction2 of
              Left aToB -> return (aToB a)
              Right bodyP2 -> fmap ($ a) bodyP2

{-|
Alternation is performed only the basis of heads.
Bodies do not participate.
-}
instance (Ord err, Stream strm) => Alternative (HeadedParsec err strm) where
  empty = HeadedParsec empty
  (<|>) (HeadedParsec p1) (HeadedParsec p2) = HeadedParsec (try p1 <|> p2)


-- * Execution
-------------------------

{-|
Convert headed parser into megaparsec parser.
-}
toParsec :: (Ord err, Stream strm) => HeadedParsec err strm a -> Parsec err strm a
toParsec (HeadedParsec p) = Megaparsec.contPossibly p


-- *
-------------------------

{-|
Lift a megaparsec parser as a head parser.

Composing consecutive heads results in one head.
-}
head :: (Ord err, Stream strm) => Parsec err strm a -> HeadedParsec err strm a
head = HeadedParsec . fmap Left

{-|
Lift a megaparsec parser as a body parser.

Composing consecutive bodies results in one body.

Composing consecutive head and body leaves the head still composable with preceding head.
-}
body :: (Stream strm) => Parsec err strm a -> HeadedParsec err strm a
body = HeadedParsec . return . Right
