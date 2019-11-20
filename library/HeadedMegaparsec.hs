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
data HeadedParsec err strm a =
  HeadHeadedParsec (Parsec err strm (Either a (Parsec err strm a))) |
  BodyHeadedParsec (Parsec err strm a)


-- * Instances
-------------------------

-- ** HeadedParsec
-------------------------

instance Functor (HeadedParsec err strm) where
  fmap fn = \ case
    HeadHeadedParsec p -> HeadHeadedParsec (fmap (bimap fn (fmap fn)) p)
    BodyHeadedParsec p -> BodyHeadedParsec (fmap fn p)

instance (Ord err, Stream strm) => Applicative (HeadedParsec err strm) where
  pure = HeadHeadedParsec . pure . pure . pure
  (<*>) = apS

instance (Ord err, Stream strm) => Selective (HeadedParsec err strm) where
  select = \ case
    HeadHeadedParsec p1 -> \ case
      HeadHeadedParsec p2 -> HeadHeadedParsec $ do
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
                junction2 <- try p2
                case junction2 of
                  Left aToB -> return (aToB a)
                  Right bodyP2 -> fmap ($ a) bodyP2
      BodyHeadedParsec p2 -> HeadHeadedParsec $ do
        junction1 <- p1
        case junction1 of
          Left eitherAOrB -> case eitherAOrB of
            Left a -> return (Right (fmap ($ a) p2))
            Right b -> return (Left b)
          Right bodyP1 -> return $ Right $ do
            eitherAOrB <- bodyP1
            case eitherAOrB of
              Left a -> fmap ($ a) p2
              Right b -> return b
    BodyHeadedParsec p1 -> \ hp2 -> BodyHeadedParsec (selectA p1 (toParsec hp2))

instance (Ord err, Stream strm) => Alternative (HeadedParsec err strm) where
  empty = HeadHeadedParsec empty
  (<|>) = \ case
    HeadHeadedParsec p1 -> \ case
      HeadHeadedParsec p2 -> HeadHeadedParsec (try p1 <|> p2)
      BodyHeadedParsec p2 -> BodyHeadedParsec (try (toParsec (HeadHeadedParsec p1)) <|> p2)
    BodyHeadedParsec p1 -> \ case
      HeadHeadedParsec p2 -> BodyHeadedParsec (try p1 <|> toParsec (HeadHeadedParsec p2))
      BodyHeadedParsec p2 -> BodyHeadedParsec (try p1 <|> p2)


-- * Execution
-------------------------

{-|
Convert headed parser into megaparsec parser.
-}
toParsec :: (Ord err, Stream strm) => HeadedParsec err strm a -> Parsec err strm a
toParsec = \ case
  HeadHeadedParsec p -> do
    junction <- try p
    case junction of
      Left a -> return a
      Right bodyP -> bodyP
  BodyHeadedParsec p -> p


-- *
-------------------------

{-|
Lift a megaparsec parser as a head parser.

Composing consecutive heads results in one head.
-}
head :: (Ord err, Stream strm) => Parsec err strm a -> HeadedParsec err strm a
head = HeadHeadedParsec . fmap Left

{-|
Lift a megaparsec parser as a body parser.

Composing consecutive bodies results in one body.

Composing consecutive head and body leaves the head still composable with preceding head.
-}
body :: (Stream strm) => Parsec err strm a -> HeadedParsec err strm a
body = BodyHeadedParsec
