{-|
Extra megaparsec combinators.
-}
module HeadedMegaparsec.Megaparsec
where

import HeadedMegaparsec.Prelude hiding (try, head, body)
import Text.Megaparsec hiding (some, endBy1, someTill, sepBy1, sepEndBy1)
import Text.Megaparsec.Char
import Control.Applicative.Combinators
import qualified Text.Megaparsec.Char.Lexer as Lexer


contPossibly :: (Ord err, Stream strm) => Parsec err strm (Either a (Parsec err strm a)) -> Parsec err strm a
contPossibly p = do
  junction <- try p
  case junction of
    Left a -> return a
    Right cont -> cont
