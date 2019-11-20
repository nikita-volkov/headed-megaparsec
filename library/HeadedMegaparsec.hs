module HeadedMegaparsec where

import HeadedMegaparsec.Prelude hiding (try)
import Text.Megaparsec hiding (some, endBy1, someTill, sepBy1, sepEndBy1)
import Text.Megaparsec.Char
import Control.Applicative.Combinators.NonEmpty
import qualified Text.Megaparsec.Char.Lexer as Lex

