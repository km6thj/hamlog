module Ham.CAT.Parser where

import Ham.Internal.Data

import qualified Data.ByteString.Char8 as B
import Data.Attoparsec.ByteString.Char8


parseFrequency :: B.ByteString -> Maybe Frequency
parseFrequency a = case r of
                     Done s r' -> Just $ MHz $ realToFrac $ r' * 1e-6 -- Assuming the number returned is in Hertz.
                     _         -> Nothing
  where r = parse (doubleFromAnswer (B.pack "FA")) a


frequencyFromAnswer :: B.ByteString
                    -> Parser Frequency
frequencyFromAnswer prefix = do
  string prefix
  v <- double
  char ';'
  return $ MHz $ realToFrac $ v * 1e-6 -- Assuming the number returned is in Hertz.


-- | Parse a double from an answer from the radio given the prefix.
doubleFromAnswer :: B.ByteString  -- Prefix to ignore from the answer.
                 -> Parser Double
doubleFromAnswer prefix = string prefix *> double <* char ';'


-- | Parse an integer from an answer from the radio given the prefix.
intFromAnswer :: B.ByteString -- Prefix to ignore from the answer.
              -> Parser Int
intFromAnswer prefix = string prefix *> decimal <* char ';'
