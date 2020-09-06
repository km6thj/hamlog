{-| Not very efficient, simple implementation of a max. size buffer.
This is used to hold the text logs from HamLog actions in the Brick UI. -}

module Ham.Internal.FixedSequence
  (FixedSequence
  ,addElement
  ,toList
  ,emptyFS)
where

import qualified Data.Sequence as S
import qualified Data.Foldable as F (toList)

data FixedSequence a = FixedSequence Int (S.Seq a)

addElement :: a -> FixedSequence a -> FixedSequence a
addElement a (FixedSequence n s) = result
  where l = S.length s
        result | l < n = FixedSequence n (a S.<| s)
               | otherwise = FixedSequence n (a S.<| s')
        s' = S.take (n - 1) s

toList :: FixedSequence a -> [a]
toList (FixedSequence n s) = F.toList s

emptyFS n = FixedSequence n S.empty
