\section{Huffman Codec}

\begin{code}
module Codec.Compression.Huffman
       (
       ) where
import qualified Data.Map as Map
import qualified Data.Vector.Unbox as UV
import GHC.Exts
\end{code}


\begin{code}
data HTree e v = Item !v !e
               | Node (HTree e v) (HTree e v)
               deriving (Show)

instance (HTree e v) where
  type Item (HTree e v) = (e,v)
  fromList = map (\(e,v) -> Item v e)
  toList   = map (\(Item v e) -> (e,v))

countHTV :: Num v => HTree e v -> v
countHTV (Item v _) = seq
countHTV Node n1 n2 = let v1 = countHTV n1
                          v2 = countHTV n2
                      in v1 `seq` v2 `seq` v1+v2

instance (Ord v,Num v) => HTree e v where
  compare a b = countHTV a `compare` countHTV b

buildHTree :: (Eq e, Num v, Ord v) => [HTree e v] -> HTree e v
buildHTree [] = error "can not be empty"
buildHTree (x:[]) = x
buildHTree xs' = buildHTree $ (Node x1 x2):xs
  where x1:x2:xs = sort xs'

buildTable :: HTree e v -> Map (e,UV.Vector Bool)
\end{code}
