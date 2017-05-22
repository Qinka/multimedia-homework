\section{Huffman Codec}

So in this part there will go with a solution of codec in Huffman. 
The basic idea of the Huffman coding is using less bits to represent the items with higher frequency.
Before encoding and decoding, we need to build a Huffman Tree so that we can get the ``relationship'' between
the items' themselves context and items' encoded context. Finally, with the ``relationship'', we can encode and decode.

\begin{code}
module Codec.Compression.Huffman
       ( HTree(..)
       , countHTV
       , buildHTree
       , buildTable
       , encode
       , decode
       ) where
import Data.List(sort)
import Data.Map (Map)
import Data.Vector.Unboxed ()
import qualified Data.Map as Map
import qualified Data.Vector.Unboxed as UV
import Data.Tuple
import GHC.Exts
\end{code}

\subsection{Huffman Tree}

We need to define the Huffman tree, where holds the item itself and a value about frequency.
Such a tree is a binary tree, so each node just needs two sub-node.
\begin{code}
data HTree e v = Item !e !v
               | Node (HTree e v) (HTree e v)
               deriving (Show,Eq)
\end{code}

Because we need sort the trees, or say elements which are boxed by \lstinline|HTree|,
there \lstinline|HTree| need to be the instance of type class \lstinline|Ord|.

\begin{code}
instance (Ord v,Num v,Eq e) => Ord (HTree e v) where
  compare a b = countHTV a `compare` countHTV b
\end{code}

There are a method, named \lstinline|countHTV|, short for count Huffman tree's value.
That method is used to count the sum of the Huffman tree, and then, the tree is comparable.

\begin{code}
countHTV :: Num v => HTree e v -> v
countHTV (Item _ v)   = v
countHTV (Node n1 n2) = let v1 = countHTV n1
                            v2 = countHTV n2
                        in v1 `seq` v2 `seq` v1+v2
\end{code}

\subsection{Build Tree}

Next step is building trees. The operation of the trees is simple. After there is list of the Huffman trees,
it just need to be sort.Then take first two trees in the list, combine them to one tree, and insert back into list of trees.
Finally, do its again until the number of list down to 1.

\begin{code}
buildHTree :: (Eq e, Num v, Ord v) => [HTree e v] -> HTree e v
buildHTree [] = error "can not be empty"
buildHTree (x:[]) = x
buildHTree xs' = buildHTree $! (Node x1 x2):xs
  where x1:x2:xs = sort xs'
\end{code}

\subsection{Encode \& Decode}

Finally, there is one thing needed to be done: encode and decode.
Before encode and decode, to accelecate encoding, there is a table for encodeing is needed.

\begin{code}
buildTable :: Ord e => HTree e v -> Map e (UV.Vector Bool)
buildTable t = buildTableStep UV.empty t Map.empty
  where buildTableStep bits (Item e v) maps = Map.insert e (UV.reverse bits) maps
        buildTableStep bits (Node n1 n2) maps =
          let bit1 = True `UV.cons` bits
              bit2 = False `UV.cons` bits
          in buildTableStep bit1 n1 $ buildTableStep bit2 n2 maps
\end{code}

Then there are the methods for encoding and decoding.
\begin{code}
encode :: (UV.Unbox e,Ord e) => Map e (UV.Vector Bool) -> UV.Vector e -> UV.Vector Bool
encode maps = UV.foldr ((UV.++) <$> (maps Map.!)) UV.empty
decode :: (UV.Unbox e) => HTree e v -> UV.Vector Bool -> UV.Vector e
decode tree xs = decodeStep tree xs UV.empty
  where decodeStep (Item e v) xs list = decodeStep tree xs $! e `UV.cons` list
        decodeStep (Node n1 n2) xs list =
          if UV.null xs then UV.reverse list
          else let x = UV.head xs
                   n = if x then n1 else n2
               in decodeStep n (UV.tail xs) list
\end{code}

\subsection{Test}



\begin{table}[h!]
    \centering
    \begin{tabular}{|c|c|c|c|}
        \hline Letter & Frequency(\%) & Letter & Frequency(\%) \\
        \hline a &  8.167 & b & 
        1.492 \\ \hline
         c &
        2.782 & d & 
        4.253 \\ 
        \hline e & 
        12.702 & f & 
        2.228 \\\hline
         g & 
        2.015 & h & 
        6.094 \\
        \hline i & 
        6.966 & j & 
        0.153 \\\hline
         k & 
        0.772 & l & 
        4.025 \\
        \hline m & 
        2.4069 & n & 
        6.749 \\\hline
         o & 
        7.507 & p & 
        1.929 \\ 
        \hline q & 
        0.095 & r & 
        5.987 \\\hline
         s & 
        6.327 & t & 
        9.056 \\ 
        \hline u & 
        2.758 & v & 
        0.978 \\\hline
         w & 
        2.360 & x & 
        0.150 \\ 
        \hline y & 1.974 & z & 
        0.074 \\ 
        \hline 
    \end{tabular}
    \caption{The Frequncy of Letter in English}
    \label{tab:lf}
\end{table}

Then we using the table \ref{tab:lf} as the frequencies. And encode "hellowhuffman".

\begin{ghci}
ghciLL> let tree = buildHTree [Item 'a' 8.167,Item 'b' 1.492,Item 'c' 2.782,Item 'd' 4.253,Item 'e' 12.702,Item 'f' 2.228,Item 'g' 2.015,Item 'h' 6.094,Item 'i' 6.966,Item 'j' 0.153,Item 'k' 0.772,Item 'l' 4.025,Item 'm' 2.406,Item 'n' 6.749,Item 'o' 7.507,Item 'p' 1.929,Item 'q' 0.095,Item 'r' 5.987,Item 's' 6.327,Item 't' 9.056,Item 'u' 2.758,Item 'v' 0.978,Item 'w' 2.360,Item 'x' 0.150,Item 'y' 1.974,Item 'z' 0.074]
ghciLL> tree
Node (Node (Node (Item 't' 9.056) (Node (Node (Node (Item 'v' 0.978) (Node (Node (Node (Item 'z' 7.4e-2) (Item 'q' 9.5e-2)) (Node (Item 'x' 0.15) (Item 'j' 0.153))) (Item 'k' 0.772))) (Item 'f' 2.228)) (Node (Item 'w' 2.36) (Item 'm' 2.406)))) (Node (Node (Node (Item 'u' 2.758) (Item 'c' 2.782)) (Item 'r' 5.987)) (Node (Item 'h' 6.094) (Item 's' 6.327)))) (Node (Node (Item 'e' 12.702) (Node (Item 'n' 6.749) (Item 'i' 6.966))) (Node (Node (Node (Node (Item 'b' 1.492) (Item 'p' 1.929)) (Node (Item 'y' 1.974) (Item 'g' 2.015))) (Item 'o' 7.507)) (Node (Item 'a' 8.167) (Node (Item 'l' 4.025) (Item 'd' 4.253)))))
ghciLL> let table = buildTable tree
ghciLL> table
fromList [('a',[False,False,False,True]),('b',[False,False,True,True,True,True]),('c',[True,False,True,True,False]),('d',[False,False,False,False,False]),('e',[False,True,True]),('f',[True,True,False,True,False]),('g',[False,False,True,True,False,False]),('h',[True,False,False,True]),('i',[False,True,False,False]),('j',[True,True,False,True,True,False,True,False,False]),('k',[True,True,False,True,True,False,False]),('l',[False,False,False,False,True]),('m',[True,True,False,False,False]),('n',[False,True,False,True]),('o',[False,False,True,False]),('p',[False,False,True,True,True,False]),('q',[True,True,False,True,True,False,True,True,False]),('r',[True,False,True,False]),('s',[True,False,False,False]),('t',[True,True,True]),('u',[True,False,True,True,True]),('v',[True,True,False,True,True,True]),('w',[True,True,False,False,True]),('x',[True,True,False,True,True,False,True,False,True]),('y',[False,False,True,True,False,True]),('z',[True,True,False,True,True,False,True,True,True])]
ghciLL> let en = encode table (UV.fromList "hellohuffman")
ghciLL> en
[True,False,False,True,False,True,True,False,False,False,False,True,False,False,False,False,True,False,False,True,False,True,False,False,True,True,False,True,True,True,True,True,False,True,False,True,True,False,True,False,True,True,False,False,False,False,False,False,True,False,True,False,True]
*Codec.Compression.Huffman> decode tree en
"hellohuffman"
\end{ghci}

