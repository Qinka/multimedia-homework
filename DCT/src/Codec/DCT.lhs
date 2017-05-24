
\section{Discrete Cosine Transformation}
\label{sec:dct}

\begin{code}
module Codec.DCT
       ( base2DDCT
       ) where

import Data.Array.Accelerate (All(..),Elt,Z(..),Acc,Array,DIM1,DIM2,Shape,Exp,(:.)(..),ToFloating)
import qualified Data.Array.Accelerate as A
\end{code}

Discrete Cosine Transformation, DCT for short, is a widely used transform coding technique. For 2D Discrete Cosine Transformation, the definition is Eq.\ref{}.
\begin{equation}
  \label{eq:2ddct}
  F(u,v) = \frac{2C(u)C(v)}{\sqrt{MN}}\sum\limit_{i=0}^{M-1}\sum\limit_{j=0}^{N-1}\cos{\frac{(2i+1)u\pi}{2M}}\cos{\frac{(2j+1)v\pi}{2N}}f(i,j)
\end{equation}
where $i,u=0,1,\cdots,M-1$,$j,v=0,1,...,N-1$, and the constants $C(u)$ and $C(v)$ are determined by
\begin{equation}
  \label{eq:2dct}
  C(\xi) = \left\{
    \begin{array}{cc}
      \frac{\sqrt{2}}{2} & \xi = 0, \\
      1 & otherwise.
    \end{array}\right.
\end{equation}

\begin{code}
base2DDCT :: (Elt e,A.Num e,A.Floating e,ToFloating Int e) => Acc (Array DIM2 e) -> Acc (Array DIM2 e)
base2DDCT arr = A.fold (+) 0 $ A.fold (+) 0 $ A.zipWith5 com is js us vs ar
  where Z :. rowM :. colN = A.unlift (A.shape arr) :: Z :. Exp Int :. Exp Int
        mk :: (Elt e,A.Floating e,A.ToFloating Int e) => Exp Int -> Acc (Array DIM1 e)
        mk len = A.generate (A.lift $ Z :. len) $ \i' ->
          let i = A.toFloating $ A.unindex1 i'
          in (2 * i + 1) * A.pi / 2 / (A.toFloating len)
        com :: (Elt e,A.Floating e,ToFloating Int e) => Exp e -> Exp e -> Exp e -> Exp e -> Exp e -> Exp e
        com i j u v f = A.cos (i*u) * A.cos(j*v) * f
        bu = A.generate  (A.lift $ Z :. rowM) $ \i -> A.toFloating $ A.unindex1 i
        bv = A.generate  (A.lift $ Z :. colN) $ \i -> A.toFloating $ A.unindex1 i
        us = A.replicate (A.lift $ Z :. All  :. colN :. rowM :. colN) bu
        vs = A.replicate (A.lift $ Z :. rowM :. All  :. rowM :. colN) bv
        is = A.replicate (A.lift $ Z :. rowM :. colN :. All  :. colN) $ mk rowM
        js = A.replicate (A.lift $ Z :. rowM :. colN :. rowM :. All ) $ mk colN
        ar = A.replicate (A.lift $ Z :. rowM :. colN :. All  :. All ) arr
\end{code}
