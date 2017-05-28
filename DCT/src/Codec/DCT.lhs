
\section{Discrete Cosine Transformation}
\label{sec:dct}

\begin{code}
module Codec.DCT
       ( dct2
       ) where

import Data.Array.Accelerate (All(..),Elt,Z(..),Acc,Array,DIM1,DIM2,Shape,Exp,(:.)(..),ToFloating,(?|))
import qualified Data.Array.Accelerate as A
\end{code}

\subsection{2D Discrete Cosine Transformation}
\label{sec:dct:dct2}

Discrete Cosine Transformation, DCT for short, is a widely used transform coding technique. For 2D Discrete Cosine Transformation, the definition is Eq.\ref{eq:dct2}.
\begin{equation}
  \label{eq:dct2}
  F(u,v) = \frac{2C(u)C(v)}{\sqrt{MN}}\sum\limits_{i=0}^{M-1}\sum\limits_{j=0}^{N-1}\cos{\frac{(2i+1)u\pi}{2M}}\cos{\frac{(2j+1)v\pi}{2N}}f(i,j)
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

Because of the Haskell's \textit{accelecate} library do not support nest parallel.
To calculate, the matrixs and the equation should be spread at high dimension.
For this equation, the matrixs are spread to 4D. Then caluculate them. Finally,
the high-dimensional matrix will be dimension-reduced and folded to a normal matrix,
result.

\begin{code}
dct2 :: (Elt e,A.Num e,A.Floating e,ToFloating Int e) => Acc (Array DIM2 e) -> Acc (Array DIM2 e)
dct2 arr = A.zipWith (*) cs $ A.fold (+) 0 $ A.fold (+) 0 $ A.zipWith5 com is js us vs ar
  where Z :. rowM :. colN = A.unlift (A.shape arr) :: Z :. Exp Int :. Exp Int
        com :: (Elt e,A.Floating e,ToFloating Int e) => Exp e -> Exp e -> Exp e -> Exp e -> Exp e -> Exp e
        com i j u v f =  A.cos (i*u) * A.cos(j*v) * f
        bu = A.generate  (A.lift $ Z :. rowM) $ \i -> A.toFloating $ A.unindex1 i
        bv = A.generate  (A.lift $ Z :. colN) $ \i -> A.toFloating $ A.unindex1 i
        us = A.replicate (A.lift $ Z :. All  :. colN :. rowM :. colN) bu
        vs = A.replicate (A.lift $ Z :. rowM :. All  :. rowM :. colN) bv
        is = A.replicate (A.lift $ Z :. rowM :. colN :. All  :. colN) $ mk rowM
        js = A.replicate (A.lift $ Z :. rowM :. colN :. rowM :. All ) $ mk colN
        ar = A.replicate (A.lift $ Z :. rowM :. colN :. All  :. All ) arr
        cs = A.generate  (A.lift $ Z :. rowM :. colN) (mkCoe rowM colN)
\end{code}

\subsection{2D Inverse Discrete Cosine Transformation}
\label{sec:dct:idct2}

Next one is the inverse one. The following Eq.\ref{eq:idct2} is the \textbf{2D Inverse Discrete Cosine Transform}, 2D IDCT for short.
\begin{equation}
    \label{eq:idct2}
    \widetilde{f}(i,j) = \sum\limits_{u=0}^{M-1}\sum\limits_{v=0}^{N-1}\frac{2C(u)C(v)}{\sqrt{MN}}\cos{\frac{(2i+1)u\pi}{2M}}\cos{\frac{(2j+1)v\pi}{2N}}F(i,j)
\end{equation} 

\begin{code}
idct2 :: (Elt e,A.Num e,A.Floating e,ToFloating Int e) => Acc (Array DIM2 e) -> Acc (Array DIM2 e)
idct2 arr = A.fold (+) 0 $ A.fold (+) 0 $ A.zipWith6 icom cs is js us vs ar
  where Z :. rowM :. colN = A.unlift (A.shape arr) :: Z :. Exp Int :. Exp Int
        icom :: (Elt e,A.Floating e,ToFloating Int e) => Exp e -> Exp e -> Exp e -> Exp e -> Exp e -> Exp e -> Exp e
        icom c i j u v f = c * A.cos (i*u) * A.cos(j*v) * f
        bu = A.generate  (A.lift $ Z :. rowM) $ \i -> A.toFloating $ A.unindex1 i
        bv = A.generate  (A.lift $ Z :. colN) $ \i -> A.toFloating $ A.unindex1 i
        is = A.replicate (A.lift $ Z :. All  :. colN :. rowM :. colN) $ mk rowM
        js = A.replicate (A.lift $ Z :. rowM :. All  :. rowM :. colN) $ mk colN
        us = A.replicate (A.lift $ Z :. rowM :. colN :. All  :. colN) bu
        vs = A.replicate (A.lift $ Z :. rowM :. colN :. rowM :. All ) bv
        ar = A.replicate (A.lift $ Z :. rowM :. colN :. All  :. All ) arr
        cs = A.replicate (A.lift $ Z :. rowM :. colN :. All  :. All ) $
             A.generate  (A.lift $ Z :. rowM :. colN) (mkCoe rowM colN)
\end{code}



\begin{code}
cFunc :: (A.Floating e) => Exp Int -> Exp e
cFunc i = A.ifThenElse (i A.== 0) (A.sqrt 2 / 2) 1
mk :: (Elt e,A.Floating e,A.ToFloating Int e) => Exp Int -> Acc (Array DIM1 e)
mk len = A.generate (A.lift $ Z :. len) $ \i' ->
  let i = A.toFloating $ A.unindex1 i'
  in (2 * i + 1) * A.pi / 2 / (A.toFloating len)
mkCoe :: (ToFloating Int e,Elt e,A.Floating e) => Exp Int -> Exp Int -> Exp DIM2 -> Exp e
mkCoe rowM colN sh = let Z :. u :. v = A.unlift sh :: Z :. Exp Int :. Exp Int
                     in 2 * cFunc u * cFunc v / A.sqrt (A.toFloating $ rowM * colN)
\end{code}


\subsection{Test}
\label{sec:dct:test}

Then we will test with the example on the text book(page 257).

The matrix is the following.

\begin{spec}
test :: Array DIM2 Float
test = A.fromList (Z :. 8:. 8)
       [ 200,202,189,188,189,175,175,175
       , 200,203,198,188,189,182,178,175
       , 203,200,200,195,200,187,185,175
       , 200,200,200,200,197,187,187,187
       , 200,205,200,200,195,188,187,175
       , 200,200,200,200,200,190,187,175
       , 205,200,199,200,191,187,187,175
       , 210,200,200,200,188,185,187,186
       ]
\end{spec}

For most GPUs, the computing on float number with single precision is faster than that of double prcision float number.

So the things test with \lstinline[language=Haskell]|Float|.

\begin{spec}
let rt = I.run $ dct2 $ A.map (\x -> x - 128) $ use test
\end{spec}


The result is 
\begin{spec}
Matrix (Z :. 8 :. 8)
    [ 514.87494,   65.0169,-11.819897,  3.5716007,   1.375021,   2.253157, -7.9574976,   4.8891025,
    -15.89108, 3.4146214, 1.9508953,   0.370826,-0.21789217, -11.213463,  -2.050467,   3.3166811,
    -12.341854, 6.3602486, 11.473793,-0.77182484,   3.079874, 0.42452312,  1.2777319,  -1.8869154,
    -7.762983, 2.6106358, -4.427642,  2.3042147, -2.3899536, -2.5522923, -4.6448708,  -1.5002513,
    0.37503007,-2.0596218,   6.55122,  -5.350648,  4.3750033,-0.47413588, -1.4959126,  -3.5484176,
    0.12198714,-3.0049448,-0.7559264,  0.3190322,     4.1586,  1.1957896, -1.1196257,  0.26229882,
    2.5414536,-1.5842085,-3.2222686,  3.0395823,   3.189147,-0.65972424,-0.72379637,   2.9414072,
    -1.5439692,  4.688013,-1.6742547,  3.5618114, -2.3736508,  1.8760357, -2.5607927,8.5374415e-2]
\end{spec}

Meanwhile, \lstinline|idct2| will be test:

\begin{spec}
let irt = I.run $ A.map (+128) $ idct2 $ use rt
\end{spec}

The result is
\begin{spec}
Matrix (Z :. 8 :. 8)
    [199.99998,201.99997,    189.0,    188.0,    189.0,174.99997,175.00003,    175.0,
    199.99997,202.99994,197.99995,187.99995,188.99997,181.99994,    178.0,174.99997,
    203.0,199.99997,    200.0,    195.0,    200.0,186.99995,185.00003,    175.0,
    199.99998,199.99997,    200.0,199.99998,    197.0,186.99995,187.00002,186.99998,
    200.0,204.99997,199.99998,199.99998,    195.0,187.99995,187.00003,    175.0,
    199.99994,199.99994,199.99994,199.99994,199.99997,189.99992,186.99998,174.99997,
    205.00003,    200.0,199.00003,200.00003,191.00003,    187.0,187.00005,175.00002,
    210.0,199.99997,    200.0,    200.0,    188.0,184.99995,187.00002,    186.0]
\end{spec}

Finally, the error range will be calucated.

\begin{spec}
let de = I.run $ A.zipWith (-) test irt
\end{spec}

The result is
\begin{spec}
Matrix (Z :. 8 :. 8)
    [ 1.5258789e-5,3.0517578e-5,          0.0,          0.0,          0.0,3.0517578e-5,-3.0517578e-5,          0.0,
    3.0517578e-5,6.1035156e-5, 4.5776367e-5, 4.5776367e-5, 3.0517578e-5,6.1035156e-5,          0.0, 3.0517578e-5,
    0.0,3.0517578e-5,          0.0,          0.0,          0.0,4.5776367e-5,-3.0517578e-5,          0.0,
    1.5258789e-5,3.0517578e-5,          0.0, 1.5258789e-5,          0.0,4.5776367e-5,-1.5258789e-5, 1.5258789e-5,
    0.0,3.0517578e-5, 1.5258789e-5, 1.5258789e-5,          0.0,4.5776367e-5,-3.0517578e-5,          0.0,
    6.1035156e-5,6.1035156e-5, 6.1035156e-5, 6.1035156e-5, 3.0517578e-5,7.6293945e-5, 1.5258789e-5, 3.0517578e-5,
    -3.0517578e-5,         0.0,-3.0517578e-5,-3.0517578e-5,-3.0517578e-5,         0.0,-4.5776367e-5,-1.5258789e-5,
    0.0,3.0517578e-5,          0.0,          0.0,          0.0,4.5776367e-5,-1.5258789e-5,          0.0]
\end{spec}

The precision seems like not to bad.