{-|
Module      : Graphics.Hpeg.Color
Description : The methods for transform between RGB and YCbCr
Copyright   : (C) Qinka 2017
License     : GPL-3
Maintainer  : qinka@live.com
Stability   : experimental
Portability : unknow

The methods for tansform between RGB and YCbCr
-}

module Graphics.Hpeg.Color
       ( -- * YCbCr to RGB
         -- ** operators
         toRedOpt
       , toBlueOpt
       , toGreenOpt
       , -- ** method to transform
         toRed
       , toBlue
       , toGreen
       , -- * RGB to YCbCr
         -- ** operators
         toYOpt
       , toCbOpt
       , toCrOpt
       , -- ** method to transform
         toY
       , toCb
       , toCr
       , -- * types
         TriColor(..)
       ) where

import           Data.Array.Accelerate ()
import qualified Data.Array.Accelerate as A
import           Data.Binary
import           Data.Word

-- | The operator to get red
toRedOpt :: Exp Word8 -- ^ Y
         -> Exp Word8 -- ^ Cr
         -> Exp Word8 -- ^ R
toRedOpt y' cr' = A.round $ y + 1.402 * (cr - 128)
  where y  = A.toFloating y'
        cr = A.toFloating cr'

-- | the operator to get blue
toBlueOpt :: Exp Word8 -- ^ Y
          -> Exp Word8 -- ^ Cb
          -> Exp Word8 -- ^ B
toBlueOpt y' cb' = A.round $ y + 1.772 * (cb - 128)
  where y  = A.toFloating y'
        cb = A.toFloating cb'

-- | the operator to get green
toGreenOpt :: Exp Word8 -- ^ Y
           -> Exp Word8 -- ^ Cb
           -> Exp Word8 -- ^ Cr
           -> Exp Word8 -- ^ G
toGreenOpt y' cb' cr' = A.round $ y - 0.34414 * (cb - 128) - 0.71414 (cr - 128)
  where y  = A.toFloating y'
        cb = A.toFloating cb'
        cr = A.toFloating cr

-- | the operator to get Y
toYOpt :: Exp Word8 -- ^ R
       -> Exp Word8 -- ^ G
       -> Exp Word8 -- ^ B
       -> Exp Word8 -- ^ Y
toYOpt r' g' b' = A.round $ 0.299 * r + 0.587 * g + 0.114 * b
  where r = A.toFloating r'
        g = A.toFloating g'
        b = A.toFloating b'

-- | the operator to get Cb
toCbOpt :: Exp Word8 -- ^ R
        -> Exp Word8 -- ^ G
        -> Exp Word8 -- ^ B
        -> Exp Word8 -- ^ Cb
toCbOpt r' g' b' = A.round $ (-0.168) * r + (-0.3313) * g + 0.5 * b + 128
  where r = A.toFloating r'
        g = A.toFloating g'
        b = A.toFloating b'
-- | the operator to get Cr
toCrOpt :: Exp Word8 -- ^ R
        -> Exp Word8 -- ^ G
        -> Exp Word8 -- ^ B
        -> Exp Word8 -- ^ Cr
toCrOpt r' g' b' = A.round $ 0.5 * r + (-0.418) * g + 0.0813 * b + 128
  where r = A.toFloating r'
        g = A.toFloating g'
        b = A.toFloating b'


-- | to get red (matrix)
toRed :: Acc (Array DIM2 Word8) -- ^ Ys
      -> Acc (Array DIM2 Word8) -- ^ Crs
      -> Acc (Array DIM2 Word8) -- ^ Rr
toRed = A.zipWith toRedOpt

-- | to get blue (matrix)
toBlue :: Acc (Array DIM2 Word8) -- ^ Ys
       -> Acc (Array DIM2 Word8) -- ^ Cbs
       -> Acc (Array DIM2 Word8) -- ^ Br
toBlue = A.zipWith toBlueOpt

-- | to get green (matrix)
toGreen :: Acc (Array DIM2 Word8) -- ^ Ys
        -> Acc (Array DIM2 Word8) -- ^ Cbs
        -> Acc (Array DIM2 Word8) -- ^ Crs
        -> Acc (Array DIM2 Word8) -- ^ Rr
toGreen = A.zipWith3 toGreenOpt

-- | to get luma (matrix)
toY :: Acc (Array DIM2 Word8) -- ^ Rs
    -> Acc (Array DIM2 Word8) -- ^ Gs
    -> Acc (Array DIM2 Word8) -- ^ Bs
    -> Acc (Array DIM2 Word8) -- ^ Ys
toY = A.zipWith3 toYOpt

-- | to get the chroma(blue) (matrix)
toCb :: Acc (Array DIM2 Word8) -- ^ Rs
     -> Acc (Array DIM2 Word8) -- ^ Gs
     -> Acc (Array DIM2 Word8) -- ^ Bs
     -> Acc (Array DIM2 Word8) -- ^ Cbs
toCb = A.zipWith3 toCbOpt

-- | to get the chroma(red) (matrix)
toCr :: Acc (Array DIM2 Word8) -- ^ Rs
     -> Acc (Array DIM2 Word8) -- ^ Gs
     -> Acc (Array DIM2 Word8) -- ^ Bs
     -> Acc (Array DIM2 Word8) -- ^ Crs
toCr = A.zipWith3 toCrOpt

-- | To hold a tri-color
data TriColor = TriColor !Word8 !Word8 !Word8

instance Binary TriColor where
  put (TriColor a b c) = do
    put a
    put b
    put c
  get = do
    a <- get
    b <- get
    c <- get
    return (TriColor a b c)
