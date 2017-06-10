{-|
Module      : Graphics.Hpeg.Matrix
Description : The methods about matrix
Copyright   : (C) Qinka, 2017
License     : GPL-3
Maintainer  : qinka@live.com
Stability   : experimental
Portability : unknow

The methods about the matrix to translate/form.
-}

module Graphics.Hpeg.Matrix
       (
       ) where

import           Data.Array            as Array
import           Data.Array.Accelerate ()
import qualified Data.Array.Accelerate as A
import           Data.Bits
import           Data.Word             (Word16, Word8)
import qualified Data.Word             as Word

-- | Create the index of the block
mkBlockIndex :: Word16 -> Word16 -> [(Word16,Word16)]
mkBlockIndex u v = [(a,b) | a <- [0..u-1], b <- [0..v-1]]

mkBlock :: Array (Word32,Word32) Word8 -> (Word16,Word16) -> [(Word8,Word8)]
mkBlock arr bi bj  = filter ((/=0).snd) $ map trans items
  where items = [(i,j,arr ! mkIndex bi bj i j) | i <- [0..7], j <- [0..7]]
        trans (i',j',x') = let i = fromIntegral i'
                               j = fromIntegral j'
                               x = fromIntegral x'
                           in (i*16+j,x)
toBlocks :: Array (Word32,Word32) Word8 -> [[[(Word8,Word8)]]]
toBlocks arr = map (mk) indexs
  where (u,v) = maximum $ indices arr
        mkL k = map (mkBlock arr) [(p,q0..l]
        blockSize var = fromIntegral $ div var 8 + 
        indexs = mkBlockIndex (blockSize u) (blockSize v)
  

mkIndex :: Word16 -> Word16 -> Word8 -> Word8 -> (Word32,Word32)
mkIndex bi' bj' i' j' = (bi*8+i,bj*8+j)
  where bi = fromIntegral bi'
        bj = fromIntegral bj'
        i  = fromIntegral i'
        j  = fromIntegral j'

fromBlock :: [[(Word8,Word8)]] -> Array (Word32,Word32) Word8
fromBlock its =
  where 


kmBlock :: Word16 -> Word16 -> (Word8,Word8) -> ((Word32,Word32),Word8)
kmBlock bi' bj' (ij,x) = ((bi*8+i,bj*8+j),x)
  where i = fromIntegral $ div ij 16
        j = fromIntegral $ mod ij 16
        bi = fromIntegral bi'
        bj = fromIntegral bj'

