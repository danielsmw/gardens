module Main where

import           Codec.Picture
import           Codec.Picture.Types (pixelMapXY, dynamicPixelMap)
import           Control.Monad       (liftM)
import           System.Environment
import Debug.Trace

shrunkImage :: Pixel p => Image p -> Image p
shrunkImage img = 
  generateImage 
    (\x y -> pixelAt img (x*2) (y*2))
    (imageWidth  img `div` 2)
    (imageHeight img `div` 2)

rotateImage :: Pixel p => Image p -> Image p
rotateImage img =
  generateImage
    (\x y -> pixelAt img (pred (imageHeight img) - y) (x))
    (imageWidth  img)
    (imageHeight img)

reconstructImage :: Pixel p => Image p -> Image p
reconstructImage img =
  let sImg = shrunkImage img
   in generateImage
        (\x y -> 
          case (x <= halfWidth, y <= halfHeight)
            of (True , True ) -> pixelAt (    r . r $ sImg) (x `rem` mid) (y `rem` mid)
               (True , False) -> pixelAt (r . r . r $ sImg) (x `rem` mid) (y `rem` mid)
               (False, False) -> pixelAt (            sImg) (x `rem` mid) (y `rem` mid)
               (False, True ) -> pixelAt (        r $ sImg) (x `rem` mid) (y `rem` mid)
        )
      (imageWidth  img)
      (imageHeight img)
  where halfWidth  = imageWidth  img `div` 2
        halfHeight = imageHeight img `div` 2
        mid = (imageWidth img `div` 2)
        r = rotateImage

sow :: Pixel p => Image p -> Image p
sow img = pixelMapXY (\x y p -> (uncurry $ pixelAt img)
                              $ landscape n (x,y)
                     ) img
  where m = imageWidth img
        n = imageHeight img

downscaleForward :: (Int, Int) -> Maybe (Int, Int)
downscaleForward (xOld, yOld)
  | even (succ xOld) && even (succ yOld) = Just (xOld `div` 2, yOld `div` 2)
  | otherwise = Nothing
  
landscape :: Int -> (Int, Int) -> (Int, Int)
landscape m (xNew, yNew)
  | xNew <= mid && yNew <= mid = (    x `mod` mid,     y`mod`mid)
  | xNew  > mid && yNew <= mid = (    y `mod` mid,   (n $ x) `mod`mid)
  | xNew  > mid && yNew  > mid = (    (n $ x) `mod` mid ,     (n $ y) `mod` mid)
  | xNew <= mid && yNew  > mid = (    (n $ y) `mod` mid,     x `mod` mid)
  where mid = m `div` 2
        n k = pred m - k
        x = 2 * ((succ $ xNew) `rem` mid)
        y = 2 * ((succ $ yNew) `rem` mid)


{-
landscape :: Pixel p => (Int,Int) -> Int -> Int -> p -> (Int, Int)
landscape (m,n) x' y' p =
  let x = 2 * (x' `rem` (m `div` 2))
      y = 2 * (y' `rem` (n `div` 2))
   in case ( x' <= m `div` 2 , y' <= n `div` 2 )
        of ( True,  True) -> (     x,      y)
           ( True, False) -> (negY y,      x)
           (False, False) -> (negX x, negY y)
           (False,  True) -> (     y, negX x)
  where negX x = (negate (x - m `div` 2)) + m `div` 2
        negY y = (negate (y - n `div` 2)) + n `div` 2
-}

main = do
  img <- liftM head getArgs >>= readImage
  mapM_ (\(j,pic) -> savePngImage (show j ++ ".png") pic)
    $ zip [0..100]
    $ either error (iterate (dynamicPixelMap reconstructImage)) img

      {-
instance (IArray UArray e, Ix i) => Ixed (UArray i e) where
  ix i f arr
    | inRange (bounds arr) i = f (arr Array.! i) <&> \e -> arr Array.// [(i,e)]
    | otherwise              = pure arr
  {-# INLINE ix #-}

instance (Pixel p) => Ixed (Image p) where
  ix i f img
    | inRange (bounds arr) i = f (pixelAt img i) <&> \e -> arr Array.// [(i,e)]
    | otherwise              = pure arr
  {-# INLINE ix #-}
-}
