module Image where

import Control.Monad
import Control.Monad.State
import Data.Array
import Data.Word (Word8)

type Color = Int
data Image = Image Int Int (Array (Int,Int) Color)

empty :: Int -> Int -> Image
empty w h = Image w h pxls
  where pxls = array ((1,1),(w,h)) [((x,y),0) | y <- [1..h], x <- [1..w]]

getWidth :: Image -> Int
getWidth (Image w _ _) = w

getHeight :: Image -> Int
getHeight (Image _ h _) = h

setPixel :: Image -> Int -> Int -> Color -> Image
setPixel (Image w h pxls) x y c = Image w h pxls'
  where pxls' = pxls//[((x,y),c)]

getPixel :: Image -> Int -> Int -> Color
getPixel (Image w _ pxls) x y = pxls!(x,y)


data FileFormat = PPM

encode :: FileFormat -> Image -> String
encode PPM (Image w h pxls) = 
  "P3\n" ++ (show w) ++ " " ++ (show h) : "\n"

decode :: String -> Image
decode _ = undefined


type ImageProc = State Image

runImageProc :: Int -> Int -> ImageProc a -> (a,Image)
runImageProc w h s = runState s (empty w h)

getWidthM :: ImageProc Int
getWidthM = do
  Image w _ _ <- get
  return w

getHeightM :: ImageProc Int
getHeightM = do
  Image _ h _ <- get
  return h

getPixelM :: Int -> Int -> ImageProc Color
getPixelM x y = do
  img <- get
  let c = getPixel img x y
  return c

setPixelM :: Int -> Int -> Color -> ImageProc ()
setPixelM x y c = do
  img <- get
  let img' = setPixel img x y c
  put img'


example :: Image
example = snd $ runImageProc 10 10 $ do
  w <- getWidthM
  h <- getHeightM
  forM_ [1..h] $ \y -> do
    forM_ [1..w] $ \x -> do
      setPixelM x y 11

main :: IO ()
main = do
  let img = example
  print $ getPixel img 1 1
