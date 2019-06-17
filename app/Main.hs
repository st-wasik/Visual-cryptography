module Main where
import Graphics.Image as I
import System.Random
import Data.Matrix as M

--chinskie twierdzenie o resztach

data Pixel44 = Pixel44
    { upList :: [Pixel RGBA Word8]  
    , downList :: [Pixel RGBA Word8] 
    } deriving (Eq)

b = black
w = white

instance Show Pixel44 where 
    show (Pixel44 upList downList) = fmap (\(PixelRGBA r g b a) -> if a == 0 then 'W' else 'B') $ upList ++ downList

type Pixel4 = ([Pixel RGBA Word8], [Pixel RGBA Word8])
type Pixel4M = Matrix Pixel4

whiteBlackImage :: Image VU RGBA Word8 -> Image VU RGBA Word8
whiteBlackImage image = I.fromLists enc
    where lists = I.toLists image
          enc = fmap (fmap (\(PixelRGBA a b c _) -> if (quot (a+b+c) 5) > 25 then (PixelRGBA 0 0 0 255) else (PixelRGBA 255 255 255 255))) lists

wb = do
    image <- readImageRGBA VU "szymkez.png"
    let i = whiteBlackImage $ toWord8I image
    writeImage "sz2.png" $ toDoubleI i

main :: IO ()
main = do
    image <- readImageRGBA VU "sz2.png"
    gen <- newStdGen
    let rands = randomRs (0,5) gen :: [Int]
    let i = toWord8I image

    let (fstImage, sndImage) = processImage i rands

    let a = I.fromLists fstImage :: Image VU RGBA Word8

    let b = I.fromLists sndImage :: Image VU RGBA Word8

    writeImage "osz1.png" $ toDoubleI a 
    writeImage "osz2.png" $ toDoubleI b 
    return ()

white = PixelRGBA 0 0 0 0
black = PixelRGBA 0 0 0 255

pTop = ([black,black],[white,white])
pBottom = ([white,white],[black,black])
pLeft = ([black,white],[black,white])
pRight = ([white,black],[white,black])
pFromLeft = ([black,white],[white,black])
pFromRight = ([white,black],[black,white])

pixels = [pTop, pBottom, pLeft, pRight, pFromLeft, pFromRight]

concat4Pixels :: Foldable t => [t ([a], [a])] -> [[a]]
concat4Pixels [] = []
concat4Pixels (p:px) = foldl (\a (b,_) -> a++b) [] p : [foldl (\a (_,b) -> a++b) [] p] ++ concat4Pixels px

processImage :: Image VU RGBA Word8 -> [Int] -> ([[Pixel RGBA Word8]], [[Pixel RGBA Word8]])
processImage image rands = (a,b)
    where a = concat4Pixels $ M.toLists img1
          b = concat4Pixels $ M.toLists img2
          irows = rows image
          icols = cols image
          emptyImage = M.matrix irows icols (\_ -> ([black,black], [black,black]))
          (img1, img2) = encrypt 1 1 emptyImage emptyImage image rands

encrypt :: Int -> Int -> Pixel4M -> Pixel4M -> Image VU RGBA Word8 -> [Int] -> (Pixel4M, Pixel4M)
encrypt x y img1 img2 image (r:rs)
    | x > iwidth = encrypt 1 (y+1) img1 img2 image (r:rs)
    | y > iheight = (img1, img2)
    where iwidth = rows image
          iheight = cols image

encrypt x y img1 img2 image (r:rs) = encrypt (x+1) y img11 img22 image rs
    where px = index image (x-1,y-1)
          (img11, img22) = modifyImageWithPixel px r (x,y) img1 img2

modifyImageWithPixel :: Pixel RGBA Word8 -> Int -> (Int, Int) -> Pixel4M -> Pixel4M -> (Pixel4M, Pixel4M)
modifyImageWithPixel px r (x,y) img1 img2 = (img11, img22)
    where (p1, p2) = getPixels (px, r)
          img11 = setElem p1 (x,y) img1
          img22 = setElem p2 (x,y) img2

getPixels :: (Pixel RGBA Word8, Int) -> (Pixel4, Pixel4)
getPixels (pixel, r)
    | pixel == black = (randPixel, get2ndBlack randPixel)
    | otherwise = (randPixel, randPixel)
    where randPixel = pixels !! r

get2ndBlack :: Pixel4 -> Pixel4
get2ndBlack x 
    | x == pTop = pBottom
    | x == pBottom = pTop
    | x == pLeft = pRight
    | x == pRight = pLeft
    | x == pFromLeft = pFromRight
    | x == pFromRight = pFromLeft
    | otherwise = error "o no no no no"




























--stan procesu to wartości liczbowe minimalnego zbioru zmiennych procesowych kt znajomość wchwili t oraz znajomość w okresie od t do t+/\ pozwala określić stan w chwili t+t/\ 