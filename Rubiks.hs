{-# LANGUAGE RankNTypes #-}

module Rubiks where

{-

Top view:

 O
GWB
 R

Front view:

 W
GRB
 Y

Corners:
with clockwise from facing top:
bottom 4 starting from DLF
top 4 starting from ULF

Edges:
with clockwise from facing top:
bottom 4 starting from
    

-}


-- Order: UDFBRL

newtype Cube = Cube { faces :: [Face] } deriving Show

{-
Sides: 0-8, up left to bottom right
top: 0-8, back up left to front up right
bottom: 0-8, front down left to back down right 
-}

newtype Face = Face { colours :: [Colour] } deriving Show

data Colour = White | Yellow | Red | Orange | Blue | Green deriving (Ord, Show, Eq, Enum)

data TurnDir = CW | CCW | Full | FlipV | FlipH | None

inv :: TurnDir -> TurnDir
inv CW = CCW
inv CCW = CW
inv x = x

solvedCube = Cube $ map (Face . replicate 9) [White .. Green]

up = 0
down = 1
front = 2
back = 3
right = 4
left = 5

faceAt :: Int -> Cube -> Face
faceAt f = (!! f) . faces


rotateL :: [a] -> [a]
rotateL (x:xs) = xs ++ [x]

rotateL2 :: [a] -> [a]
rotateL2 (x:y:xs) = xs ++ (x:[y])

-- takes `to` elements starting from `from`. Note to is the size, not ending pos
takeFrom :: (Int, Int) -> [a] -> [a]
takeFrom (from, to) = take to . drop from

              --from length      list   replacement
replaceList :: (Int, Int) -> [a] -> [a] -> [a]
replaceList (0, 0) xs ys = xs
replaceList (0, to) (_:xs) (y:ys) = y : replaceList (0, to-1) xs ys
replaceList (from, to) (x:xs) ys = x : replaceList (from-1, to) xs ys

replaceElements :: forall a. [a] -> [(Int, a)] -> [a]
replaceElements = replaceElements' 0
  where
    replaceElements' :: Int -> [a] -> [(Int, a)] -> [a]
    replaceElements' i (x:xs) repls = res : replaceElements' (i+1) xs repls
      where res = case lookup i repls of
                        Just y -> y
                        Nothing -> x

    replaceElements' _ xs [] = xs
    replaceElements' _ [] _ = []

replaceElement :: [a] -> Int -> a -> [a]
replaceElement xs i y = modifyElement xs i $ const y

modifyElement :: [a] -> Int -> (a -> a) -> [a]
modifyElement = modifyElement' 0
    where modifyElement' cur (x:xs) ind f =
            if cur == ind
            then f x : xs
            else x : modifyElement' (cur+1) xs ind f

modifyFace :: Cube -> Int -> (Face -> Face) -> Cube
modifyFace c i = Cube . modifyElement (faces c) i

rotateFace :: TurnDir -> Face -> Face
rotateFace dir (Face colours) =
    let [c0,c1,c2,c3,c4,c5,c6,c7,c8] = colours
    in case dir of
      CW ->
        Face [c6,c3,c0,c7,c4,c1,c8,c5,c2]
      CCW ->
        Face [c2,c5,c8,c1,c4,c7,c0,c3,c6]
      Full ->
        Face [c8,c7,c6,c5,c4,c3,c2,c1,c0]
      None -> Face colours
      FlipV ->
        Face [c6,c7,c8,c3,c4,c5,c0,c1,c2]
      FlipH ->
        Face [c2,c1,c0,c5,c4,c3,c8,c7,c6]

rotateFaceOnCube :: Int -> TurnDir -> Cube -> Cube
rotateFaceOnCube face dir cube = modifyFace cube face $ rotateFace dir

data Axis = X | Y | Z
-- rotation of cube is looking from right, up or front

swapFaces :: [(Int, Int, TurnDir)] -> Cube -> Cube
swapFaces toSwap cube =
    let newFaces = map (takeFace cube) toSwap
    in Cube $ replaceElements (faces cube) newFaces

takeFace :: Cube ->(Int, Int, TurnDir) -> (Int, Face)
takeFace cube (from, to, rot) = (to, rotateFace rot $ faceAt from cube)


rotateCube :: Axis -> TurnDir -> Cube -> Cube
rotateCube ax dir = swapFixed ax . swapFaces (toSwap ax dir)
  where
    faces X = [front, up, back, down]
    faces Y = [front, left, back, right]
    faces Z = [up, right, down, left]
    
    toSwap :: Axis -> TurnDir -> [(Int, Int, TurnDir)]
{-    toSwap ax CW = zip3 (faces ax) (rotateL (faces ax))
    toSwap ax CCW = zip3 (rotateL (faces ax)) (faces ax)
    toSwap ax Full = zip3 (faces ax) (rotateL2 (faces ax))
-}
   
    toSwap X CW = zip3 (faces X) (rotateL (faces X)) [None,Full,Full,None]
    toSwap X CCW = zip3 (rotateL (faces X)) (faces X) [None,Full,Full,None]
    toSwap X Full = zip3 (faces X) (rotateL2 (faces X)) [Full, None, None, Full]

    toSwap Y CW = zip3 (faces Y) (rotateL (faces Y)) (replicate 4 None)
    toSwap Y CCW = zip3 (rotateL (faces Y)) (faces Y) (replicate 4 None)
    toSwap Y Full = zip3 (faces Y) (rotateL2 (faces Y)) (replicate 4 None)

    toSwap Z CW = zip3 (faces Z) (rotateL (faces Z)) (replicate 4 CW)
    toSwap Z CCW = zip3 (rotateL (faces Z)) (faces Z) (replicate 4 CCW)
    toSwap Z Full = zip3 (faces Z) (rotateL2 (faces Z)) (replicate 4 Full)

    swapFixed :: Axis -> Cube -> Cube
    swapFixed X = rotateFaceOnCube right dir . rotateFaceOnCube left (inv dir)
    swapFixed Y = rotateFaceOnCube up dir . rotateFaceOnCube down (inv dir)
    swapFixed Z = rotateFaceOnCube front dir . rotateFaceOnCube back (inv dir)
    

-- layers 0 - 2 top to bottom
rotateVertAx :: Int -> TurnDir -> Cube -> Cube
rotateVertAx layer dir cube =
    if layer /= 1
    then rotateFaceOnCube face dir' cube'
    else cube'
  where
    dir' = if layer == 0 then dir else inv dir
    face = case layer of
        0 -> up
        2 -> down
        3 -> undefined
    cube' = rotateVertAxSides layer dir cube


rotateVertAxSides :: Int -> TurnDir -> Cube -> Cube
rotateVertAxSides layer dir cube = 
    Cube $ replaceElements (faces cube) (moddedFaces)
  where
    moddedFaces :: [(Int, Face)]
    moddedFaces = map newFace $ zip sideFaces layers
    newFace :: ((Int, Int), [Colour]) -> (Int, Face)
    newFace ((_, to), layerTaken) = (to, Face $ replaceList (layer*3, 3) (colours $ faceAt to cube) layerTaken)

    layers = map layerOf sideFaces
    layerOf :: (Int, Int) -> [Colour]
    layerOf (from, _) = takeFrom (layer*3, 3) . colours $ faceAt from cube
    
    rotFaces = [front, left, back, right]

    sideFaces = sideFaces' dir -- [(from, to)...]

    sideFaces' CW = zip rotFaces (rotateL rotFaces)
    sideFaces' CCW = zip (rotateL rotFaces) rotFaces
    sideFaces' Full = zip rotFaces (rotateL2 rotFaces)
    
rotateHorizXAx :: Int -> TurnDir -> Cube -> Cube
rotateHorizXAx layer dir = rotateCube Z CW . rotateVertAx layer dir . rotateCube Z CCW

rotateHorizZAx :: Int -> TurnDir -> Cube -> Cube
rotateHorizZAx layer dir = rotateCube X CCW . rotateVertAx layer dir . rotateCube X CW

rotate :: Axis -> Int -> TurnDir -> Cube -> Cube
rotate X = rotateHorizXAx
rotate Y = rotateVertAx
rotate Z = rotateHorizZAx

_U = rotate Y 0 CW
_U' = rotate Y 0 CCW
_U2 = rotate Y 0 Full

_D = rotate Y 2 CCW
_D' = rotate Y 2 CW
_D2 = rotate Y 2 Full

_F = rotate Z 0 CW
_F' = rotate Z 0 CCW
_F2 = rotate Z 0 Full

_B = rotate Z 2 CCW
_B' = rotate Z 2 CW
_B2 = rotate Z 2 Full

_R = rotate X 0 CW
_R' = rotate X 0 CCW
_R2 = rotate X 0 Full

_L = rotate X 2 CCW
_L' = rotate X 2 CW
_L2 = rotate X 2 Full

_E = rotate Y 1 CCW
_E' = rotate Y 1 CW
_E2 = rotate Y 1 Full

_M = rotate X 1 CCW
_M' = rotate X 1 CW
_M2 = rotate X 1 Full

_S = rotate Z 1 CW
_S' = rotate Z 1 CCW
_S2 = rotate Z 1 Full

u = _U . _E'
u' = _U' . _E
u2 = _U2 . _E2

d = _D . _E
d' = _D' . _E'
d2 = _D2 . _E2

r = _R . _M'
r' = _R' . _M
r2 = _R2 . _M2

l = _L . _M
l' = _L' . _M'
l2 = _L2 . _M2

f = _F . _S
f' = _F' . _S'
f2 = _F2 . _S2

b = _B . _S'
b' = _B' . _S
b2 = _B2 . _S2

x = rotateCube X CW
x' = rotateCube X CCW
x2 = rotateCube X Full

y = rotateCube Y CW
y' = rotateCube Y CCW
y2 = rotateCube Y Full

z = rotateCube Z CW
z' = rotateCube Z CCW
z2 = rotateCube Z Full

alg :: String -> Cube -> Cube
alg (p:q:xs) = if q == '\'' then (alg xs . inverse p) else
                    if q == '2' then (alg xs . double p) else (alg (q:xs) . normal p)
alg (p:[]) = normal p
alg [] = id

normal 'U' = _U
normal 'D' = _D
normal 'F' = _F
normal 'B' = _B
normal 'R' = _R
normal 'L' = _L
normal 'E' = _E
normal 'M' = _M
normal 'S' = _S
normal 'u' = u
normal 'd' = d
normal 'r' = r
normal 'l' = l
normal 'f' = f
normal 'b' = b
normal 'x' = x
normal 'y' = y
normal 'z' = z

inverse 'U' = _U'
inverse 'D' = _D'
inverse 'F' = _F'
inverse 'B' = _B'
inverse 'R' = _R'
inverse 'L' = _L'
inverse 'E' = _E'
inverse 'M' = _M'
inverse 'S' = _S'
inverse 'u' = u'
inverse 'd' = d'
inverse 'r' = r'
inverse 'l' = l'
inverse 'f' = f'
inverse 'b' = b'
inverse 'x' = x'
inverse 'y' = y'
inverse 'z' = z'

double 'U' = _U2
double 'D' = _D2
double 'F' = _F2
double 'B' = _B2
double 'R' = _R2
double 'L' = _L2
double 'E' = _E2
double 'M' = _M2
double 'S' = _S2
double 'u' = u2
double 'd' = d2
double 'r' = r2
double 'l' = l2
double 'f' = f2
double 'b' = b2
double 'x' = x2
double 'y' = y2
double 'z' = z2
