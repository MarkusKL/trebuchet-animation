module Window
( initWindow
, mainLoop
, v3
, t3
, s3
, c3
, c4
) where

import Graphics.UI.GLUT
import Data.IORef

initWindow :: a
           -> (a -> IO ())
           -> (Int -> a -> a)
           -> (Key -> KeyState -> Modifiers -> Position -> a -> a)
           -> IO ()
initWindow start draw update keyCB = do
    getArgsAndInitialize
    initialWindowSize $= Size 800 800
    initialDisplayMode $= [WithDepthBuffer,DoubleBuffered,WithAlphaComponent,WithAccumBuffer]
    createWindow "Trebuchet"
    depthFunc $= Just Less
    lineSmooth $= Enabled
    blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    lineWidth $= 1.5
    pointSize $= 1

    worldRef <- newIORef start

    keyboardMouseCallback $= Just (keyboardMouse keyCB worldRef)
    displayCallback $= (display draw update worldRef)
    idleCallback $= Just (idle)
    --reshapeCallback $= Just (reshape)
    return ()

display :: (a -> IO ()) -> (Int -> a -> a) -> IORef a -> DisplayCallback
display drawFunc updateFunc worldRef = do

    clear [ColorBuffer,DepthBuffer]
    --accum Return 0.95
    --clear [AccumBuffer]

    loadIdentity
    perspective 90 1 0.01 2.01
    t3 0 0 (-1.01)
    s3 0.001 0.001 0.001

    time <- get elapsedTime
    worldRef $~! (updateFunc time)
    world <- get worldRef
    drawFunc world

    swapBuffers
    --accum Accum 0.9

keyboardMouse :: (Key -> KeyState -> Modifiers -> Position -> a -> a)
              -> IORef a
              -> KeyboardMouseCallback
keyboardMouse _ _ (Char 'f') Down _ _ = fullScreenToggle
keyboardMouse _ _ (Char '\ESC') Down _ _ = exit
keyboardMouse f worldRef key keySt mod pos = worldRef $~! f key keySt mod pos


reshape :: Size -> IO ()
reshape (Size x y) = return ()

v3 :: GLfloat -> GLfloat -> GLfloat -> IO ()
v3 x y z = vertex $ Vertex3 x y z

t3 :: GLfloat -> GLfloat -> GLfloat -> IO ()
t3 x y z = translate $ Vector3 x y z

s3 :: GLfloat -> GLfloat -> GLfloat -> IO ()
s3 x y z = scale x y z

c3 :: GLfloat -> GLfloat -> GLfloat -> Color4 GLfloat
c3 r g b = Color4 r g b 1

c4 :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> Color4 GLfloat
c4 = Color4

idle :: IdleCallback
idle = postRedisplay Nothing
