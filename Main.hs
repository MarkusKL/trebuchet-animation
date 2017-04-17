import Window
import Models
--import Util
import Graphics.UI.GLUT

data Perspective = Norm | Far | Zoom
data World = World {time :: Int, start :: Int, absTime :: Int, p :: Perspective, r :: GLfloat, r2 :: GLfloat}

main :: IO ()
main = do
    initWindow newWorld drawWorld updateWorld handleKeys
    mainLoop

updateWorld :: Int -> World -> World
updateWorld n (World _ start _ p r r2) = World (n - start) start n p r r2

newWorld :: World
newWorld = World 0 0 0 Zoom 0 0

drawWorld :: World -> IO ()
drawWorld w = do
    rotate (r w) $ Vector3 0 1 (0 :: GLfloat)
    rotate (r2 w) $ Vector3 1 0 (0 :: GLfloat)
    --rotate 45 $ Vector3 1 0 (0 :: GLfloat)

    case p w of
        Norm -> do
            s3 0.5 0.5 0.5
            t3 0 0 0
        Zoom -> return ()
        Far -> do
            s3 0.2 0.2 0.2
            t3 (-2000) 0 0

    color $ c3 0 0.5 0
    renderPrimitive Quads $ do
        v3 1000 (-465) 1000
        v3 (-1000) (-465) 1000
        v3 (-1000) (-465) (-1000)
        v3 1000 (-465) (-1000)


    preservingMatrix $ do
        renderProjectile2 ((fromIntegral . time) w)
        let t = fromIntegral (time w) in rotate (180+53000/t*sin (t/400)) $ Vector3 0 0 (1 :: GLfloat)
        renderAxle
        renderArm (time w)


    t3 0 0 (-300)
    renderBeamPair
    t3 0 0 (600)
    renderBeamPair

handleKeys :: Key -> KeyState -> Modifiers -> Position -> World -> World
handleKeys (Char 'n') Down _ _ (World time start absTime p r r2) = World 0 absTime absTime p r r2
handleKeys (Char 'a') Down _ _ w = w {r = r w + 10}
handleKeys (Char 'd') Down _ _ w = w {r = r w - 10}
handleKeys (Char 'w') Down _ _ w = w {r2 = r2 w + 10}
handleKeys (Char 's') Down _ _ w = w {r2 = r2 w - 10}
handleKeys (Char 'z') Down _ _ w = w {p = Zoom}
handleKeys (Char 'x') Down _ _ w = w {p = Norm}
handleKeys (Char 'c') Down _ _ w = w {p = Far}
handleKeys _ _ _ _ w = w
