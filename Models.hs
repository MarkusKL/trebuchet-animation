module Models
( renderBeamPair
, renderAxle
, renderArm
, renderProjectile2
) where

import Graphics.UI.GLUT
import Window

renderBeamPair :: IO ()
renderBeamPair = preservingMatrix $ do
    t3 (-400) (-440) (-100)
    renderBeam
    s3 (-1) 1 1
    t3 (-800) 0 0
    renderBeam

renderBeam :: IO ()
renderBeam = preservingMatrix $ do
    preservingMatrix $ do
        color $ c3 (150/255) (111/255) (51/255)
        renderPrimitive Quads $ do
            mapM_ (v3 ~+~) beamSide
        t3 0 0 beamWidth
        renderPrimitive Quads $ do
            mapM_ (v3 ~+~) beamSide

    preservingMatrix $ do
        renderPrimitive Quads $ do
            mapM_ (v3 ~+~) beamFront
        t3 200 0 0
        renderPrimitive Quads $ do
            mapM_ (v3 ~+~) beamFront

    preservingMatrix $ do
        renderPrimitive Quads $ do
            mapM_ (v3 ~+~) beamBottom
        renderPrimitive Quads $ do
            mapM_ (v3 ~+~) beamTop

    preservingMatrix $ do
        color $ c3 (70/255) (31/255) (0)
        renderPrimitive LineStrip $ do
            mapM_ (v3 ~+~) [
                beamPoints (0,0,0),
                beamPoints (1,0,0),
                beamPoints (1,1,0),
                beamPoints (0,1,0),
                beamPoints (0,0,0),
                beamPoints (0,0,1),
                beamPoints (1,0,1),
                beamPoints (1,0,0),
                beamPoints (1,0,1),
                beamPoints (1,1,1),
                beamPoints (1,1,0),
                beamPoints (1,1,1),
                beamPoints (0,1,1),
                beamPoints (0,1,0),
                beamPoints (0,1,1),
                beamPoints (0,0,1)
                ]

beamFront :: [(GLfloat,GLfloat,GLfloat)]
beamFront = [
    beamPoints (0,0,0),
    beamPoints (0,1,0),
    beamPoints (0,1,1),
    beamPoints (0,0,1)]

beamSide :: [(GLfloat,GLfloat,GLfloat)]
beamSide = [
    beamPoints (0,0,0),
    beamPoints (1,0,0),
    beamPoints (1,1,0),
    beamPoints (0,1,0)]

beamBottom :: [(GLfloat,GLfloat,GLfloat)]
beamBottom = [
    beamPoints (0,0,0),
    beamPoints (1,0,0),
    beamPoints (1,0,1),
    beamPoints (0,0,1)
    ]

beamTop :: [(GLfloat,GLfloat,GLfloat)]
beamTop = [
    beamPoints (0,1,0),
    beamPoints (1,1,0),
    beamPoints (1,1,1),
    beamPoints (0,1,1)
    ]

beamPoints :: (Int,Int,Int) -> (GLfloat, GLfloat, GLfloat)
beamPoints (x,y,z) = [[
    [(0,0,0),(0,0,beamWidth)],
    [(300,beamHeight,0),(300,beamHeight,beamWidth)]],
    [[(200,0,0),(200,0,beamWidth)],
    [(beamHeight,beamHeight,0),(beamHeight,beamHeight,beamWidth)]]
    ] !! x !! y !! z

beamWidth :: GLfloat
beamWidth = 200

beamWidth2 :: GLfloat
beamWidth2 = 200

beamHeight :: GLfloat
beamHeight = 500

-------------------------------

renderAxle :: IO ()
renderAxle = preservingMatrix $ do
    color $ c3 (100/255) (61/255) (30/255)
    renderCylinder
    s3 1 1 (-1)
    renderCylinder

renderCylinder :: IO ()
renderCylinder = renderQuadric cylinderStyle (Cylinder 40 40 cylinderLength 16 16)

cylinderStyle :: QuadricStyle
cylinderStyle = QuadricStyle Nothing NoTextureCoordinates Inside FillStyle

cylinderLength :: GLdouble
cylinderLength = 450

-------------------------------

renderArm :: Int -> IO ()
renderArm t = preservingMatrix $ do

    preservingMatrix $ do
        t3 0 200 0
        renderWeight

    color $ c3 (130/255) (91/255) (60/255)
    renderPrimitive Quads $ do
        mapM_ (v3 ~+~) armSide

    s3 1 1 (-1)
    renderPrimitive Quads $ do
        mapM_ (v3 ~+~) armSide

    rotate 90 $ Vector3 0 1 (0 :: GLfloat)
    color $ c3 (130/255) (91/255) (60/255)
    renderPrimitive Quads $ do
        mapM_ (v3 ~+~) armSide

    s3 1 1 (-1)
    renderPrimitive Quads $ do
        mapM_ (v3 ~+~) armSide

    t3 0 (-600) 0
    renderSling t



armSide :: [(GLfloat, GLfloat, GLfloat)]
armSide = [
    armPoints (0,0,0),
    armPoints (0,1,0),
    armPoints (1,1,0),
    armPoints (1,0,0)
    ]

armTop :: [(GLfloat, GLfloat, GLfloat)]
armTop = [
    armPoints (0,0,0),
    armPoints (1,0,0),
    armPoints (1,0,1),
    armPoints (0,0,1)
    ]

armPoints :: (Int, Int, Int) -> (GLfloat, GLfloat, GLfloat)
armPoints (x,y,z) = [(50,200,50),(-50,200,50),(5,-600,5),(-5,-600,5),(5,200,-5),(-5,200,-5),(5,-600,-5),(-5,-600,-5)] !! (x + y*2 + z*4)


----------------------------------

renderWeight :: IO ()
renderWeight = preservingMatrix $ do
    color $ c3 0.5 0.5 0.5
    weightSide
    preservingMatrix $ do
        s3 1 1 (-1)
        weightSide

    preservingMatrix $ do
        rotate 90 $ Vector3 1 0 (0 :: GLfloat)
        weightSide
        s3 1 1 (-1)
        weightSide

    preservingMatrix $ do
        rotate 90 $ Vector3 0 1 (0 :: GLfloat)
        weightSide
        s3 1 1 (-1)
        weightSide

weightSide :: IO ()
weightSide = preservingMatrix $ do
    t3 (-weightWidth/2) (-weightWidth/2) (-weightWidth/2)
    renderPrimitive Quads $ do
        v3 0 0 0
        v3 weightWidth 0 0
        v3 weightWidth weightWidth 0
        v3 0 weightWidth 0

weightWidth :: GLfloat
weightWidth = 200

weightPoints :: (Int, Int, Int) -> (GLfloat, GLfloat, GLfloat)
weightPoints (x,y,z) = [(0,0,0),(300,0,0),(0,300,0),(300,300,0),(0,0,300),(300,0,300),(0,300,300),(300,300,300)] !! (x + y*2 + z*4)

---------------------------------------

renderSling :: Int -> IO ()
renderSling i = preservingMatrix $ do
    let traw = fromIntegral i
    let t = if traw > 1800 then 1800 else traw
    color $ c3 0.7 0.7 0.7
    s3 1 (-1) 1
    rotate (t/5-180) $ Vector3 1 0 (0 :: GLfloat)
    renderPrimitive LineStrip $ do
        v3 0 0 0
        v3 0 (400) 0

    t3 0 400 0
    renderProjectile1 t

----------------------------------------

calcPos :: GLfloat -> (GLfloat, GLfloat, GLfloat)
calcPos t = if t <= 950
    then (
        cos ((-t/5-180+(90+53000/t*sin (t/400)))*pi/180)*400 + cos ((90+53000/t*sin (t/400))*pi/180)*600,
        sin ((-t/5-180+(90+53000/t*sin (t/400)))*pi/180)*400 + sin ((90+53000/t*sin (t/400))*pi/180)*600,
        0)
    else let (x0,y0,z0) = calcPos 950 in
        let (vx,vy,vz) = (x0,y0,z0) +++ (-1) *- calcPos 949 in
            let x = t-950 in
                (x0+vx*x,y0+vy*x-0.0015*x^2,z0+vz*x)

renderProjectile1 :: GLfloat -> IO ()
renderProjectile1 t = preservingMatrix $ do
    color $ c3 0.5 0.5 0.5
    if t < 950 then
        renderQuadric projectileStyle (Sphere 50 16 16)
    else return ()

renderProjectile2 :: GLfloat -> IO ()
renderProjectile2 t = preservingMatrix $ do
    color $ c3 0.5 0.5 0.5
    t3 ~+~ calcPos t
    renderQuadric projectileStyle (Sphere 50 16 16)

projectileStyle :: QuadricStyle
projectileStyle = QuadricStyle Nothing NoTextureCoordinates Inside FillStyle


infixr 5 ~+~ -- Apply triple
(~+~) :: (a -> a -> a -> b) -> (a,a,a) -> b
f ~+~ (a,b,c) = f a b c

infixr 7 *- -- Scalar multiplication
(*-) :: (Num a) => a -> (a,a,a) -> (a,a,a)
a *- (a1,a2,a3) = (a*a1,a*a2,a*a3)

infixr 6 +++ -- Add triple
(+++) :: (Num a) => (a,a,a) -> (a,a,a) -> (a,a,a)
(a1,a2,a3) +++ (b1,b2,b3) = (a1+b1,a2+b2,a3+b3)
