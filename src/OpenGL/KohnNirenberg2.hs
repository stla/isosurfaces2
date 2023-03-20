module OpenGL.KohnNirenberg2
  ( main )
  where
import           Control.Concurrent             ( threadDelay )
import           Control.Monad                  ( when, forM_ )
import qualified Data.ByteString               as B
import           Data.IORef
import           Data.Vector.Unboxed            ( Vector, (!) )
import qualified Data.Vector.Unboxed           as V
import           Graphics.Rendering.OpenGL.Capture
                                                ( capturePPM )
import           Graphics.Rendering.OpenGL.GL
                                         hiding ( Color )
import           Graphics.UI.GLUT        hiding ( Color )
import           MarchingCubes                  -- ( XYZ, Voxel, Mesh (..), makeVoxel, makeMesh )
import           Math.Algebra.Hspray
import           System.Directory               ( doesDirectoryExist )
import           System.IO.Unsafe               ( unsafePerformIO )
import           Text.Printf                    ( printf )
import           Foreign.Ptr
import           Foreign.Marshal.Array

type F = Double
type Triangles = (Int, Ptr F)

makeNormals :: Vector (XYZ F) -> (XYZ F -> XYZ F) -> Vector (XYZ F)
makeNormals vrtcs gradient =  V.map (normaliz . gradient) vrtcs
  where
    normaliz (x, y, z) = (x / nrm, y / nrm, z / nrm)
      where
        nrm = sqrt (x*x + y*y + z*z)

fromVoxel :: Voxel F -> F -> (XYZ F -> XYZ F) -> IO Triangles
fromVoxel vox isolevel gradient = do 
  mesh <- makeMesh vox isolevel
  let vertices = _vertices mesh
      faces    = _faces mesh
      normals  = makeNormals vertices gradient
      triangle face =
        ( (normals ! i, vertices ! i)
        , (normals ! j, vertices ! j)
        , (normals ! k, vertices ! k)
        )
        where
          (i, j, k) = face
      flatten = concatMap $ 
        \(
          (
            (x1,  x2,  x3)
           ,(x4,  x5,  x6)
          ),
          (
            (x7,  x8,  x9)
           ,(x10, x11, x12)
          ),
          (
            (x13, x14, x15)
           ,(x16, x17, x18)
          )
        ) -> [
               x1,  x2,  x3
              ,x4,  x5,  x6
              ,x7,  x8,  x9
              ,x10, x11, x12
              ,x13, x14, x15
              ,x16, x17, x18
              ]
  withArrayLen (flatten (map triangle faces)) (\n p -> pure (n, p))


data Context = Context
    {
      contextRot1      :: IORef GLfloat
    , contextRot2      :: IORef GLfloat
    , contextRot3      :: IORef GLfloat
    , contextZoom      :: IORef Double
    , contextTriangles :: IORef Triangles
    }

white, black, whitesmoke, discord :: Color4 GLfloat
white   = Color4 1 1 1 1
black   = Color4 0 0 0 1
whitesmoke = Color4 0.96 0.96 0.96 1
discord = Color4 0.21 0.22 0.25 1

poly :: Spray Double
poly = 3 *^ z ^+^ 9 *^ (b ^*^ z^**^2) ^+^ b^**^4 
         ^+^ ((15/7) *^ b) ^*^ (x^**^6 ^-^ 15 *^ (x^**^4 ^*^ y^**^2) ^+^ 15 *^ (x^**^2 ^*^ y^**^4) ^-^ y^**^6)
         ^+^ 0.1 *^ (b ^+^ 9 *^ z^**^2)^**^5
  where
    x = lone 1 :: Spray Double
    y = lone 2 :: Spray Double
    z = lone 3 :: Spray Double
    b = x^**^2 ^+^ y^**^2


polygradient :: (Spray Double, Spray Double, Spray Double)
polygradient = (
    derivSpray 1 poly
  , derivSpray 2 poly
  , derivSpray 3 poly
  )

fun :: XYZ F -> F
fun (x, y, z) = evalSpray poly [x, y, z]

fungradient :: XYZ F -> XYZ F
fungradient (x, y, z) =
  (evalSpray poly1 [x, y, z], evalSpray poly2 [x, y, z], evalSpray poly3 [x, y, z])
   where
    (poly1, poly2, poly3) = polygradient

voxel :: Voxel F
voxel = makeVoxel fun 
  ((-3.1, 3.1),(-3.5, 3.5),(-0.6, 0.5)) (100, 100, 50)

trianglesIO :: IO Triangles
trianglesIO = fromVoxel voxel 0 fungradient

display :: Context -> DisplayCallback
display context = do
  clear [ColorBuffer, DepthBuffer]
  r1 <- get (contextRot1 context)
  r2 <- get (contextRot2 context)
  r3 <- get (contextRot3 context)
  (n, triangles) <- get (contextTriangles context)
  zoom <- get (contextZoom context)
  (_, size) <- get viewport
  loadIdentity
  resize zoom size
  rotate r1 $ Vector3 1 0 0
  rotate r2 $ Vector3 0 1 0
  rotate r3 $ Vector3 0 0 1
  materialDiffuse Front $= whitesmoke
  unsafeRenderPrimitive Triangles $ 
    forM_ [0..n `quot` 18] $ \i -> drawTriangle triangles (i * 18 * 8)
  swapBuffers
  where
  drawTriangle p i = do
    normalv (plusPtr p (i + 0 * 8) :: Ptr (Normal3 F))
    vertexv (plusPtr p (i + 3 * 8) :: Ptr (Vertex3 F))
    normalv (plusPtr p (i + 6 * 8) :: Ptr (Normal3 F))
    vertexv (plusPtr p (i + 9 * 8) :: Ptr (Vertex3 F))
    normalv (plusPtr p (i + 12 * 8) :: Ptr (Normal3 F))
    vertexv (plusPtr p (i + 15 * 8) :: Ptr (Vertex3 F))

resize :: Double -> Size -> IO ()
resize zoom s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45.0 (w'/h') 1.0 100.0
  lookAt (Vertex3 0 (-9 + zoom) 0) (Vertex3 0 0 0) (Vector3 0 0 1)
  matrixMode $= Modelview 0
  where
    w' = realToFrac w
    h' = realToFrac h

keyboard
  :: IORef GLfloat
  -> IORef GLfloat
  -> IORef GLfloat -- rotations
  -> IORef Double  -- zoom
  -> IORef Bool    -- animation
  -> IORef Int     -- animation delay
  -> IORef Bool    -- save animation
  -> KeyboardCallback
keyboard rot1 rot2 rot3 zoom anim delay save c _ = do
  case c of
    'e' -> rot1 $~! subtract 2
    'r' -> rot1 $~! (+ 2)
    't' -> rot2 $~! subtract 2
    'y' -> rot2 $~! (+ 2)
    'u' -> rot3 $~! subtract 2
    'i' -> rot3 $~! (+ 2)
    'm' -> zoom $~! (+ 0.25)
    'l' -> zoom $~! subtract 0.25
    'a' -> anim $~! not
    'o' -> delay $~! (+ 10000)
    'p' -> delay $~! (\d -> if d == 0 then 0 else d - 10000)
    's' -> save $~! not
    'q' -> leaveMainLoop
    _   -> return ()
  postRedisplay Nothing

ppmExists :: Bool
{-# NOINLINE ppmExists #-}
ppmExists = unsafePerformIO $ doesDirectoryExist "./ppm"

idle
  :: IORef Bool
  -> IORef Int
  -> IORef Bool
  -> IORef Int
  -> IORef GLfloat
  -> IdleCallback
idle anim delay save snapshots rot3 = do
  a        <- get anim
  snapshot <- get snapshots
  s        <- get save
  when a $ do
    d <- get delay
    when (s && ppmExists && snapshot < 180) $ do
      let ppm = printf "ppm/pic%04d.ppm" snapshot
      (>>=) capturePPM (B.writeFile ppm)
      print snapshot
      snapshots $~! (+ 1)
    rot3 $~! (+ 2)
    _ <- threadDelay d
    postRedisplay Nothing


main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Kohn-Nirenberg surface"
  windowSize $= Size 512 512
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  clearColor $= discord
  materialAmbient Front $= white
  lighting $= Enabled
  lightModelTwoSide $= Enabled
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 0 (-100) 0 1
  ambient (Light 0) $= black
  diffuse (Light 0) $= white
  specular (Light 0) $= white
  depthFunc $= Just Less
  shadeModel $= Smooth
  cullFace $= Just Back
  rot1 <- newIORef 0.0
  rot2 <- newIORef 0.0
  rot3 <- newIORef 0.0
  zoom <- newIORef 0.0
  triangles <- newIORef =<< trianglesIO 
  displayCallback $= display Context {contextRot1 = rot1,
                                      contextRot2 = rot2,
                                      contextRot3 = rot3,
                                      contextZoom = zoom,
                                      contextTriangles = triangles}
  reshapeCallback $= Just (resize 0)
  anim      <- newIORef False
  delay     <- newIORef 0
  save      <- newIORef False
  snapshots <- newIORef 0
  keyboardCallback $= Just (keyboard rot1 rot2 rot3 zoom anim delay save)
  idleCallback $= Just (idle anim delay save snapshots rot3)
  putStrLn "*** Kohn-Nirenberg surface ***\n\
        \    To quit, press q.\n\
        \    Scene rotation:\n\
        \        e, r, t, y, u, i\n\
        \    Zoom: l, m\n\
        \    Animation: a\n\
        \    Animation speed: o, p\n\
        \    Save animation: s\n\
        \"
  mainLoop

