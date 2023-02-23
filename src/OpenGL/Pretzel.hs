module OpenGL.Pretzel
  ( main )
  where
import           Colors.ColorRamp
import           Control.Concurrent             ( threadDelay )
import           Control.Monad                  ( when )
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
import           System.Directory               ( doesDirectoryExist )
import           System.IO.Unsafe               ( unsafePerformIO )
import           Text.Printf                    ( printf )


type F = Double
type Color = Color4 GLfloat
type Triangles = 
  [((XYZ F, XYZ F, XYZ F), (XYZ F, XYZ F, XYZ F), (Color, Color, Color))]

makeNormals :: Vector (XYZ F) -> (XYZ F -> XYZ F) -> Vector (XYZ F)
makeNormals vrtcs gradient =  V.map (normaliz . gradient) vrtcs
  where
    normaliz (x, y, z) = (x / nrm, y / nrm, z / nrm)
      where
        nrm = sqrt (x*x + y*y + z*z)

funColor :: F -> F -> F -> Color
funColor dmin dmax d = clrs !! j
 where
  clrs = colorRamp "klingon" 256
  j    = floor ((d - dmin) * 255 / (dmax - dmin))

fromVoxel :: Voxel F -> F -> (XYZ F -> XYZ F) -> IO Triangles
fromVoxel vox isolevel gradient = do 
  mesh <- makeMesh vox isolevel
  let vertices = _vertices mesh
      faces    = _faces mesh
      normals  = makeNormals vertices gradient
      ds   = V.map (\(x, y, z) -> sqrt (x * x + y * y + z * z)) vertices
      dmin = V.minimum ds
      dmax = V.maximum ds
      colors = V.map (funColor dmin dmax) ds
      triangle face =
        ( (vertices ! i, vertices ! j, vertices ! k)
        , (normals ! i , normals ! j , normals ! k )
        , (colors ! i  , colors ! j  , colors ! k  )
        )
        where
          (i, j, k) = face
  return $ map triangle faces


data Context = Context
    {
      contextRot1      :: IORef GLfloat
    , contextRot2      :: IORef GLfloat
    , contextRot3      :: IORef GLfloat
    , contextZoom      :: IORef Double
    , contextTriangles :: IORef Triangles
    }

white, black, discord :: Color4 GLfloat
white   = Color4 1 1 1 1
black   = Color4 0 0 0 1
discord = Color4 0.21 0.22 0.25 1

fun :: F -> XYZ F -> F
fun c (x, y, z) =
  sqr ((sqr (x - c) + y2 - 1) * (sqr (x + c) + y2 - 1) 
    / (1 + (1 + c)*(x2 + y2))) + (1 + c) * z2
  where
    sqr u = u * u
    x2 = x * x
    y2 = y * y
    z2 = z * z

fungradient :: F -> XYZ F -> XYZ F
fungradient c (x, y, z) =
  (
    (f (x + eps, y, z) - fxyz) / eps
  , (f (x, y + eps, z) - fxyz) / eps
  , (f (x, y, z + eps) - fxyz) / eps
  )
    where
      f = fun c
      fxyz = f (x, y, z)
      eps = 0.000001

voxel :: Voxel F
voxel = makeVoxel (fun (1.3)) 
  ((-2.8, 2.8),(-1.6, 1.6),(-0.7, 0.7)) (150, 150, 150)

trianglesIO :: IO Triangles
trianglesIO = fromVoxel voxel 1 (fungradient (1.3))

display :: Context -> DisplayCallback
display context = do
  clear [ColorBuffer, DepthBuffer]
  r1 <- get (contextRot1 context)
  r2 <- get (contextRot2 context)
  r3 <- get (contextRot3 context)
  triangles <- get (contextTriangles context)
  zoom <- get (contextZoom context)
  (_, size) <- get viewport
  loadIdentity
  resize zoom size
  rotate r1 $ Vector3 1 0 0
  rotate r2 $ Vector3 0 1 0
  rotate r3 $ Vector3 0 0 1
  renderPrimitive Triangles $ mapM_ drawTriangle triangles
  swapBuffers
  where
  drawTriangle ((v1, v2, v3), (n1, n2, n3), (c1, c2, c3)) = do
    normal (toNormal n1)
    materialDiffuse Front $= c1
    vertex (toVertex v1)
    normal (toNormal n2)
    materialDiffuse Front $= c2
    vertex (toVertex v2)
    normal (toNormal n3)
    materialDiffuse Front $= c3
    vertex (toVertex v3)
    where
      toNormal (x, y, z) = Normal3 x y z
      toVertex (x, y, z) = Vertex3 x y z

resize :: Double -> Size -> IO ()
resize zoom s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45.0 (w'/h') 1.0 100.0
  lookAt (Vertex3 0 (-7.5 + zoom) 0) (Vertex3 0 0 0) (Vector3 0 0 1)
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
  _ <- createWindow "Pretzel"
  windowSize $= Size 512 512
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  clearColor $= discord
  materialAmbient Front $= black
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
  putStrLn "*** Pretzel ***\n\
        \    To quit, press q.\n\
        \    Scene rotation:\n\
        \        e, r, t, y, u, i\n\
        \    Zoom: l, m\n\
        \    Animation: a\n\
        \    Animation speed: o, p\n\
        \    Save animation: s\n\
        \"
  mainLoop

