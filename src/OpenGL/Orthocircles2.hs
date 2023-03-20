module OpenGL.Orthocircles2
  ( main )
  where
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
type Triangles = [((XYZ F, XYZ F, XYZ F), (XYZ F, XYZ F, XYZ F))]

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
        ( (vertices ! i, vertices ! j, vertices ! k)
        , (normals ! i , normals ! j , normals ! k )
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
    }

white, black, whitesmoke, discord :: Color4 GLfloat
white   = Color4 1 1 1 1
black   = Color4 0 0 0 1
whitesmoke = Color4 0.96 0.96 0.96 1
discord = Color4 0.21 0.22 0.25 1

fun :: F -> F -> XYZ F -> F
fun a b (x, y, z) =
  (xy2 * xy2 + z2) * (yz2 * yz2 + x2) 
    * (zx2 * zx2 + y2) - a * a * (1 + b * (x2 + y2 + z2))
  where
    x2 = x * x
    y2 = y * y
    z2 = z * z
    xy2 = x2 + y2 - 1
    yz2 = y2 + z2 - 1
    zx2 = z2 + x2 - 1

fungradient :: F -> F -> XYZ F -> XYZ F
fungradient a b (x, y, z) =
  (
    -2 * a2b * x + 2 * x * xxz * zzy 
      + 4 * x * zx2 * xxz * yyx 
      + 4 * x * xy2 * zzy * yyx,
    -2 * a2b * y + 2 * y * yyx * xxz 
      + 4 * y * yz2 * xxz * zzy 
      + 4 * y * xy2 * zzy * yyx,
    -2 * a2b * z + 2 * z * zzy * yyx 
      + 4 * z * yz2 * xxz * zzy 
      + 4 * z * zx2 * xxz * yyx
  )
  where
    a2b = a * a * b
    x2 = x * x
    y2 = y * y
    z2 = z * z
    xy2 = x2 + y2 - 1
    yz2 = y2 + z2 - 1
    zx2 = z2 + x2 - 1
    xxz = xy2 * xy2 + z2
    yyx = yz2 * yz2 + x2
    zzy = zx2 * zx2 + y2

aa, bb :: F
aa = 0.075
bb = -0.5

voxel :: Voxel F
voxel = makeVoxel (fun aa bb) 
  ((-1.3, 1.3),(-1.3, 1.3),(-1.3, 1.3)) (150, 150, 150)

trianglesIO :: IO Triangles
trianglesIO = fromVoxel voxel 0 (fungradient aa bb)

triangles :: Triangles
{-# NOINLINE triangles #-}
triangles = unsafePerformIO trianglesIO

display :: Context -> DisplayCallback
display context = do
  clear [ColorBuffer, DepthBuffer]
  r1 <- get (contextRot1 context)
  r2 <- get (contextRot2 context)
  r3 <- get (contextRot3 context)
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
  drawTriangle ((v1, v2, v3), (n1, n2, n3)) = do
    materialDiffuse Front $= whitesmoke
    normal (toNormal n1)
    vertex (toVertex v1)
    normal (toNormal n2)
    vertex (toVertex v2)
    normal (toNormal n3)
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
  lookAt (Vertex3 0 (-4.3 + zoom) 0) (Vertex3 0 0 0) (Vector3 0 0 1)
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
  _ <- createWindow "Orthocircles"
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
  displayCallback $= display Context {contextRot1 = rot1,
                                      contextRot2 = rot2,
                                      contextRot3 = rot3,
                                      contextZoom = zoom}
  reshapeCallback $= Just (resize 0)
  anim      <- newIORef False
  delay     <- newIORef 0
  save      <- newIORef False
  snapshots <- newIORef 0
  keyboardCallback $= Just (keyboard rot1 rot2 rot3 zoom anim delay save)
  idleCallback $= Just (idle anim delay save snapshots rot3)
  putStrLn "*** Orthocircles ***\n\
        \    To quit, press q.\n\
        \    Scene rotation:\n\
        \        e, r, t, y, u, i\n\
        \    Zoom: l, m\n\
        \    Animation: a\n\
        \    Animation speed: o, p\n\
        \    Save animation: s\n\
        \"
  mainLoop

