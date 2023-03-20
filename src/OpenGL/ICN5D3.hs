module OpenGL.ICN5D3
  ( main )
  where
import           Control.Monad                  ( when, forM_, unless )
import qualified Data.ByteString               as B
import           Data.IORef
import           Data.Vector.Unboxed            ( (!) )
import           Graphics.Rendering.OpenGL.Capture
                                                ( capturePPM )
import           Graphics.Rendering.OpenGL.GL
                                         hiding ( Color )
import           MarchingCubes                  -- ( XYZ, Voxel, Mesh (..), makeVoxel, makeMesh )
import           System.Directory               ( doesDirectoryExist )
import           System.IO.Unsafe               ( unsafePerformIO )
import           Text.Printf                    ( printf )
import qualified Data.Vector.Storable          as VS
import           Foreign.Ptr                    ( Ptr, plusPtr )
import           Graphics.UI.GLFW              as GLFW
import           Graphics.Rendering.OpenGL.GLU.Matrix
import           Data.Maybe

type F = Double
type Triangles = VS.Vector F

fromVoxel :: Voxel F -> F -> (XYZ F -> XYZ F) -> IO Triangles
fromVoxel vox isolevel gradient = do 
  mesh <- makeMesh vox isolevel
  let vertices = _vertices mesh
      faces    = _faces mesh
      flat (x,y,z) = [x,y,z]
      normaliz (x, y, z) = (x / nrm, y / nrm, z / nrm)
        where
          nrm = sqrt (x*x + y*y + z*z)
      f i = flat (normaliz (gradient (vertices ! i))) ++ flat (vertices ! i)
  pure (VS.fromList (concat [f i ++ f j ++ f k | (i,j,k) <- faces]))


data Context = Context
    {
      contextRot1      :: IORef GLfloat
    , contextRot2      :: IORef GLfloat
    , contextRot3      :: IORef GLfloat
    , contextZoom      :: IORef Double
    , contextPhase     :: IORef F
    }

white, black, fuchsia, discord :: Color4 GLfloat
white   = Color4 1 1 1 1
black   = Color4 0 0 0 1
fuchsia = Color4 1 0 1 1
discord = Color4 0.21 0.22 0.25 1

fun :: F -> XYZ F -> F
fun a (x, y, z) = 
  sq(sqrt(
    sq(sqrt(sq(sqrt(sq(x*sina) + sq(z*cosa)) - 5) + sq(y*sina)) - 2.5) + 
      sq(x*cosa)) - 1.25
  ) + sq(sqrt(sq(sqrt(sq(z*sina) + sq(y*cosa)) - 2.5)) - 1.25)
  where
    sq u = u * u
    sina = sin a
    cosa = cos a

fungradient :: F -> XYZ F -> XYZ F
fungradient a (x, y, z) =
  (
    (f (x + eps, y, z) - fxyz) / eps
  , (f (x, y + eps, z) - fxyz) / eps
  , (f (x, y, z + eps) - fxyz) / eps
  )
    where
      f = fun a
      fxyz = f (x, y, z)
      eps = 0.000001

voxel :: F -> Voxel F
voxel a = makeVoxel (fun a) 
  ((-10, 10), (-10, 10), (-10, 10)) (200, 200, 200)

trianglesIO :: F -> IO Triangles
trianglesIO a = fromVoxel (voxel a) 0.25 (fungradient a)

resize :: Double -> Size -> IO ()
resize zoom s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45.0 (w'/h') 1.0 100.0
  lookAt (Vertex3 0 0 (-28 + zoom)) (Vertex3 0 0 0) (Vector3 0 1 0)
  matrixMode $= Modelview 0
  where
    w' = realToFrac w
    h' = realToFrac h

keyboard
  :: IORef GLfloat
  -> IORef GLfloat
  -> IORef GLfloat -- rotations
  -> IORef Double  -- zoom
  -> IORef F       -- phase
  -> IORef Bool    -- animation
  -> IORef Bool    -- save animation
  -> CharCallback
keyboard rot1 rot2 rot3 zoom phase anim save window c = do
  case c of
    'e' -> rot1 $~! subtract 2
    'r' -> rot1 $~! (+ 2)
    't' -> rot2 $~! subtract 2
    'y' -> rot2 $~! (+ 2)
    'u' -> rot3 $~! subtract 2
    'i' -> rot3 $~! (+ 2)
    'm' -> zoom $~! (+ 0.25)
    'l' -> zoom $~! subtract 0.25
    'k' -> phase $~! (+ 0.03)
    'j' -> phase $~! subtract 0.03
    'a' -> anim $~! not
    's' -> save $~! not
    'q' -> setWindowShouldClose window True
    _   -> return ()

ppmExists :: Bool
{-# NOINLINE ppmExists #-}
ppmExists = unsafePerformIO $ doesDirectoryExist "./ppm"


mainLoop 
  :: Context 
  -> GLFW.Window 
  -> IORef Bool 
  -> IORef Bool 
  -> IORef Int
  -> IORef F
  -> IO ()
mainLoop context window anim save snapshots phase = do

  r1 <- get (contextRot1 context)
  r2 <- get (contextRot2 context)
  r3 <- get (contextRot3 context)
  ph <- get (contextPhase context)
  triangles <- trianglesIO ph

  -- idle callback
  a        <- get anim
  snapshot <- get snapshots
  s        <- get save
  when a $ do
    when (s && ppmExists && snapshot < 120) $ do
      let ppm = printf "ppm/pic%04d.ppm" snapshot
      (>>=) capturePPM (B.writeFile ppm)
      print snapshot
      snapshots $~! (+ 1)
    phase $~! (+ (pi / 120))
  ----

  clear [ColorBuffer, DepthBuffer]
  zoom <- get (contextZoom context)
  (_, size) <- get viewport
  loadIdentity
  resize zoom size
  rotate r1 $ Vector3 1 0 0
  rotate r2 $ Vector3 0 1 0
  rotate r3 $ Vector3 0 0 1
  materialDiffuse Front $= fuchsia
  VS.unsafeWith triangles $ 
    \ptr -> unsafeRenderPrimitive Triangles $
      forM_ [0 .. VS.length triangles `quot` 18] $ 
        \i -> drawTriangle ptr (18 * 8 * i)
  swapBuffers window
  GLFW.pollEvents
  b <- GLFW.windowShouldClose window
  unless b (mainLoop context window anim save snapshots phase)
  where
    drawTriangle p i = do
      normalv (plusPtr p (i + 0 * 8) :: Ptr (Normal3 F))
      vertexv (plusPtr p (i + 3 * 8) :: Ptr (Vertex3 F))
      normalv (plusPtr p (i + 6 * 8) :: Ptr (Normal3 F))
      vertexv (plusPtr p (i + 9 * 8) :: Ptr (Vertex3 F))
      normalv (plusPtr p (i + 12 * 8) :: Ptr (Normal3 F))
      vertexv (plusPtr p (i + 15 * 8) :: Ptr (Vertex3 F))


main :: IO ()
main = do
  _ <- GLFW.init
  window <- fromJust <$> 
    GLFW.createWindow 512 512 "Toratope" Nothing Nothing
  GLFW.makeContextCurrent (Just window)
  GLFW.swapInterval 1
  clearColor $= discord
  materialAmbient Front $= black
  lighting $= Enabled
  lightModelTwoSide $= Enabled
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 100 (100) (-100) 1
  ambient (Light 0) $= black
  diffuse (Light 0) $= white
  specular (Light 0) $= white
  light (Light 1) $= Enabled
  position (Light 1) $= Vertex4 (-10) (-100) (-100) 1
  ambient (Light 1) $= black
  diffuse (Light 1) $= black
  specular (Light 1) $= black
  depthFunc $= Just Less
  shadeModel $= Smooth
  cullFace $= Just Back
  rot1 <- newIORef 0.0
  rot2 <- newIORef 0.0
  rot3 <- newIORef 0.0
  zoom <- newIORef 0.0
  phase <- newIORef 0.0
  let context = Context { contextRot1 = rot1,
                          contextRot2 = rot2,
                          contextRot3 = rot3,
                          contextZoom = zoom,
                          contextPhase = phase }
  anim      <- newIORef False
  save      <- newIORef False
  snapshots <- newIORef 0
  setCharCallback 
    window (Just (keyboard rot1 rot2 rot3 zoom phase anim save))
  putStrLn "*** Toratope ***\n\
        \    To quit, press q.\n\
        \    Scene rotation:\n\
        \        e, r, t, y, u, i\n\
        \    Zoom: l, m\n\
        \    Animation: a\n\
        \    Save animation: s\n\
        \"
  mainLoop context window anim save snapshots phase
