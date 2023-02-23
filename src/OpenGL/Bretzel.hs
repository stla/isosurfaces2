module OpenGL.Bretzel
  ( main )
  where
import           Data.IORef
import           Data.Vector.Unboxed            ( (!)
                                                , Vector
                                                )
import           Graphics.Rendering.OpenGL.GL
                                         hiding ( Color )
import           Graphics.UI.GLUT        hiding ( Color )
import           MarchingCubes

type F = Double
type Triangles = [((XYZ F, XYZ F, XYZ F), (XYZ F, XYZ F, XYZ F))]

data Context = Context
    {
      contextRot1      :: IORef GLfloat
    , contextRot2      :: IORef GLfloat
    , contextRot3      :: IORef GLfloat
    , contextZoom      :: IORef Double
    , contextTriangles :: IORef Triangles
    }

white, black, red :: Color4 GLfloat
white = Color4 1 1 1 1
black = Color4 0 0 0 1
red   = Color4 1 0 0 1

fBretz :: XYZ F -> F
fBretz (x,y,z) = ((x2 + y2/4 - 1) * (x2/4 + y2 - 1))^2 + z2
  where
  x2 = x*x
  y2 = y*y
  z2 = z*z

voxel :: Voxel F
voxel = makeVoxel fBretz ((-2.5, 2.5),(-2.5, 2.5),(-0.5, 0.5))
                         (200, 200, 100)

trianglesIO :: IO Triangles
trianglesIO = do 
  msh <- makeMesh voxel 0.1
  let vertices = _vertices msh
      faces    = _faces msh
      normals  = _normals msh
      triangle face =
        ( (vertices ! i, vertices ! j, vertices ! k)
        , (normals ! i , normals ! j , normals ! k)
        )
        where
          (i, j, k) = face
  return $ map triangle faces

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
  drawTriangle ((v1, v2, v3), (n1, n2, n3)) = do
      materialDiffuse FrontAndBack $= red
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
  lookAt (Vertex3 0 0 (-6+zoom)) (Vertex3 0 0 0) (Vector3 0 1 0)
  matrixMode $= Modelview 0
  where
    w' = realToFrac w
    h' = realToFrac h

keyboard :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -- rotations
         -> IORef Double -- zoom
         -> KeyboardCallback
keyboard rot1 rot2 rot3 zoom c _ = do
  case c of
    'e' -> rot1 $~! subtract 2
    'r' -> rot1 $~! (+ 2)
    't' -> rot2 $~! subtract 2
    'y' -> rot2 $~! (+ 2)
    'u' -> rot3 $~! subtract 2
    'i' -> rot3 $~! (+ 2)
    'm' -> zoom $~! (+ 1)
    'l' -> zoom $~! subtract 1
    'q' -> leaveMainLoop
    _   -> return ()
  postRedisplay Nothing


main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Bretzel"
  windowSize $= Size 512 512
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  clearColor $= white
  materialAmbient FrontAndBack $= black
  lighting $= Enabled
  lightModelTwoSide $= Enabled
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 0 0 (-100) 1
  ambient (Light 0) $= black
  diffuse (Light 0) $= white
  specular (Light 0) $= white
  depthFunc $= Just Less
  shadeModel $= Smooth
  rot1 <- newIORef 0.0
  rot2 <- newIORef 0.0
  rot3 <- newIORef 0.0
  zoom <- newIORef 0.0
  trgls <- trianglesIO
  triangles <- newIORef trgls
  displayCallback $= display Context {contextRot1 = rot1,
                                      contextRot2 = rot2,
                                      contextRot3 = rot3,
                                      contextZoom = zoom,
                                      contextTriangles = triangles}
  reshapeCallback $= Just (resize 0)
  keyboardCallback $= Just (keyboard rot1 rot2 rot3 zoom)
  idleCallback $= Nothing
  putStrLn "*** Bretzel ***\n\
        \    To quit, press q.\n\
        \    Scene rotation:\n\
        \        e, r, t, y, u, i\n\
        \    Zoom: l, m\n\
        \"
  mainLoop

