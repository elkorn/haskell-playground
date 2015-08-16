import qualified Geometry as G
import qualified Geometry.Sphere as Sphere

main :: IO ()
main = do
    print $
        G.sphereVolume 10
    print $
        Sphere.volume 10
