
data Producto = Producto {
    nombre :: String,
    categoria :: String,
    precio :: Float,
} deriving (Show, Read)



main :: IO ()
main = do



Menu :: [Productos] -> IO ()
Menu