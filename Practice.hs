-- Autores:
-- Laura Restrepo Berrio
-- Johan Samuel Rico

import System.Directory
import Data.List
import System.IO
import Control.DeepSeq (deepseq)

-- Definición de producto
data Producto = Producto {
-- Atributos del producto
    nombre :: String,
    categoria :: String,
    precio :: Float
} deriving (Show, Read)

-- Función para cargar la información del inventario desde un archivo de texto
leerInventario :: IO [Producto]
leerInventario = do
    contenido <- withFile "inventario.txt" ReadMode $ \h -> do
        contenido <- hGetContents h
        -- Evalua el contenido para forzar la lectura del archivo
        contenido `deepseq` return contenido
    let lineas = lines contenido
    -- Convierte las líneas del archivo en una lista de productos
    return (map leerProducto lineas)
    where
        leerProducto linea = read linea :: Producto

-- Función para guardar los productos en un archivo de texto
guardarInventario :: [Producto] -> IO ()
guardarInventario inventario = do
    let filePath = "inventario.txt"
    -- Escribe el inventario en el archivo
    withFile filePath WriteMode $ \h -> do
        -- Convierte los productos en cadenas de texto y las escribe en el archivo
        hPutStr h (unlines (map show inventario))
    putStrLn "Inventario guardado en el archivo inventario.txt."

-- Funcion para mostrar los productos
mostrarProducto :: Producto -> String
mostrarProducto (Producto nombre categoria precio) =
    "Producto: " ++ nombre ++ "\ncategoria: " ++ categoria ++ "\nprecio: " ++ show precio ++ "\n"
    
-- Funcion para buscar productos por categoria
buscarPorCategoria :: [Producto] ->     String -> [Producto]
buscarPorCategoria inventario cat = filter (\producto -> cat == categoria producto) inventario

-- Funcion para listar todos los productos
listarProductos :: [Producto] -> IO ()
listarProductos [] = putStrLn "No hay productos en el inventario"
listarProductos inventario = do
    putStrLn "\nProductos en el inventario:\n"
    -- Muestra cada producto en el inventario
    mapM_ (putStrLn . mostrarProducto) inventario


-- Funcion para mostrar la cantidad de productos por categoria
cantidadPorCategoria :: [Producto] -> String -> Int
cantidadPorCategoria inventario cat = length (buscarPorCategoria inventario cat)


main :: IO ()
main = do
    inventario <- leerInventario
    putStrLn "!Bienvenido al Sistema de Gestión de Inventario!"
    menu inventario

menu :: [Producto] -> IO ()
menu inventario = do
    putStrLn "\nSeleccione una opción:"
    putStrLn "1. Añadir producto"
    putStrLn "2. Buscar artículos por categoría"
    putStrLn "3. Listar todos los artículos"
    putStrLn "4. Mostrar cantidad de artículos por categoría" 
    putStrLn "5. Salir"
    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Nombre: "
            nombre <- getLine
            putStrLn "Categoria: "
            categoria <- getLine
            putStrLn "Precio: "
            precio <- getLine
            let producto = Producto nombre categoria (read precio :: Float)
            guardarInventario (inventario ++ [producto])
            menu (inventario ++ [producto])
        "2" -> do
            putStrLn "Categoria: "
            categoria <- getLine
            let productos = buscarPorCategoria inventario categoria
            listarProductos productos
            menu inventario
        "3" -> do
            listarProductos inventario
            menu inventario
        "4" -> do
            putStrLn "Categoria: "
            categoria <- getLine
            let cantidad = cantidadPorCategoria inventario categoria
            putStrLn $ "Cantidad de productos en la categoria " ++ categoria ++ ": " ++ show cantidad
            menu inventario
        "5" -> do
            putStrLn "Adios"
        _ -> do
            putStrLn "Opcion invalida"
            menu inventario