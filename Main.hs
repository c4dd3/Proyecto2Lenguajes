import System.IO
import System.Directory (doesFileExist)
import Control.Monad (when)
import Data.List (intercalate)
import Control.Exception (evaluate)
import System.Process (callCommand)
import System.Info (os)
import Control.Exception (catch, IOException)

main :: IO ()
main = do
  putStrLn "=== Administrador de Contraseñas ==="
  menuPrincipal

menuPrincipal :: IO ()
menuPrincipal = do
  putStrLn "Seleccione una opción:"
  putStrLn "1. Iniciar Sesión"
  putStrLn "2. Registrarse"
  putStrLn "3. Salir del programa"
  opcion <- getLine
  case opcion of
    "1" -> iniciarSesion >> menuPrincipal
    "2" -> registrarUsuario >> menuPrincipal
    "3" -> putStrLn "Saliendo del programa. ¡Hasta luego!"
    _   -> putStrLn "Opción inválida. Intente de nuevo." >> menuPrincipal

-- Registro
registrarUsuario :: IO ()
registrarUsuario = do
  putStrLn "--- Registro de Usuario ---"
  putStrLn "Ingrese nombre de usuario: "
  nombre <- getLine
  existe <- verificarUsuarioExistente nombre
  if existe
    then putStrLn "El usuario ya existe. Intente con otro nombre."
    else do
      pin <- solicitarPIN
      guardarUsuario nombre pin
      putStrLn "Usuario registrado con éxito."

-- Login
iniciarSesion :: IO ()
iniciarSesion = do
  putStrLn "--- Iniciar Sesión ---"
  putStrLn "Ingrese nombre de usuario: "
  nombre <- getLine
  existe <- verificarUsuarioExistente nombre
  if not existe
    then putStrLn "El usuario no existe. Regístrese primero."
    else do
      putStrLn "Ingrese su PIN: "
      pin <- getLine
      valido <- verificarCredenciales nombre pin
      if valido
        then do
          putStrLn $ "Inicio de sesión exitoso. Bienvenido, " ++ nombre ++ "."
          menuUsuario nombre
        else putStrLn "PIN incorrecto. Intente de nuevo."

menuUsuario :: String -> IO ()
menuUsuario nombre = do
  putStrLn $ "--- Menú de " ++ nombre ++ " ---"
  putStrLn "1. Ver servicios guardados"
  putStrLn "2. Agregar nuevo servicio"
  putStrLn "3. Editar un servicio"
  putStrLn "4. Borrar un servicio"
  putStrLn "5. Consultar un servicio"
  putStrLn "6. Cerrar sesión"
  opcion <- getLine
  case opcion of
    "1" -> verServicios nombre >> menuUsuario nombre
    "2" -> agregarServicio nombre >> menuUsuario nombre
    "3" -> editarServicio nombre >> menuUsuario nombre
    "4" -> borrarServicio nombre >> menuUsuario nombre
    "5" -> consultarServicio nombre >> menuUsuario nombre
    "6" -> putStrLn "Sesión cerrada. Volviendo al menú principal."
    _   -> putStrLn "Opción inválida. Intente de nuevo." >> menuUsuario nombre

-- Función para mostrar servicios con índice y devolver lista para manipular
listarServiciosConIndice :: String -> IO [(Int, String)]
listarServiciosConIndice nombreUsuario = do
  existeArchivo <- doesFileExist "servicios.txt"
  if not existeArchivo
    then do
      putStrLn "No hay servicios guardados aún."
      return []
    else do
      contenido <- readFile "servicios.txt"
      let lineas = lines contenido
          serviciosUsuario = filter (\linea -> takeWhile (/= ':') linea == nombreUsuario) lineas
      if null serviciosUsuario
        then do
          putStrLn "No tienes servicios guardados."
          return []
        else do
          putStrLn "--- Tus servicios guardados ---"
          putStrLn $ padRight 5 "ID" ++ padRight 15 "Servicio" ++ padRight 25 "Identificador" ++ "Contraseña"
          putStrLn $ replicate 70 '-'
          let enumerados = zip [1..] serviciosUsuario
          mapM_ (\(i,linea) -> putStrLn $ padRight 5 (show i) ++ formatoServicioTabla linea) enumerados
          putStrLn ""
          return enumerados

formatoServicioTabla :: String -> String
formatoServicioTabla linea =
  let (_usuario:servicio:identificador:contrasena:_) = splitBy ':' linea
  in padRight 15 servicio ++ padRight 25 identificador ++ contrasena

-- Editar servicio
editarServicio :: String -> IO ()
editarServicio nombreUsuario = do
  servicios <- listarServiciosConIndice nombreUsuario
  if null servicios
    then return ()
    else do
      putStrLn "Ingrese el ID del servicio que desea editar: "
      input <- getLine
      case reads input of
        [(idSeleccionado, "")] -> 
          if idSeleccionado >= 1 && idSeleccionado <= length servicios
            then do
              let (_, lineaOriginal) = servicios !! (idSeleccionado - 1)
              let (_usuario:servicio:identificador:contrasena:_) = splitBy ':' lineaOriginal
              putStrLn $ "Editando servicio: " ++ servicio
              putStrLn $ "Nuevo nombre del servicio (Enter para mantener: " ++ servicio ++ "):"
              nuevoServicio <- getLine
              putStrLn $ "Nuevo identificador (Enter para mantener: " ++ identificador ++ "):"
              nuevoIdentificador <- getLine
              putStrLn $ "Nueva contraseña (Enter para mantener):"
              nuevaContrasena <- getLine
              let servicioFinal = if null nuevoServicio then servicio else nuevoServicio
                  identificadorFinal = if null nuevoIdentificador then identificador else nuevoIdentificador
                  contrasenaFinal = if null nuevaContrasena then contrasena else nuevaContrasena
                  nuevaLinea = nombreUsuario ++ ":" ++ servicioFinal ++ ":" ++ identificadorFinal ++ ":" ++ contrasenaFinal
              -- Reescribir archivo con el servicio editado
              actualizarLineaArchivo "servicios.txt" lineaOriginal nuevaLinea
              putStrLn "Servicio editado exitosamente."
            else putStrLn "ID inválido."
        _ -> putStrLn "Entrada inválida."

-- Borrar servicio
borrarServicio :: String -> IO ()
borrarServicio nombreUsuario = do
  servicios <- listarServiciosConIndice nombreUsuario
  if null servicios
    then return ()
    else do
      putStrLn "Ingrese el ID del servicio que desea borrar: "
      input <- getLine
      case reads input of
        [(idSeleccionado, "")] ->
          if idSeleccionado >= 1 && idSeleccionado <= length servicios
            then do
              let (_, lineaABorrar) = servicios !! (idSeleccionado - 1)
              putStrLn "¿Está seguro que desea borrar este servicio? (s/n)"
              confirm <- getLine
              when (confirm `elem` ["s","S"]) $ do
                eliminarLineaArchivo "servicios.txt" lineaABorrar
                putStrLn "Servicio borrado exitosamente."
            else putStrLn "ID inválido."
        _ -> putStrLn "Entrada inválida."

-- Función para actualizar línea en archivo
actualizarLineaArchivo :: FilePath -> String -> String -> IO ()
actualizarLineaArchivo archivo lineaVieja lineaNueva = do
  contenido <- readFile archivo
  evaluate (length contenido)  -- fuerza evaluación estricta para liberar archivo
  let lineas = lines contenido
      nuevasLineas = map (\l -> if l == lineaVieja then lineaNueva else l) lineas
  writeFile archivo (unlines nuevasLineas)

-- Función para eliminar línea en archivo
eliminarLineaArchivo :: FilePath -> String -> IO ()
eliminarLineaArchivo archivo lineaABorrar = do
  contenido <- readFile archivo
  evaluate (length contenido)  -- fuerza evaluación estricta para liberar archivo
  let lineas = lines contenido
      nuevasLineas = filter (/= lineaABorrar) lineas
  writeFile archivo (unlines nuevasLineas)

-- Alinea texto a la izquierda con espacios
padRight :: Int -> String -> String
padRight n s = s ++ replicate (max 0 (n - length s)) ' '

-- Función auxiliar para separar por ":"
splitBy :: Char -> String -> [String]
splitBy _ "" = [""]
splitBy delim (c:cs)
  | c == delim = "" : rest
  | otherwise = (c : head rest) : tail rest
  where
    rest = splitBy delim cs


agregarServicio :: String -> IO ()
agregarServicio nombreUsuario = do
  putStrLn "--- Agregar nuevo servicio ---"
  putStrLn "Nombre del servicio (ej: Gmail, Netflix, etc.): "
  nombreServicio <- getLine
  putStrLn "Nombre de usuario o correo del servicio: "
  identificador <- getLine
  putStrLn "Contraseña del servicio: "
  contrasena <- getLine
  let entrada = nombreUsuario ++ ":" ++ nombreServicio ++ ":" ++ identificador ++ ":" ++ contrasena ++ "\n"
  appendFile "servicios.txt" entrada
  putStrLn "Servicio guardado exitosamente."

-- Función modificada para mostrar servicios en formato tabla
verServicios :: String -> IO ()
verServicios nombreUsuario = do
  existeArchivo <- doesFileExist "servicios.txt"
  if not existeArchivo
    then putStrLn "No hay servicios guardados aún."
    else do
      contenido <- readFile "servicios.txt"
      let lineas = lines contenido
          serviciosUsuario = filter (\linea -> takeWhile (/= ':') linea == nombreUsuario) lineas
      if null serviciosUsuario
        then putStrLn "No tienes servicios guardados."
        else do
          putStrLn "--- Tus servicios guardados ---"
          putStrLn $ padRight 15 "Servicio" ++ padRight 25 "Identificador" ++ "Contraseña"
          putStrLn $ replicate 60 '-'
          mapM_ (uncurry mostrarServicio) (zip [1..] serviciosUsuario)
          putStrLn ""

mostrarServicio :: Int -> String -> IO ()
mostrarServicio idx linea = do
  let (_usuario:servicio:identificador:contrasena:_) = splitBy ':' linea
      usuarioOculto = ocultarUsuario identificador
      contrasenaOculta = replicate (length contrasena) '*'
  putStrLn $ padRight 5 (show idx) ++ padRight 15 servicio ++ padRight 25 usuarioOculto ++ contrasenaOculta

-- Oculta parte del identificador (usuario o correo)
ocultarUsuario :: String -> String
ocultarUsuario ident
  | length ident <= 4 = replicate (length ident) '*'
  | otherwise =
      let inicio = take 2 ident
          fin = reverse . take 2 . reverse $ ident
          medio = replicate (length ident - 4) '*'
      in inicio ++ medio ++ fin

-- Verificación de existencia
verificarUsuarioExistente :: String -> IO Bool
verificarUsuarioExistente nombre = do
  existeArchivo <- doesFileExist "usuarios.txt"
  if not existeArchivo
    then return False
    else do
      contenido <- readFile "usuarios.txt"
      let lineas = lines contenido
          nombres = map (takeWhile (/= ':')) lineas
      return (nombre `elem` nombres)

-- Verifica credenciales correctas
verificarCredenciales :: String -> String -> IO Bool
verificarCredenciales nombre pin = do
  contenido <- readFile "usuarios.txt"
  let lineas = lines contenido
      credenciales = map (splitBy ':') lineas
  return $ any (\cred -> length cred == 2 && head cred == nombre && cred !! 1 == pin) credenciales

-- Solicita PIN
solicitarPIN :: IO String
solicitarPIN = do
  putStrLn "Ingrese un PIN de 6 dígitos: "
  pin1 <- getLine
  if length pin1 /= 6 || not (all (`elem` ['0'..'9']) pin1)
    then putStrLn "El PIN debe tener exactamente 6 dígitos." >> solicitarPIN
    else do
      putStrLn "Confirme su PIN: "
      pin2 <- getLine
      if pin1 == pin2
        then return pin1
        else putStrLn "Los PIN no coinciden. Intente de nuevo." >> solicitarPIN

-- Guarda usuario
guardarUsuario :: String -> String -> IO ()
guardarUsuario nombre pin =
  appendFile "usuarios.txt" (nombre ++ ":" ++ pin ++ "\n")

copiarAlPortapapeles :: String -> IO ()
copiarAlPortapapeles texto = do
  case os of
    "linux"   -> callCommand $ "echo " ++ texto ++ " | xclip -selection clipboard"
    "mingw32" -> callCommand $ "echo " ++ texto ++ " | clip"
    "darwin"  -> callCommand $ "echo " ++ texto ++ " | pbcopy"
    _         -> putStrLn "Sistema operativo no soportado para copiar al portapapeles."

consultarServicio :: String -> IO ()
consultarServicio nombreUsuario = do
  putStrLn "Ingrese el nombre del servicio a consultar:"
  servicio <- getLine
  let archivo = "servicios.txt"
  contenido <- readFile archivo
  let lineas = lines contenido
      -- Filtrar líneas que tengan el usuario y el servicio solicitados
      coincidencias = filter (\linea -> 
        case splitBy ':' linea of
          (usuario:serv:_:_) -> usuario == nombreUsuario && serv == servicio
          _ -> False
        ) lineas
  if null coincidencias
    then putStrLn "Servicio no encontrado."
    else do
      let (usuario:serv:email:contrasena:_) = splitBy ':' (head coincidencias)
      putStrLn $ "Usuario: " ++ usuario
      putStrLn $ "Servicio: " ++ serv
      putStrLn $ "Email/Usuario: " ++ email
      putStrLn $ "Contraseña: " ++ contrasena
      putStrLn "¿Desea copiar al portapapeles?"
      putStrLn "1. Copiar email/usuario"
      putStrLn "2. Copiar contraseña"
      putStrLn "3. No copiar"
      opc <- getLine
      case opc of
        "1" -> copiarAlPortapapeles email >> putStrLn "Email/usuario copiado al portapapeles."
        "2" -> copiarAlPortapapeles contrasena >> putStrLn "Contraseña copiada al portapapeles."
        _   -> putStrLn "No se copió nada."

