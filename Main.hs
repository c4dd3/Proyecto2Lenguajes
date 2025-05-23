import System.IO
import System.Directory (doesFileExist)
import Control.Monad (when)
import Data.List (intercalate)
import Control.Exception (evaluate)
import System.Process (callCommand)
import System.Info (os)
import Control.Exception (catch, IOException)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --Librerias importadas con ayuda de cabal-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
import Crypto.Cipher.AES
import Crypto.Cipher.Types (BlockCipher(..), Cipher(..), nullIV, makeIV)
import Crypto.Error (CryptoFailable(..))
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base64 as B64

claveSecreta :: BS.ByteString
claveSecreta = BS.pack "1234567890abcdef" -- Clave de 16 bytes para AES-128

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- Encriptación/Desencriptación -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- Encripta un texto plano
encriptarAES :: String -> String
encriptarAES texto =
  case cipherInit claveSecreta :: CryptoFailable AES128 of
    CryptoPassed cipher ->
      let Just iv = makeIV (BS.replicate 16 '\0')
          encriptado = cbcEncrypt cipher iv (pad (BS.pack texto))
      in BS.unpack $ B64.encode encriptado
    CryptoFailed err -> error (show err)

-- Desencripta un texto encriptado
desencriptarAES :: String -> String
desencriptarAES base64Texto =
  case cipherInit claveSecreta :: CryptoFailable AES128 of
    CryptoPassed cipher ->
      let Just iv = makeIV (BS.replicate 16 '\0')
          encriptado = case B64.decode (BS.pack base64Texto) of
                         Right bytes -> bytes
                         Left _ -> error "Texto mal encriptado"
          desencriptado = cbcDecrypt cipher iv encriptado
      in BS.unpack (unpad desencriptado)
    CryptoFailed err -> error (show err)

-- Padding al estilo PKCS7
pad :: BS.ByteString -> BS.ByteString
pad bs = let padding = 16 - BS.length bs `mod` 16
         in bs `BS.append` BS.replicate padding (toEnum padding)

-- Quitar padding
unpad :: BS.ByteString -> BS.ByteString
unpad bs = BS.take (BS.length bs - fromEnum (BS.last bs)) bs
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- Main -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- Función principal (MAIN)
main :: IO ()
main = do
  putStrLn "=== Administrador de Contraseñas ==="
  menuPrincipal

-- Menú principal
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

-- Verifica credenciales correctas
verificarCredenciales :: String -> String -> IO Bool
verificarCredenciales nombre pin = do
  contenido <- readFile "usuarios.txt"
  let lineas = lines contenido
      credenciales = map (splitBy ':') lineas
  return $ any (\cred -> length cred == 2 && head cred == nombre &&
                          desencriptarAES (cred !! 1) == pin) credenciales

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
guardarUsuario nombre pin = do
  let pinCifrado = encriptarAES pin
  appendFile "usuarios.txt" (nombre ++ ":" ++ pinCifrado ++ "\n")
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- Post-Login -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- Menú de usuario
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
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- #1
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
          -- Aquí procesamos cada línea descifrando los datos
          mapM_ (uncurry mostrarServicio) (zip [1..] serviciosUsuario)
          putStrLn ""

-- Función para mostrar un servicio en formato tabla
mostrarServicio :: Int -> String -> IO ()
mostrarServicio idx linea =
  case splitBy ':' linea of
    (usuario : datosCifrados : _) -> do
      let datosDescifrados = desencriptarAES datosCifrados
          campos = splitBy ':' datosDescifrados
      case campos of
        (servicio:identificador:contrasena:_) -> do
          let usuarioOculto = ocultarUsuario identificador
              contrasenaOculta = replicate (length contrasena) '*'
          putStrLn $ padRight 5 (show idx) ++ padRight 15 servicio ++ padRight 25 usuarioOculto ++ contrasenaOculta
        _ -> putStrLn $ "Datos descifrados mal formados: " ++ datosDescifrados
    _ -> putStrLn $ "Línea mal formada: " ++ linea

-- Oculta parte del identificador (usuario o correo)
ocultarUsuario :: String -> String
ocultarUsuario ident
  | length ident <= 4 = replicate (length ident) '*'
  | otherwise =
      let inicio = take 2 ident
          fin = drop (length ident - 2) ident
          medio = replicate (length ident - 4) '*'
      in inicio ++ medio ++ fin
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- #2
-- Función para agregar un nuevo servicio
agregarServicio :: String -> IO ()
agregarServicio nombreUsuario = do
  putStrLn "--- Agregar nuevo servicio ---"
  putStrLn "Nombre del servicio (ej: Gmail, Netflix, etc.): "
  nombreServicio <- getLine
  putStrLn "Nombre de usuario o correo del servicio: "
  identificador <- getLine
  putStrLn "Contraseña del servicio: "
  contrasena <- getLine
  let datosPlano = nombreServicio ++ ":" ++ identificador ++ ":" ++ contrasena
      datosCifrados = encriptarAES datosPlano
      entrada = nombreUsuario ++ ":" ++ datosCifrados ++ "\n"
  appendFile "servicios.txt" entrada
  putStrLn "Servicio guardado exitosamente."
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- #3
-- Función para editar un servicio
editarServicio :: String -> IO ()
editarServicio nombreUsuario = do
  -- Listar servicios desencriptados con índice y línea original encriptada
  servicios <- listarServiciosConIndice nombreUsuario
  if null servicios
    then putStrLn "No tienes servicios guardados."
    else do
      putStrLn "Servicios disponibles:"
      -- Mostrar servicios desencriptados para que el usuario elija
      mapM_ (\(idx, lineaEnc) -> do
          let (_:datosCifrados:_) = splitBy ':' lineaEnc
              datosPlano = desencriptarAES datosCifrados
              [servicio, identificador, contrasena] = splitBy ':' datosPlano
          putStrLn $ show idx ++ ". " ++ servicio ++ " (" ++ ocultarUsuario identificador ++ ")"
        ) servicios

      putStrLn "Ingrese el ID del servicio que desea editar: "
      input <- getLine
      case reads input of
        [(idSeleccionado, "")] ->
          if idSeleccionado >= 1 && idSeleccionado <= length servicios
            then do
              let (_, lineaOriginal) = servicios !! (idSeleccionado - 1)
                  (_:datosCifrados:_) = splitBy ':' lineaOriginal
                  datosPlano = desencriptarAES datosCifrados
                  [servicio, identificador, contrasena] = splitBy ':' datosPlano

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

                  datosFinalesPlano = servicioFinal ++ ":" ++ identificadorFinal ++ ":" ++ contrasenaFinal
                  datosFinalesCifrados = encriptarAES datosFinalesPlano
                  nuevaLinea = nombreUsuario ++ ":" ++ datosFinalesCifrados

              -- Reescribir archivo con la línea editada
              actualizarLineaArchivo "servicios.txt" lineaOriginal nuevaLinea

              putStrLn "Servicio editado exitosamente."
            else putStrLn "ID inválido."
        _ -> putStrLn "Entrada inválida."

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
          mapM_ (\(i, lineaEnc) -> putStrLn $ padRight 5 (show i) ++ formatoServicioTabla lineaEnc) enumerados
          putStrLn ""
          return enumerados

-- Función para formatear la línea del servicio en la tabla
formatoServicioTabla :: String -> String
formatoServicioTabla lineaEnc =
  let (_usuario:datosCifrados:_) = splitBy ':' lineaEnc
      datosPlano = desencriptarAES datosCifrados
      [servicio, identificador, contrasena] = splitBy ':' datosPlano
  in padRight 15 servicio ++ padRight 25 (ocultarUsuario identificador) ++ replicate (length contrasena) '*'

-- Función para actualizar línea en archivo
actualizarLineaArchivo :: FilePath -> String -> String -> IO ()
actualizarLineaArchivo archivo lineaVieja lineaNueva = do
  contenido <- readFile archivo
  evaluate (length contenido)  -- fuerza evaluación estricta para liberar archivo
  let lineas = lines contenido
      nuevasLineas = map (\l -> if l == lineaVieja then lineaNueva else l) lineas
  writeFile archivo (unlines nuevasLineas)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- #4
-- Función para borrar un servicio
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
              when (confirm `elem` ["s", "S"]) $ do
                eliminarLineaArchivo "servicios.txt" lineaABorrar
                putStrLn "Servicio borrado exitosamente."
            else putStrLn "ID inválido."
        _ -> putStrLn "Entrada inválida."

-- Función para eliminar línea en archivo
eliminarLineaArchivo :: FilePath -> String -> IO ()
eliminarLineaArchivo archivo lineaABorrar = do
  contenido <- readFile archivo
  evaluate (length contenido)  -- fuerza evaluación estricta para liberar archivo
  let lineas = lines contenido
      nuevasLineas = filter (/= lineaABorrar) lineas
  writeFile archivo (unlines nuevasLineas)
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
-- #5
-- Función para consultar un servicio
consultarServicio :: String -> IO ()
consultarServicio nombreUsuario = do
  putStrLn "Ingrese el nombre del servicio a consultar:"
  servicioBuscado <- getLine
  let archivo = "servicios.txt"
  contenido <- readFile archivo
  let lineas = lines contenido
      -- Filtrar líneas del usuario
      lineasUsuario = filter (\linea -> takeWhile (/= ':') linea == nombreUsuario) lineas
      -- Desencriptar y buscar coincidencias por nombre de servicio
      coincidencias = [ (lineaOriginal, datos) 
                      | lineaOriginal <- lineasUsuario
                      , let cifrado = drop (length nombreUsuario + 1) lineaOriginal
                            datos = desencriptarAES cifrado
                            partes = splitBy ':' datos
                      , length partes >= 3
                      , let (servicio:_:_) = partes
                      , servicio == servicioBuscado
                      ]
  if null coincidencias
    then putStrLn "Servicio no encontrado."
    else do
      let (_lineaOriginal, datos) = head coincidencias
          (servicio:identificador:contrasena:_) = splitBy ':' datos
      putStrLn $ "Servicio: " ++ servicio
      putStrLn $ "Email/Usuario: " ++ identificador
      putStrLn $ "Contraseña: " ++ contrasena
      putStrLn "¿Desea copiar al portapapeles?"
      putStrLn "1. Copiar email/usuario"
      putStrLn "2. Copiar contraseña"
      putStrLn "3. No copiar"
      opc <- getLine
      case opc of
        "1" -> copiarAlPortapapeles identificador >> putStrLn "Email/usuario copiado al portapapeles."
        "2" -> copiarAlPortapapeles contrasena >> putStrLn "Contraseña copiada al portapapeles."
        _   -> putStrLn "No se copió nada."

-- Función para copiar al portapapeles
copiarAlPortapapeles :: String -> IO ()
copiarAlPortapapeles texto = do
  case os of
    "linux"   -> callCommand $ "echo " ++ texto ++ " | xclip -selection clipboard"
    "mingw32" -> callCommand $ "echo " ++ texto ++ " | clip"
    "darwin"  -> callCommand $ "echo " ++ texto ++ " | pbcopy"
    _         -> putStrLn "Sistema operativo no soportado para copiar al portapapeles."
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 

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
