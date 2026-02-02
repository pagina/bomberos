--Analizo que cuartel enviar.

--Podria hacer una función en la cual devuelve 
--aleatoriamente en que barrio sucede el incidente
--que amerita con urgencia la atención de los bomberos
--y grado del incendio utilizamos los códigos que usan
-- los bomberos

--Tipos de Incendios y Grados de Peligro:
--Clase A (Sólidos): Incendios de materiales combustibles comunes como madera, papel, telas.
--Clase B (Líquidos/Gases): Incendios de combustibles líquidos como gasolina, pinturas, alcoholes.
--Clase C (Eléctricos): Incendios que involucran equipos eléctricos energizados (transformadores, motores).
--Clase D (Metales): Incendios de metales inflamables como magnesio o aluminio.
--Clase F/K (Cocina): Aceites y grasas vegetales o animales. 

--RECURSOS QUE DEBEN TENER POR CLASE

-- Incendios Clase A (estructurales / sólidos)
--Recursos obligatorios:
--Mangueras de:
--38 mm
--45 mm
--Lanzas regulables (chorro pleno / niebla)
--Herramientas de entrada forzada:
--Halligan
--Hacha
--Barreta
--Cámaras térmicas (MUY importante)
--Ventilador hidráulico o eléctrico
--Iluminación portátil

--Riesgos cubiertos:
--Flashover
--Colapso
--Visibilidad nula

-- Incendios Clase B (líquidos y gases)
--Recursos sí o sí:
--Espuma AFFF o equivalente
--Proporcionador de espuma
--Lanzas aptas para espuma
--Tapones y obturadores
--Detectores de gases inflamables
--Material absorbente
-- Sin espuma → no se controla un incendio Clase B

-- Incendios Clase C (eléctricos)
--Recursos obligatorios:
--Matafuegos de CO₂
--Matafuegos PQS
--Guantes dieléctricos
--Pértiga aislante
--Detector de tensión
--Coordinación con empresa eléctrica
-- Prohibido agua sin corte de energía

-- Incendios Clase D (metales)
--(Alta peligrosidad – poco frecuentes, pero críticos)

--Recursos mínimos:
--Agentes extintores Clase D
--Palas metálicas
--Arena seca
--Contención térmica
-- Agua = reacción violenta

-- Incendios Clase F / K (cocinas)
--Recursos obligatorios:
--Matafuegos Clase K
--Mantas ignífugas
--Procedimientos claros de ataque


-- Funcion que decide que cuartel enviar en base a lugar 
--del hecho.

--analizar primero cercania de cuarteles, cuando dos están
--cerca se decide con el que dispone de recursos necesarios.

--es para que cuando se quiere mostrar algo por pantalla que 
--tenga acentos muestre los acentos

import System.Random (randomRIO)

data ClaseIncendio = A | B | C | D | FK
  deriving (Eq, Show)

data Zona
  = Suroeste
  | Sudeste
  | Oeste
  | CentroNorte
  | Noroeste
  | Centro
  deriving (Eq, Show)

data Cuartel = Cuartel
  { nombre   :: String
  , zona     :: Zona
  , barrios  :: [Barrio]
  , recursos :: [Recurso]
  , personal :: Int
  } deriving (Show)


type Recurso = String
recursosPorClase :: [(ClaseIncendio, [Recurso])]
recursosPorClase =
  [ (A,
     ["Mangueras 38mm","Mangueras 45mm","Lanzas regulables",
      "Halligan","Hacha","Barreta","Cámara térmica",
      "Ventilador","Iluminación portátil"])

  , (B,
     ["Espuma AFFF","Proporcionador de espuma","Lanzas espuma",
      "Tapones","Detector de gases","Material absorbente"])

  , (C,
     ["CO₂","PQS","Guantes dieléctricos","Pértiga","Detector tensión"])

  , (D,
     ["Agente Clase D","Palas","Arena seca","Contención térmica"])

  , (FK,
     ["Matafuegos K","Mantas ignífugas","Procedimiento de ataque"])
  ]

type Barrio = String
barriosPorZona :: [(Zona, [Barrio])]
barriosPorZona =
  [ (Suroeste, ["Villa Lugano","Albariño"])
  , (Sudeste, ["Parque Patricios","Nueva Pompeya","Barracas","La Boca"])
  , (Oeste, ["Nueva Chicago","Mataderos","Liniers","Vélez Sarsfield","Versalles","Villa Devoto"])
  , (CentroNorte, ["Villa Crespo","Flores"])
  , (Noroeste, ["Belgrano","Villa Urquiza","Palermo","Chacarita","Saavedra"])
  , (Centro, ["Recoleta","Once","Balvanera","Retiro","Caballito"])
  ]

cuarteles :: [Cuartel]
cuarteles =
  -- ZONA SUR / SUDESTE
  [ Cuartel
      "Estación II Patricios"
      Sudeste
      ["Parque Patricios"]
      (recursosClase A ++ recursosClase B)
      18
  , Cuartel
      "Destacamento Nueva Pompeya"
      Sudeste
      ["Nueva Pompeya"]
      (recursosClase A ++ recursosClase B ++ recursosClase C ++ recursosClase D ++ recursosClase FK)
      14
  , Cuartel
      "Estación III Barracas"
      Sudeste
      ["Barracas"]
      (recursosClase A ++ recursosClase B ++ recursosClase C ++ recursosClase D ++ recursosClase FK)
      20
  , Cuartel
      "Destacamento La Boca"
      Sudeste
      ["La Boca"]
      (recursosClase A ++ recursosClase B ++ recursosClase C ++ recursosClase D)
      15

  -- ZONA CENTRO
  , Cuartel
      "Estación IV Recoleta"
      Centro
      ["Recoleta"]
      (recursosClase A ++ recursosClase B ++ recursosClase C)
      22
  , Cuartel
      "Destacamento Once"
      Centro
      ["Once","Balvanera"]
      (recursosClase A ++ recursosClase B ++ recursosClase C ++ recursosClase D ++ recursosClase FK)
      16
  , Cuartel
      "Destacamento Retiro"
      Centro
      ["Retiro"]
      (recursosClase A ++ recursosClase C)
      14
  , Cuartel
      "Destacamento G.E.R. Caballito"
      Centro
      ["Caballito"]
      (recursosClase FK ++ recursosClase B)
      18

  -- ZONA NORTE / NOROESTE
  , Cuartel
      "Estación V Belgrano"
      Noroeste
      ["Belgrano"]
      (recursosClase A ++ recursosClase C)
      22
  , Cuartel
      "Destacamento Villa Urquiza"
      Noroeste
      ["Villa Urquiza"]
      (recursosClase A ++ recursosClase FK)
      18
  , Cuartel
      "Destacamento Palermo"
      Noroeste
      ["Palermo"]
      (recursosClase A ++ recursosClase B ++ recursosClase C ++ recursosClase D ++ recursosClase FK)
      20
  , Cuartel
      "Destacamento Chacarita"
      Noroeste
      ["Chacarita"]
      (recursosClase A ++ recursosClase B ++ recursosClase FK)
      17
  , Cuartel
      "Destacamento G.E.R. Saavedra"
      Noroeste
      ["Saavedra"]
      (recursosClase A ++ recursosClase C)
      16


  -- ZONA CENTRO-NORTE / OESTE CERCANO
  , Cuartel
      "Estación VI Villa Crespo"
      CentroNorte
      ["Villa Crespo"]
      (recursosClase A ++ recursosClase B ++ recursosClase C)
      19
  , Cuartel
      "Estación VII Flores"
      CentroNorte
      ["Flores"]
      (recursosClase A ++ recursosClase D ++ recursosClase FK)
      21

  -- ZONA OESTE / NOROESTE LEJANO
  , Cuartel
      "Estación VIII Nueva Chicago"
      Oeste
      ["Nueva Chicago","Mataderos","Liniers"]
      (recursosClase A ++ recursosClase B ++ recursosClase C ++ recursosClase D ++ recursosClase FK)
      20
  , Cuartel
      "Destacamento Vélez Sarsfield"
      Oeste
      ["Vélez Sarsfield","Floresta"]
      (recursosClase A ++ recursosClase D ++ recursosClase FK)
      15
  , Cuartel
      "Estación IX Versalles"
      Oeste
      ["Versalles"]
      (recursosClase B ++ recursosClase FK)
      19

  , Cuartel
      "Destacamento Villa Devoto"
      Oeste
      ["Villa Devoto"]
      (recursosClase A)
      20
  
  , Cuartel
      "Estación X Lugano"
      Suroeste
      ["Villa Lugano"]
      (recursosClase A ++ recursosClase B)
      20
  , Cuartel
      "Estación XI Albariño"
      Suroeste
      ["Albariño"]
      (recursosClase C ++ recursosClase D ++ recursosClase FK)
      20]

matcheoBarrioZona :: Barrio -> [(Zona, [Barrio])] -> Maybe Zona
matcheoBarrioZona _ [] = Nothing
matcheoBarrioZona barrio ((z, bs):rbs)
  | elem barrio bs = Just z
  | otherwise = matcheoBarrioZona barrio rbs

recursosClase :: ClaseIncendio -> [Recurso]
recursosClase clase =
  case filter (\(c, _) -> c == clase) recursosPorClase of
    [(_, rs)] -> rs
    _         -> []

--MOSTRAR RECURSOS DE FORMA MÁS LEGIBLE
mostrarRecursosClase :: ClaseIncendio -> IO ()
mostrarRecursosClase clase =
  mapM_ putStrLn (recursosClase clase)

--filtra los cuarteles por barrio
cuartelesPorBarrio :: Barrio -> [Cuartel]
cuartelesPorBarrio b =
  filter (\c -> b `elem` barrios c) cuarteles

--Se queda solo con los cuarteles que tienen TODOS los recursos 
--necesarios para la clase de incendio
cuartelesConRecursos :: ClaseIncendio -> [Cuartel] -> [Cuartel]
cuartelesConRecursos clase =
  filter (\c -> all (`elem` recursos c) (recursosClase clase))

--Decide que cuartel acude a incendio
--decidirCuartel :: Barrio -> ClaseIncendio -> Maybe Cuartel
--decidirCuartel barrio clase =
--  case cuartelesConRecursos clase (cuartelesPorBarrio barrio) of
--    []    -> Nothing
--    (c:_) -> Just c

decidirCuartel :: Barrio -> ClaseIncendio -> IO (Maybe Cuartel)
decidirCuartel barrio clase =
  case cuartelesConRecursos clase (cuartelesPorBarrio barrio) of
    [] -> return Nothing
    cs -> do
      i <- randomRIO (0, length cs - 1)
      return (Just (cs !! i))
