import Data.List (sortOn, intersect)

------------------------------TIPOS Y Clases de tipo-----------------------------------
type Barrio = String 
type Recurso = String
type Coordenada = (Float, Float)

data ClaseIncendio = A | B | C | D | FK
  deriving (Eq, Show)

data Zona = Suroeste | Sudeste | Oeste | CentroNorte | Noroeste | Centro
  deriving (Eq, Show)

data Estado = Libre | Ocupado
  deriving (Eq, Show)

data Cuartel = Cuartel
  { nombre   :: String
  , zona     :: Zona
  , barrio   :: Barrio
  , coord    :: Coordenada
  , recursos :: [Recurso]
  , personal :: Int
  , estado   :: Estado
  } deriving (Show, Eq)

--incendio que reporta un usuario
data Incendio = Incendio
  { coordIncendio :: Coordenada
  , clase         :: ClaseIncendio
  } deriving (Show)

---------------------------------------------------------------------------
-- CUARTELES (COORDENADAS SIMULADAS)

cuarteles :: [Cuartel]
cuarteles = 
  [ Cuartel "Patricios" Sudeste "Parque Patricios" (1,1) (recursosPorClase A ++ recursosPorClase B) 18 Libre
  , Cuartel "Barracas" Sudeste "Barracas" (2,1) (recursosPorClase A ++ recursosPorClase B ++ recursosPorClase C) 20 Libre
  , Cuartel "Destacamento Nueva Pompeya" Sudeste "Nueva Pompeya" (3,4) (recursosPorClase A ++ recursosPorClase B ++ recursosPorClase C ++ recursosPorClase D ++ recursosPorClase FK) 14 Libre
  , Cuartel
      "Destacamento La Boca" Sudeste "La Boca" (3,4)
      (recursosPorClase A ++ recursosPorClase B ++ recursosPorClase C ++ recursosPorClase D)
      15 Libre
  , Cuartel "Recoleta" Centro "Recoleta" (3,4) (recursosPorClase A ++ recursosPorClase C) 22 Libre
  , Cuartel "Palermo" Noroeste "Palermo" (5,5) (recursosPorClase A ++ recursosPorClase B ++ recursosPorClase C ++ recursosPorClase D ++ recursosPorClase FK) 20 Libre
  , Cuartel "Chacarita" Noroeste "Chacarita" (4,5) (recursosPorClase A ++ recursosPorClase FK) 17 Libre
   , Cuartel
      "Estación V Belgrano" Noroeste "Belgrano" (4,2)
      (recursosPorClase A ++ recursosPorClase C)
      22 Libre
  , Cuartel
      "Destacamento Villa Urquiza" Noroeste "Villa Urquiza" (2,2)
      (recursosPorClase A ++ recursosPorClase FK)
      18 Libre
  , Cuartel
      "Destacamento G.E.R. Saavedra" Noroeste "Saavedra" (1,1)
      (recursosPorClase A ++ recursosPorClase C)
      16 Libre
  , Cuartel "Caballito" Centro "Caballito" (3,3) 
  (recursosPorClase B ++ recursosPorClase FK) 18 Ocupado  
  , Cuartel
      "Destacamento Once"
      Centro
      "Balvanera" (3,5) 
      (recursosPorClase A ++ recursosPorClase B ++ recursosPorClase C ++ recursosPorClase D ++ recursosPorClase FK)
      16
      Libre
  , Cuartel
      "Destacamento Retiro"
      Centro
      "Retiro" (1,1)
      (recursosPorClase A ++ recursosPorClase C)
      14
      Libre
       , Cuartel
      "Estación VI Villa Crespo" CentroNorte "Villa Crespo" (4,1)
      (recursosPorClase A ++ recursosPorClase B ++ recursosPorClase C)
      19 Libre
  , Cuartel
      "Estación VII Flores" CentroNorte "Flores" (1,5)
      (recursosPorClase A ++ recursosPorClase D ++ recursosPorClase FK)
      21
      Ocupado
      , Cuartel
      "Estación VIII Nueva Chicago"
      Oeste
      "Liniers" (1,5)
      (recursosPorClase A ++ recursosPorClase B ++ recursosPorClase C ++ recursosPorClase D ++ recursosPorClase FK)
      20 Libre
  , Cuartel
      "Destacamento Vélez Sarsfield"
      Oeste
      "Vélez Sarsfield" (1,4)
      (recursosPorClase A ++ recursosPorClase D ++ recursosPorClase FK)
      15 Libre
  , Cuartel
      "Estación IX Versalles"
      Oeste
      "Versalles" (1,1)
      (recursosPorClase B ++ recursosPorClase FK)
      19 Ocupado
  , Cuartel
      "Destacamento Villa Devoto"
      Oeste
      "Villa Devoto" (2,2)
      (recursosPorClase A)
      20 Libre
  , Cuartel
      "Estación X Lugano" Suroeste "Villa Lugano" (2,2)
      (recursosPorClase A ++ recursosPorClase B)
      20 Libre
  , Cuartel
      "Estación XI Albariño"
      Suroeste
      "Albariño" (2,3)
      (recursosPorClase C ++ recursosPorClase D ++ recursosPorClase FK)
      20 Ocupado]

-- RECURSOS POR CLASE
recursosPorClase :: ClaseIncendio -> [Recurso]
recursosPorClase A  = ["Mangueras","Lanzas","Halligan","Camara termica"]
recursosPorClase B  = ["Espuma","Detector gases","Material absorbente"]
recursosPorClase C  = ["CO2","Guantes dielectricos","Pertiga"]
recursosPorClase D  = ["Agente D","Arena seca","Contencion termica"]
recursosPorClase FK = ["Matafuegos K","Mantas ignifugas"]

-- distancia lo que hace es restar las coordenadas del incendio menos la distancia
-- de las coordenadas de un cuartel y devuelve la distancia a la que estan. 
distanciaEuclideana :: Coordenada -> Coordenada -> Float
distanciaEuclideana (x1,y1) (x2,y2) =
  sqrt(((x2-x1)**2) + ((y2-y1)**2))

--Devuelve True Si el cuartel esta libre, false caso contrario
estadoCuartel :: Cuartel -> Bool 
estadoCuartel (Cuartel _ _ _ _ _ _ e) =  e == Libre 

--evalua si todos los recursos de la clase del incendio estan en inventario 
verificarRecursos :: ClaseIncendio -> Cuartel -> Bool 
verificarRecursos clase (Cuartel _ _ _ _ inventario _ _) =  all (\r -> r `elem` inventario) (recursosPorClase clase)
--devuelve un bool si todos los elementos de r (de los recursos de la clase A) pertenecen al inventario

--calculo la distancia de un cuartel respecto a cierta coordenada
distanciaCuartel :: Coordenada -> Cuartel -> Float
distanciaCuartel (x,y) (Cuartel _ _ _ c _ _ _) = distanciaEuclideana (x,y) c

--Cambia el estado de un cuartel 
cambiarEstado :: Cuartel -> Estado -> Cuartel
cambiarEstado (Cuartel a b c d e f otroEstado) estado =  Cuartel a b c d e f estado

-- muestra todos cuarteles Libres
cuartelesDisponibles :: [Cuartel] -> [Cuartel]
cuartelesDisponibles cuarteles = filter (estadoCuartel) (cuarteles)

-- filtra los cuarteles que cumplen con la clase del incendio
cuartelesConRecursos :: ClaseIncendio -> [Cuartel] -> [Cuartel]
cuartelesConRecursos clase cuarteles = filter (verificarRecursos clase) cuarteles

-- Ordena cada cuartel segun su distancia respecto de la coordenada de un incendio 
ordenarPorDistancia :: Coordenada -> [Cuartel] -> [Cuartel]
ordenarPorDistancia coord =
  sortOn (distanciaCuartel coord) 

-- selección óptima del mejor cuartel
mejorCuartel :: Incendio -> [Cuartel] -> Maybe Cuartel
mejorCuartel (Incendio coord claseInc) cs =
  case (ordenarPorDistancia coord
        (cuartelesConRecursos claseInc -- en el caso de que haya cuarteles con 
        --recursos y disp, llama a ordenarPorDistancia coord 
        --(le pasa los cuarteles que cumplieron las 2 caracteristicas y busca el de menor dist)
        (cuartelesDisponibles cs))) of
    []    -> Nothing -- Si no hay cuartel optimo: Nothing
    (c:_) -> Just c -- Si hay: Just Cuartel
  

resolverIncendios :: [Incendio] -> [Cuartel] -> [(Incendio,Maybe Cuartel)]
-- liberar los cuarteles
resolverIncendios [] _ = []
resolverIncendios (i:is) cs =
  case mejorCuartel i cs of
    Nothing ->
      (i, Nothing) : resolverIncendios is cs

    Just c ->
      (i, Just (cambiarEstado c Ocupado))
      : resolverIncendios is
          (map (\x -> if x == c
                      then cambiarEstado x Ocupado
                      else x) cs)


incendiosHoy :: [Incendio]
incendiosHoy =
  [ Incendio (2,1) A   
  , Incendio (5,5) C
  , Incendio (3,1) FK  
  ] 


