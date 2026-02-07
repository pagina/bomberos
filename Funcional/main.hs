
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

--MAL
recursosPorCategoria :: [(String, [String])]
recursosPorCategoria =
  [ ("A",
     ["mangueras 38mm","mangueras 45mm","Lanzas regulables","Halligan","Hacha","Barreta",
      "Cámaras térmicas","Ventilador hidráulico o eléctrico","Iluminación portátil"])

  , ("B",
     ["Espuma AFFF","Proporcionador de espuma","Lanzas aptas para espuma",
      "Tapones y obturadores","Detectores de gases inflamables","Material absorbente"])

  , ("C",
     ["Matafuegos de CO₂","Matafuegos PQS","Guantes dieléctricos","Pértiga aislante",
      "Detector de tensión","Coordinación con empresa eléctrica"])

  , ("D",
     ["Agentes extintores Clase D","Palas metálicas","Arena seca","Contención térmica"])

  , ("F-K",
     ["Matafuegos Clase K","Mantas ignífugas","Procedimientos claros de ataque"])
  ]


-- en el valor que va a tener la funcion hay que aclararlo bien.
cuartel :: [(String, String, [String])]
cuartel = [
("Estación X Lugano","Suroeste",["A","B","C"]),
("Estación XI Albariño","Suroeste",["A","B"]),
("Estación VIII Nueva Chicago","Oeste",["A","B","C","D","F-K"]),
("Destacamento Vélez Sarsfield","Oeste",["A","B","C"]),
("Estación IX Versalles","Oeste",["A","B","C","D"]),
("Destacamento Villa Devoto","Oeste",["A","B","C"]),
("Estación VI Villa Crespo", "Centro-Norte", ["A","B"]),
("Estación VII Flores", "Centro-Norte", ["A","B","C","D"]),
("Estación V Belgrano","Noroeste",["A","B","C"]),
("Destacamento Urquiza","Noroeste",["A","B"]),
("Destacamento Palermo","Noroeste",["A","B","C","D","F-K"]),
("Destacamento Chacarita","Noroeste",["A","B","C","D"]),
("Destacamento G.E.R. Saavedra","Noroeste",["A","B","C"]),
("Estación IV Recoleta","Centro",["A","B"]),
("Destacamento Once","Centro",["A","B","C","D"]),
("Destacamento Retiro","Centro",["A","B","C"]),
("Destacamento G.E.R. Caballito","Centro",["A","B","C","D","F-K"]),
("Destacamento La Boca","Sudeste",["A","B"]),
("Estación III Barracas","Sudeste",["A","B"]),
("Destacamento Nueva Pompeya","Sudeste",["A","B","C","D","F-K"]),
("Estación II Patricios","Sudeste",["A","B","C","D","F-K"])]

barriosPorZonas :: [(String, [String])]
barriosPorZonas =
  [ ("Suroeste", ["Villa Lugano","Albariño"])
  , ("Sudeste", ["Parque Patricios","Nueva Pompeya","Barracas","La Boca"])
  , ("Oeste", ["Nueva Chicago","Vélez Sarsfield","Versalles","Villa Devoto"])
  , ("Centro-Norte", ["Villa Crespo","Flores"])
  , ("Noroeste", ["Belgrano","Villa Urquiza","Palermo","Chacarita","Saavedra"])
  , ("Centro", ["Recoleta","Once","Retiro","Caballito"])]


--porque elem devuelve un booleano
matcheoBarrio :: String -> barriosPorZonas -> Bool
matcheoBarrio barrio s = elem barrio s
