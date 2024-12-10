module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Turista = UnTurista {
    cansancio :: Number,
    estres :: Number,
    estaSolo :: Bool,
    idiomas :: [String]
} deriving(Eq, Show)

-- Punto 1

ana :: Turista
ana = UnTurista 0 21 False ["Espanol"]

beto :: Turista
beto = UnTurista 15 15 True ["Aleman"]

cathi :: Turista
cathi = UnTurista 15 15 True ["Aleman", "Catalan"]

-- Punto 2

type Excursion = Turista -> Turista

alterarCansancio :: Number -> Turista -> Turista
alterarCansancio n turista = turista {cansancio = (cansancio turista) + n}

alterarEstres :: Number -> Turista -> Turista
alterarEstres n turista = turista {estres = (estres turista) + n}

salirAcompanado :: Turista -> Turista
salirAcompanado turista = turista {estaSolo = False}

aprenderIdioma :: String -> Turista -> Turista
aprenderIdioma idioma turista
    | not(elem idioma (idiomas turista)) = turista {idiomas = (idioma:(idiomas turista))}
    | otherwise = turista

irALaPlaya :: Excursion
irALaPlaya turista
    | estaSolo turista = alterarCansancio (-5) turista
    | otherwise = alterarEstres (-1) turista

verPaisaje :: String -> Excursion
verPaisaje paisaje = alterarEstres (-(length paisaje))

salirAHablarIdioma :: String -> Excursion
salirAHablarIdioma idioma = salirAcompanado.(aprenderIdioma idioma)

intensidad :: Number -> Number
intensidad n = div n 4

caminar :: Number -> Excursion
caminar minutos = (alterarEstres (-(intensidad minutos))).(alterarCansancio (intensidad minutos))

data Marea = Tranquila | Moderada | Fuerte

paseoEnBarco :: Marea -> Excursion
paseoEnBarco Fuerte = (alterarCansancio 10).(alterarEstres 6)
paseoEnBarco Moderada = id
paseoEnBarco Tranquila = ((salirAHablarIdioma "Aleman").(verPaisaje "mar").(caminar 10))

porcentaje :: Number -> Number -> Number
porcentaje porciento n = div (porciento * n) 100

hacerExcursion :: Excursion -> Turista -> Turista
hacerExcursion excursion turista = ((alterarEstres (-(porcentaje 10 (estres turista)))).excursion) turista

deltaSegun :: (a -> Number) -> a -> a -> Number
deltaSegun f algo1 algo2 = f algo1 - f algo2

deltaExcursionSegun :: (Turista -> Number) -> Turista -> Excursion -> Number
deltaExcursionSegun indice turista excursion = deltaSegun indice (hacerExcursion excursion turista) turista

esEducativa :: Excursion -> Turista -> Bool
esEducativa excursion turista = (deltaExcursionSegun (length.idiomas) turista excursion) > 0

esDesestresante :: Excursion -> Turista -> Bool
esDesestresante excursion turista = (deltaExcursionSegun estres turista excursion) < (-3)

excursionesDesestresantes :: Turista -> [Excursion] -> [Excursion]
excursionesDesestresantes turista = filter (flip esDesestresante turista)

-- Punto 3

type Tour = [Excursion]

completo :: Tour
completo = [(caminar 20), (verPaisaje "cascada"), (caminar 40), irALaPlaya, (salirAHablarIdioma "melmacquiano")]

ladoB :: Excursion -> Tour
ladoB excursion =[(paseoEnBarco Tranquila), excursion, (caminar 120)]

islaVecina :: Marea -> Tour
islaVecina marea = [(paseoEnBarco marea), (excursionEnIslaVecina marea), (paseoEnBarco marea)]

excursionEnIslaVecina :: Marea -> Excursion
excursionEnIslaVecina Fuerte = (verPaisaje "lago")
excursionEnIslaVecina _  = irALaPlaya

hacerExcursiones :: [Excursion] -> Turista -> Turista
hacerExcursiones excursiones turista = foldr hacerExcursion turista excursiones

hacerTour :: Tour -> Turista -> Turista
hacerTour tour = (hacerExcursiones tour).(alterarEstres (length tour))

loDejaAcompanando :: Excursion -> Turista -> Bool
loDejaAcompanando excursion = (estaSolo).(hacerExcursion excursion)

excursionConveniente :: Excursion -> Turista -> Bool
excursionConveniente excursion turista = (esDesestresante excursion turista) && (loDejaAcompanando excursion turista)

esConveniente :: Tour -> Turista -> Bool
esConveniente tour turista = any (flip excursionConveniente turista) tour

algunoEsConveniente :: [Tour] -> Turista -> Bool
algunoEsConveniente toures turista = any (flip esConveniente turista) toures

deltaTourSegun :: (Turista -> Number) -> Turista -> Tour -> Number
deltaTourSegun indice turista tour = deltaSegun indice (hacerTour tour turista) turista

espiritualidadRecibida :: Tour -> Turista -> Number
espiritualidadRecibida tour turista = -((deltaTourSegun estres turista tour) + (deltaTourSegun cansancio turista tour))

efectividad :: Tour -> [Turista] -> Number
efectividad tour = sum.(map (espiritualidadRecibida tour)).(filter (esConveniente tour))

infinitasPlayas :: Tour
infinitasPlayas = repeat irALaPlaya