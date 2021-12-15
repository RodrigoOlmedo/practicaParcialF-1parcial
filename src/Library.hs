module Library where
import PdePreludat

type Palabra = String
type Verso = String
type Estrofa = [Verso]
type Artista = String -- Solamente interesa el nombre
 
esVocal :: Char -> Bool
esVocal = flip elem "aeiouAEIOUáéíóú"
 
tieneTilde :: Char -> Bool
tieneTilde = flip elem "áéíóú"
 
 
cumplen :: (a -> b) -> (b -> b -> Bool) -> a -> a -> Bool
cumplen f comp v1 v2 = comp (f v1) (f v2)

type Rima = Palabra->Palabra->Bool
rimaAsonante :: Rima
rimaAsonante palabra = (==(ultimasVocales 2 palabra)).(ultimasVocales 2)

takeLast :: Number->String -> String
takeLast cantidad = reverse.(take cantidad).reverse

ultimasVocales :: Number -> Palabra -> String
ultimasVocales cantidad = (takeLast cantidad).(filter esVocal)

rimaConsonante :: Rima
rimaConsonante palabra = (==(takeLast 3 palabra)).(takeLast 3)

palabrasRiman :: Rima
palabrasRiman palabra otraPalabra = (rimaAsonante palabra otraPalabra || rimaConsonante palabra otraPalabra)&&(palabra/=otraPalabra)

type Conjugacion = Verso -> Verso -> Bool

porRimas :: Conjugacion
porRimas verso1 = (palabrasRiman (ultimaPalabra verso1)).ultimaPalabra

ultimaPalabra :: String -> String
ultimaPalabra = last.words

anadiplosis :: Conjugacion
anadiplosis verso1 = (== (ultimaPalabra verso1)).primeraPalabra

primeraPalabra :: String -> String
primeraPalabra = head.words

conjugacionesPosibles = [porRimas, anadiplosis]

dosVersosRiman :: Conjugacion
dosVersosRiman verso1 verso2 = rimanSegunConjugaciones conjugacionesPosibles verso1 verso2 

rimanSegunConjugaciones :: [Conjugacion]->Conjugacion
rimanSegunConjugaciones [] _ _ = False
rimanSegunConjugaciones (x:xs) verso1 verso2 | x verso1 verso2 = True
                                             | otherwise = rimanSegunConjugaciones xs verso1 verso2 

type Patron = Estrofa -> Bool

simple :: Number -> Number -> Patron
simple pos1 pos2 estrofa = dosVersosRiman (versoXPosicion pos1 estrofa) (versoXPosicion pos2 estrofa)

versoXPosicion :: Number -> Estrofa -> Verso
versoXPosicion pos = (!! (pos-1))  

esdrujulas :: Patron
esdrujulas = (all esPalabraEsdrujula).ultimaPalabraDeCadaVerso

ultimaPalabraDeCadaVerso :: Estrofa -> [Palabra]
ultimaPalabraDeCadaVerso = map ultimaPalabra 

esPalabraEsdrujula :: Palabra -> Bool
esPalabraEsdrujula = tieneTilde.antepenultimaVocal

antepenultimaVocal :: Palabra -> Char
antepenultimaVocal = head.(ultimasVocales 3)

anafora :: Patron
anafora  estrofa = ((all (==((primeraPalabra.head) estrofa))).primeraPalabraDeCadaVerso) estrofa

primeraPalabraDeCadaVerso :: Estrofa -> [Palabra]
primeraPalabraDeCadaVerso = map primeraPalabra

cadena :: Conjugacion->Patron
cadena _ (x:[]) = True
cadena forma (x:xs) | forma x (head xs) = cadena forma xs
                    | otherwise = False
                        
combinaDos :: Patron->Patron->Patron
combinaDos estiloPatron estiloPatron2 estrofa = estiloPatron estrofa && estiloPatron2 estrofa

aabb :: Patron
aabb = combinaDos (simple 1 2) (simple 3 4)
abab :: Patron
abab = combinaDos (simple 1 3) (simple 2 4)
abba :: Patron
abba = combinaDos (simple 1 4) (simple 2 3)

hardcore :: Patron
hardcore = combinaDos (cadena porRimas) esdrujulas

data PuestaEnEscena = PuestaEnEscena{
    publicoExaltado :: Bool,
    potencia :: Number,
    estrofaTirada :: Estrofa,
    mc :: Artista
}
modificarPotencia :: Number->PuestaEnEscena -> PuestaEnEscena
modificarPotencia modificador puestaI = puestaI{potencia=modificador*potencia puestaI}
cambiarEstadoPublico :: Bool->PuestaEnEscena -> PuestaEnEscena
cambiarEstadoPublico estado puestaI = puestaI {publicoExaltado=estado}

type Estilo = PuestaEnEscena -> PuestaEnEscena
gritar :: Estilo
gritar = modificarPotencia 1.5 
responderAcote :: Bool->Estilo
responderAcote efectividad = cambiarEstadoPublico efectividad
tirarTecnicas :: Patron->Estilo
tirarTecnicas patronDemostrado puestaI | patronDemostrado (estrofaTirada puestaI) = modificarPotencia 1.1 puestaI
                                       | otherwise = puestaI
puestaBase = PuestaEnEscena{publicoExaltado=False, potencia = 1, estrofaTirada= [""], mc = ""}

tirarFreestyle :: Artista -> Estrofa -> Estilo -> PuestaEnEscena
tirarFreestyle mc versos estilo = estilo puestaBase{estrofaTirada=versos,mc=mc}
{-

-}