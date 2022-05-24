-----------------------------
---------- Parte 1 ----------
-----------------------------

-- Modelar a las personas, de las cuales nos interesa la edad, cuáles son los ítems que tiene y la cantidad de experiencia que tiene; y a las criaturas teniendo en cuenta lo descrito anteriormente, y lo que queremos hacer en el punto siguiente.
type Item = String
type Requisito = (Persona -> Bool)
data Persona = UnaPersona {edad :: Int, items :: [Item], experiencia :: Int}
data Criatura = UnaCriatura {peligrosidad :: Int, requisito :: Requisito}

-- El siempredetras: la peligrosidad de esta criatura legendaria es 0, ya que no le hace nada a la persona que está acechando, es tan inofensivo que nunca nadie pudo afirmar que estaba siendo acechado. Sin embargo, no hay nada que se pueda hacer para que te deje en paz.
-- o
-- siempreFalso _ = False
-- siempreDetras = UnaCriatura { peligrosidad=0, requisito=siempreFalso}
siempreDetras = UnaCriatura {peligrosidad = 0, requisito = const False}

-- Los gnomos: individualmente son inofensivos, pero se especializan en atacar en grupo. La peligrosidad es 2 elevado a la cantidad de gnomos agrupados. Una persona puede deshacerse de un grupo de gnomos si tiene un soplador de hojas entre sus ítems.
tiene elemento persona = elem elemento (items persona)

gnomo cuantosGnomos = UnaCriatura {peligrosidad = 2 ^ cuantosGnomos, requisito = tiene "Soplador de hojas"}

-- Los fantasmas: se categorizan del 1 al 10 dependiendo de qué tan poderosos sean, y el nivel de peligrosidad es esa categoría multiplicada por 20. Cada fantasma tiene un asunto pendiente distinto, con lo cual se debe indicar para cada uno qué tiene que cumplir la persona para resolver su conflicto.
fantasma categoria requisito = UnaCriatura {peligrosidad = 20 * categoria, requisito = requisito}

-- Hacer que una persona se enfrente a una criatura, que implica que si esa persona puede deshacerse de ella gane tanta experiencia como la peligrosidad de la criatura, o que se escape (que le suma en 1 la experiencia, porque de lo visto se aprende) en caso de que no pueda deshacerse de ella.
cuantaExperiencia persona criatura | requisito criatura persona = peligrosidad criatura
                                   | otherwise = 1

enfrentar criatura persona = persona {experiencia = experiencia persona + cuantaExperiencia persona criatura}

-- Determinar cuánta experiencia es capaz de ganar una persona luego de enfrentar sucesivamente a un grupo de criaturas
cuantaExperienciaAlEnfrentar persona criaturas = experiencia (foldr enfrentar persona criaturas) - experiencia persona 

-- Mostrar un ejemplo de consulta para el punto anterior incluyendo las siguientes criaturas: al siempredetras, a un grupo de 10 gnomos, un fantasma categoría 3 que requiere que la persona tenga menos de 13 años y un disfraz de oveja entre sus ítems para que se vaya y un fantasma categoría 1 que requiere que la persona tenga más de 10 de experiencia.
requisitoFantasma persona = edad persona > 13 && tiene "disfraz de oveja" persona 
unaPersonaDeEjemplo = UnaPersona {edad = 15, items = ["Soplador de hojas"], experiencia = 15}
ejemplo = cuantaExperienciaAlEnfrentar unaPersonaDeEjemplo [gnomo 10, fantasma 3 requisitoFantasma, fantasma 1 (\persona -> experiencia persona > 10)]






-----------------------------
---------- Parte 2 ----------
-----------------------------

-- 1) Definir recursivamente la función:
-- zipWithIf :: (a -> b -> b) -> (b -> Bool) -> [a] -> [b] -> [b]
-- que a partir de dos listas retorne una lista donde cada elemento:
-- se corresponda con el elemento de la segunda lista, en caso de que el mismo
-- no cumpla con la condición indicada
-- - en el caso contrario, debería usarse el resultado de aplicar la primer función con el par de elementos de dichas listas Sólo debería avanzarse sobre los elementos de la primer lista cuando la condición se cumple.
-- > zipWithIf (*) even [10..50] [1..7]
--[1,20,3,44,5,72,7] ← porque [1, 2*10, 3, 4*11, 5, 6*12, 7]

zipWithIf _ _ [] _ = []
zipWithIf _ _ _ [] = [] -- Sólo para que no diga de todo Haskell
zipWithIf funcion condicion (cabezaLista1 : restoLista1) (cabezaLista2 : restoLista2) | condicion cabezaLista2 = funcion cabezaLista1 cabezaLista2 : zipWithIf funcion condicion restoLista1 restoLista2
                                                                                      | otherwise = cabezaLista2 : zipWithIf funcion condicion (cabezaLista1 : restoLista1) restoLista2

-- a. Hacer una función abecedarioDesde :: Char -> [Char] que retorne las letras del abecedario empezando por la letra indicada
abecedarioDesde letra = init ([letra .. 'z'] ++ ['a' .. letra])

-- b. Hacer una función desencriptarLetra :: Char -> Char -> Char que a partir una letra clave (la que reemplazaría a la a) y la letra que queremos desencriptar, retorna la letra que se corresponde con esta última en el abecedario que empieza con la letra clave. Por ejemplo: desencriptarLetra 'x' 'b' retornaría 'e'.

enMismaPosicion _ _ [] = error "No existe la letra"
enMismaPosicion _ [] _ = error "No existe la letra"
enMismaPosicion elementoBuscado (cabezaLista1:restoLista1) (cabezaLista2:restoLista2)
                        | cabezaLista1 == elementoBuscado = cabezaLista2
                        | otherwise = enMismaPosicion elementoBuscado restoLista1 restoLista2

-- Si vamos por la alternativa con zipwith en el punto d...
desencriptarLetra letraClave letraADesencriptar 
            | esLetra letraADesencriptar = enMismaPosicion letraADesencriptar (abecedarioDesde 'a') (abecedarioDesde letraClave)
            | otherwise = letraADesencriptar
-- Y si no, directamente
desencriptarLetraParaZipWith letraClave letraADesencriptar = enMismaPosicion letraADesencriptar (abecedarioDesde 'a') (abecedarioDesde letraClave)

-- c. Definir la función cesar :: Char -> String -> String que recibe la letra clave y un texto encriptado y retorna todo el texto desencriptado, teniendo en cuenta que cualquier caracter del mensaje encriptado que no sea una letra (por ejemplo '!') se mantiene igual. Usar zipWithIf para resolver este problema.
-- Alternativa sin zipwith (ojo que desencriptarLetra tiene que tener la lógica de que no transforme no-letras )
esLetra letra = elem letra (abecedarioDesde 'a')
cesar letraClave = map (desencriptarLetra letraClave)

-- Con zipwith, con el desencriptarLetra más simple
cesar' letraClave = zipWithIf desencriptarLetraParaZipWith esLetra (repeat letraClave) 
-- cesar' letraClave texto = zipWithIf desencriptarLetraParaZipWith esLetra (repeat letraClave) texto 

-- d. Realizar una consulta para obtener todas las posibles desencripciones (una por cada letra del abecedario) usando cesar para el texto "jrzel zrfaxal!"
-- map (\letra -> cesar letra "jrzel zrfaxal!") (abecedarioDesde 'a')
-- map (\letra -> cesar' letra "jrzel zrfaxal!") (abecedarioDesde 'a')
