# MatermindClean
Homework in Clean programming language for ELTE Functional Programming course

1. beadandó: Mastermind
Használható segédanyagok egy külön oldalon találhatóak.

Tekintve, hogy a tesztesetek, bár odafigyelés mellett íródnak, nem fedik le minden esetben a függvény teljes működését, határozottan javasolt még külön próbálgatni a megoldásokat beadás előtt, vagy megkérdezni a felügyelőket!

A feladatban a Mastermind játék logikáját fogjuk megvalósítani. Adott egy kód, ezt négy egész szám listája fogja jelképezni. A játékos ezt a kódot próbálja kitalálni és minden próbálkozás után visszajelzést kap, hogy hány számjegyet talált el.

A következő modulokat érdemes használni: StdEnv, StdLib.

Egyezések helyenként (2 pont)
Definiáljuk a positionalMatches függvényt, amely megszámolja, hogy két kódban hányszor szerepel ugyanazon a pozíción ugyanaz a számjegy!

Típusa:

positionalMatches :: [Int] [Int] -> Int
Tesztesetek:

positionalMatches_test =
  [ positionalMatches [4,2,7,1] [1,2,3,4] == 1
  , positionalMatches [9,3,0,5] [5,6,7,8] == 0
  , positionalMatches [6,6,6,1] [6,6,5,1] == 3
  ]
Egyezések (3 pont)
Definiáljuk a matches függvényt, amely megszámolja, hogy összesen hány egyező számjegy van két kódban, pozíciótól függetlenül! Ha két kódban egy számjegy n-szer szerepel, az eredménybe ez n-szer számítson bele!

Típusa:

matches :: [Int] [Int] -> Int
Tesztesetek:

matches_test =
  [ matches [4,2,7,1] [1,2,3,4] == 3
  , matches [9,3,0,5] [5,6,7,8] == 1
  , matches [6,6,6,1] [6,6,5,1] == 3
  , matches [5,8,7,9] [9,9,7,8] == 3
  ]
Kód beolvasása (3 pont)
Definiáljuk a readCode függvényt, amely megpróbál egy szöveget egészek listájává alakítani! Az alakítást akkor végezzük el, ha minden karakter számjegy és a lista hossza pontosan 4 -- ekkor csomagoljuk a listát a Just adatkonstruktorba! A Nothing adatkonstruktor jelzi, ha nem teljesül a feltétel.

Típusa:

readCode :: String -> Maybe [Int]
Tippek:

Az isDigit függvénnyel ellenőrizni tudjuk, hogy egy karakter számjegy-e.
A digitToInt függvénnyel karaktert számmá tudunk alakítani.
Tesztesetek:

readCode_test =
  [ readCode "1234"  == Just [1,2,3,4]
  , readCode "12345" == Nothing
  , readCode "123a"  == Nothing
  ]
Maybe érték feldolgozása (2 pont)
Definiáljuk a maybe függvényt, amellyel Maybe típusú értékeket tudunk feldolgozni egy függvény és egy alapértelmezett érték segítségével!

Ha a Maybe érték Nothing, térjünk vissza az alapértelmezett, b típusú értékkel!
A Just konstruktorba csomagolt a típusú értéket a függvény paraméterrel képezzük át b típusúvá, ezzel térjünk vissza!
Típusa:

maybe :: (a -> b) b (Maybe a) -> b
Tesztesetek:

maybe_test =
  [ maybe ((+) 10) 7 Nothing  == 7
  , maybe ((+) 10) 7 (Just 5) == 15
  ]
Eredményszámítás (2 pont)
Definiáljuk az allMatches függvényt, amely egy kitalálandó kódból és a felhasználó bemenetéből visszaadja egy rendezett párban

az eltérő pozíción lévő egyezéseket (ezek nem tartalmazzák az egyező pozíción lévő egyezéseket) és
az egyező pozíción lévő egyezéseket!
Ha a felhasználó bemenete érvénytelen, a (0, 0) párt adjuk vissza!

Típusa:

allMatches :: [Int] String -> (Int, Int)
Tesztesetek:

allMatches_test =
  [ allMatches [4,2,7,1] "1234" == (2, 1)
  , allMatches [9,3,0,5] "1234" == (1, 0)
  , allMatches [9,3,0,5] "123a" == (0, 0)
  ]
Az első tesztesetnél 3 az összes egyezés, ebből 2 van eltérő pozíción, 1 van helyes pozíción.

Segítség a feltöltéshez
Az alábbi állományt érdemes módosítani, így, szövegesen kell feltölteni (az alábbi természetesen hibás működésű program):

module Mastermind

import StdEnv, StdLib

positionalMatches :: [Int] [Int] -> Int
positionalMatches xs ys = 0

matches :: [Int] [Int] -> Int
matches a b = 0

readCode :: String -> Maybe [Int]
readCode str = Nothing

maybe :: (a -> b) b (Maybe a) -> b
maybe f b x = b

allMatches :: [Int] String -> (Int, Int)
allMatches a b = (0,0)
