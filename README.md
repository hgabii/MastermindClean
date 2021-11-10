# MatermindClean
Homework in Clean programming language for ELTE Functional Programming course

Exercise description in Hungarian:
----

<div style="margin-left:0px; background: lightgray"><h1 id="beadandó-mastermind">1. beadandó: Mastermind</h1>
<p><em>Használható segédanyagok egy <a href="https://bead.inf.elte.hu/files/clean/">külön oldalon</a> találhatóak.</em></p>
<p><em>Tekintve, hogy a tesztesetek, bár odafigyelés mellett íródnak, nem fedik le minden esetben a függvény teljes működését, határozottan javasolt még külön próbálgatni a megoldásokat beadás előtt, vagy megkérdezni a felügyelőket!</em></p>
<p>A feladatban a Mastermind játék logikáját fogjuk megvalósítani. Adott egy kód, ezt négy egész szám listája fogja jelképezni. A játékos ezt a kódot próbálja kitalálni és minden próbálkozás után visszajelzést kap, hogy hány számjegyet talált el.</p>
<p>A következő modulokat érdemes használni: <code>StdEnv</code>, <code>StdLib</code>.</p>
<h2 id="egyezések-helyenként-2-pont">Egyezések helyenként (2 pont)</h2>
<p>Definiáljuk a <code>positionalMatches</code> függvényt, amely megszámolja, hogy két kódban hányszor szerepel ugyanazon a pozíción ugyanaz a számjegy!</p>
<p>Típusa:</p>
<pre><code>positionalMatches :: [Int] [Int] -&gt; Int</code></pre>
<p>Tesztesetek:</p>
<pre><code>positionalMatches_test =
  [ positionalMatches [4,2,7,1] [1,2,3,4] == 1
  , positionalMatches [9,3,0,5] [5,6,7,8] == 0
  , positionalMatches [6,6,6,1] [6,6,5,1] == 3
  ]</code></pre>
<h2 id="egyezések-3-pont">Egyezések (3 pont)</h2>
<p>Definiáljuk a <code>matches</code> függvényt, amely megszámolja, hogy összesen hány egyező számjegy van két kódban, pozíciótól függetlenül! Ha két kódban egy számjegy <code>n</code>-szer szerepel, az eredménybe ez <code>n</code>-szer számítson bele!</p>
<p>Típusa:</p>
<pre><code>matches :: [Int] [Int] -&gt; Int</code></pre>
<p>Tesztesetek:</p>
<pre><code>matches_test =
  [ matches [4,2,7,1] [1,2,3,4] == 3
  , matches [9,3,0,5] [5,6,7,8] == 1
  , matches [6,6,6,1] [6,6,5,1] == 3
  , matches [5,8,7,9] [9,9,7,8] == 3
  ]</code></pre>
<h2 id="kód-beolvasása-3-pont">Kód beolvasása (3 pont)</h2>
<p>Definiáljuk a <code>readCode</code> függvényt, amely megpróbál egy szöveget egészek listájává alakítani! Az alakítást akkor végezzük el, ha minden karakter számjegy és a lista hossza pontosan 4 -- ekkor csomagoljuk a listát a <code>Just</code> adatkonstruktorba! A <code>Nothing</code> adatkonstruktor jelzi, ha nem teljesül a feltétel.</p>
<p>Típusa:</p>
<pre><code>readCode :: String -&gt; Maybe [Int]</code></pre>
<p>Tippek:</p>
<ul>
<li>Az <code>isDigit</code> függvénnyel ellenőrizni tudjuk, hogy egy karakter számjegy-e.</li>
<li>A <code>digitToInt</code> függvénnyel karaktert számmá tudunk alakítani.</li>
</ul>
<p>Tesztesetek:</p>
<pre><code>readCode_test =
  [ readCode "1234"  == Just [1,2,3,4]
  , readCode "12345" == Nothing
  , readCode "123a"  == Nothing
  ]</code></pre>
<h2 id="maybe-érték-feldolgozása-2-pont"><code>Maybe</code> érték feldolgozása (2 pont)</h2>
<p>Definiáljuk a <code>maybe</code> függvényt, amellyel <code>Maybe</code> típusú értékeket tudunk feldolgozni egy függvény és egy alapértelmezett érték segítségével!</p>
<ul>
<li>Ha a <code>Maybe</code> érték <code>Nothing</code>, térjünk vissza az alapértelmezett, <code>b</code> típusú értékkel!</li>
<li>A <code>Just</code> konstruktorba csomagolt <code>a</code> típusú értéket a függvény paraméterrel képezzük át <code>b</code> típusúvá, ezzel térjünk vissza!</li>
</ul>
<p>Típusa:</p>
<pre><code>maybe :: (a -&gt; b) b (Maybe a) -&gt; b</code></pre>
<p>Tesztesetek:</p>
<pre><code>maybe_test =
  [ maybe ((+) 10) 7 Nothing  == 7
  , maybe ((+) 10) 7 (Just 5) == 15
  ]</code></pre>
<h2 id="eredményszámítás-2-pont">Eredményszámítás (2 pont)</h2>
<p>Definiáljuk az <code>allMatches</code> függvényt, amely egy kitalálandó kódból és a felhasználó bemenetéből visszaadja egy rendezett párban</p>
<ul>
<li>az eltérő pozíción lévő egyezéseket (ezek nem tartalmazzák az egyező pozíción lévő egyezéseket) és</li>
<li>az egyező pozíción lévő egyezéseket!</li>
</ul>
<p>Ha a felhasználó bemenete érvénytelen, a <code>(0, 0)</code> párt adjuk vissza!</p>
<p>Típusa:</p>
<pre><code>allMatches :: [Int] String -&gt; (Int, Int)</code></pre>
<p>Tesztesetek:</p>
<pre><code>allMatches_test =
  [ allMatches [4,2,7,1] "1234" == (2, 1)
  , allMatches [9,3,0,5] "1234" == (1, 0)
  , allMatches [9,3,0,5] "123a" == (0, 0)
  ]</code></pre>
<p>Az első tesztesetnél 3 az összes egyezés, ebből 2 van eltérő pozíción, 1 van helyes pozíción.</p>
<h2 id="segítség-a-feltöltéshez">Segítség a feltöltéshez</h2>
<p>Az alábbi állományt érdemes módosítani, így, szövegesen kell feltölteni (az alábbi természetesen hibás működésű program):</p>
<pre><code>module Mastermind

import StdEnv, StdLib

positionalMatches :: [Int] [Int] -&gt; Int
positionalMatches xs ys = 0

matches :: [Int] [Int] -&gt; Int
matches a b = 0

readCode :: String -&gt; Maybe [Int]
readCode str = Nothing

maybe :: (a -&gt; b) b (Maybe a) -&gt; b
maybe f b x = b

allMatches :: [Int] String -&gt; (Int, Int)
allMatches a b = (0,0)</code></pre></div>
