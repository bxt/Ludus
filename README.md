Ludus [ludi, m., o-Dekl.] - Schule, Spiel
=========================================

Version 1.0

This is an Eclipse project containing code i did for uni in Java, Python, MATLAB/Octave, Prolog, Haskell, Processing.org and a bit Mathematica, Javascript, Rust and C++.


Liste von Algorithmen
=====================

Suchen und Sortieren
--------------------

* Knuth-Morris-Pratt: [js](unilectures.js/subsequencof.js), [py](unilectures.py/src/bxt/unilectures/theoinf/hausaufg07/aufg05.py)
* Binäre Suche: [hs](https://github.com/bxt/Ludus/tree/master/unilectures.hs/binarySearch)
* Grep: [hs](unilectures.hs/grep/grep.hs), [py](unilectures.py/src/bxt/unilectures/theoinf/hausaufg07/aufg05.py)
* Äqualisierung: [py](unilectures.py/src/bxt/unilectures/crypto/fun/puddle_of_mudd.py)
* Sortieralgoritmen: [java](https://github.com/bxt/Ludus/tree/master/unilectures/src/bxt/unilectures/algorithmenunddatenstrukturen/fun/sorting)
  * Bubble sort: [java](unilectures/src/bxt/unilectures/algorithmenunddatenstrukturen/fun/sorting/BubbleSort.java)
  * Heap sort: [java](unilectures/src/bxt/unilectures/algorithmenunddatenstrukturen/fun/sorting/HeapSort.java), [coffee (extern)](https://github.com/bxt/Rainbow-Sort/blob/master/main.coffee#L177)
  * Insertion sort: [java](unilectures/src/bxt/unilectures/algorithmenunddatenstrukturen/fun/sorting/InsertionSort.java)
  * Merge sort: [java](unilectures/src/bxt/unilectures/algorithmenunddatenstrukturen/fun/sorting/MergeSort.java)
  * Patience sort: [java](unilectures/src/bxt/unilectures/algorithmenunddatenstrukturen/fun/sorting/PatienceSort.java)
  * Quick sort: [java](unilectures/src/bxt/unilectures/algorithmenunddatenstrukturen/fun/sorting/QuickSort.java)
  * Quick sort with merge sort for small arrays: [java](unilectures/src/bxt/unilectures/algorithmenunddatenstrukturen/fun/sorting/CombinedQuickSort.java)
  * Selection sort: [java](unilectures/src/bxt/unilectures/algorithmenunddatenstrukturen/fun/sorting/SelectionSort.java)
* A*: [ruby (extern)](https://github.com/bxt/adventofcode/blob/master/2016/day11/a_star.rb)
* Problem des Handlungsreisenden: [ruby (extern)](https://github.com/bxt/adventofcode/blob/master/2015/day09/main.hs)

Restklassenalgebren und Lineare Algebra
---------------------------------------

* Teiler: [hs](unilectures.hs/divisors/divisors.hs)
* Polynom-Division und -Multiplikation, Schulmethode: [m](unilectures.m/infoue-4.m), [py](unilectures.py/src/bxt/unilectures/theoinf/hausaufg04/aufg03.py), [py](unilectures.py/src/bxt/unilectures/crypto/hausaufg02/f256.py)
* Schieberegister: [m](unilectures.m/infoue-3.m)
* Matrixmultiplikation: [py](unilectures.py/src/bxt/unilectures/crypto/hausaufg03/AES.py), [java](unilectures/src/bxt/unilectures/informationsuebertragung/fun/redundancy/MatrixZ2.java)
* Gauß-Jordan-Algorithmus: [java](unilectures/src/bxt/unilectures/informationsuebertragung/fun/redundancy/MatrixZ2.java)
* Einfacher Eiklidischer Algorithmus: [py](unilectures.py/src/bxt/unilectures/theoinf/hausaufg03/aufg02.py), [RAM](unilectures.py/src/bxt/unilectures/theoinf/hausaufg03/aufg02.ramprog)
* Erweiterter Euklidischer Algorithmus
* Potenzieren, Square-and-Multiply: [cpp](unilectures.cpp/sq-n-mult.cpp), [java](unilectures/src/bxt/unilectures/algorithmenunddatenstrukturen/fun/Powers.java)
* Konvertierung Dualsystem: [py](unilectures.py/src/bxt/unilectures/theoinf/hausaufg01/binary.py)
* Konvertierung Ternärsystem: [hs](unilectures.hs/triplets/triplets.hs)
* Konvertierung Hexadezimalsystem: [py](unilectures.py/src/bxt/unilectures/theoinf/fun/color.py), [java](unilectures/src/bxt/unilectures/informationsuebertragung/fun/entropy/Hyte.java)
* Gram-Schmidtsches Orthogonalisierungsverfahren: [m](unilectures.m/gramschmidt.m)
* Bijektion Z<->N: [py](unilectures.py/src/bxt/unilectures/theoinf/hausaufg03/aufg03ab.py)

Datenstrukturen
---------------

* Stack: [cpp](unilectures.cpp/stack.cpp), [java](unilectures/src/bxt/unilectures/algorithmenunddatenstrukturen/fun/PushAndPop.java)
* Baum: [hs](unilectures.hs/types/lyah-chpt14.hs)
* Liste: [py](unilectures.py/src/bxt/unilectures/theoinf/hausaufg02/aufg3.py), [py](unilectures.py/src/bxt/unilectures/theoinf/hausaufg02/aufg5_reviewed.py)
* Hash-Map: [java](https://github.com/bxt/Ludus/tree/master/unilectures/src/bxt/unilectures/algorithmenunddatenstrukturen/fun/hashing)
* Heap: [java](unilectures/src/bxt/unilectures/informationsuebertragung/fun/entropy/Heap.java)

Automaten
---------

* Stackmaschine: [hs](unilectures.hs/types/stacks.hs), [java](unilectures/src/bxt/unilectures/informationsuebertragung/fun/entropy/StateMachine.java)
* Deterministischer Endlicher Automat: [hs](unilectures.hs/stateMachine/stateMachine.hs), [py](unilectures.py/src/bxt/unilectures/theoinf/hausaufg06/aufg03b.py)
* DEA-Äquivalenztest: [py](unilectures.py/src/bxt/unilectures/theoinf/hausaufg09/aufg03b.py)
* Nichtdeterministischer Endlicher Automat: [py](unilectures.py/src/bxt/unilectures/theoinf/hausaufg08/aufg01c.py)
* Turing-Maschine: [py](unilectures.py/src/bxt/unilectures/theoinf/hausaufg04/aufg02.py)
* RAM-Maschine: [py](unilectures.py/src/bxt/unilectures/theoinf/hausaufg03/aufg04.py)

Logik
-----

* Horn-Formel-Löser: [hs](https://github.com/bxt/Ludus/tree/master/unilectures.hs/hornsolve)
* Karnaugh-Veitch-Diagramme: [py](unilectures.py/src/bxt/unilectures/theoinf/fun/karnaugh.py)
* Sudoku-Löser: [py](unilectures.py/src/bxt/unilectures/theoinf/fun/sudoku.py)

Kombinatorik
------------

* Potenzmengen: [hs](https://github.com/bxt/Ludus/tree/master/unilectures.hs/combinations), [py](unilectures.py/src/bxt/unilectures/crypto/hausaufg03/streets.py)
* Bell-Ringing: [m](https://github.com/bxt/Ludus/tree/master/unilectures.m/bells)
* Karten Mischen: [java](unilectures/src/bxt/unilectures/algorithmenunddatenstrukturen/fun/shuffle/Shuffle.java)
* Teilsummenproblem: [C (extern)](https://github.com/bxt/adventofcode/blob/master/2015/day17/main.c)

Kodierungstheorie
-----------------

* Shannon-Fano: [m](unilectures.m/infoue6.m), [java](unilectures/src/bxt/unilectures/informationsuebertragung/fun/entropy/ShannonFano.java)
* Morse-Code: [m](unilectures.m/morse/morse.m)
* Huffman: [java](unilectures/src/bxt/unilectures/informationsuebertragung/fun/entropy/Huffman.java)
* Redundanz und Informationsgehalt: [java](unilectures/src/bxt/unilectures/informationsuebertragung/fun/entropy/Counts.java)
* Bose-Chaudhuri-Hocquenghem-Code: [java](unilectures/src/bxt/unilectures/informationsuebertragung/fun/redundancy/Blockcode.java)
* Parity-Check-Code: [java](unilectures/src/bxt/unilectures/informationsuebertragung/fun/redundancy/Blockcode.java)
* Lauflängenkodierung: [hs (extern)](https://github.com/bxt/adventofcode/blob/master/2015/day10/main.hs)

Kryptographie
-------------

* AES: [py](unilectures.py/src/bxt/unilectures/crypto/hausaufg03/AES.py)

Algorithmische Geometrie
------------------------

* Flächenberechnung von Polygonen: [java](unilectures/src/bxt/unilectures/vorkurs/flaechenberechnung/Flaechenberechnung.java)
* Konvexe Hülle (naiv): [java](unilectures/src/bxt/unilectures/algogeo/fun/convexhull/FirstConvexHullBuilder.java)
* Jarvis' gift wrapping Algorithmus: [java](unilectures/src/bxt/unilectures/algogeo/fun/convexhull/GiftWrapppingConvexHullBuilder.java)
* Andrew's monotone chain convex hull algorithm: [java](unilectures/src/bxt/unilectures/algogeo/fun/convexhull/StableConvexHullBuilder.java), [processing](unilectures.processing/src/bxt/convexhull/processing/ConvexHullSketch.java)

Sonstiges
---------

* Fibonacci-Zahlen: [py](unilectures.py/src/bxt/unilectures/theoinf/fun/color.py), [java](unilectures/src/bxt/unilectures/vorkurs/fibonacci/RecursiveFibonacci.java)
* PI-Berechnung (Monte-Carlo, Leibniz-Reihe): [java](https://github.com/bxt/Ludus/tree/master/unilectures/src/bxt/unilectures/vorkurs/pi)
* FizzBuzz: [rs](unilectures.rs/fizzbuzz.rs)
* Hirsch-Index: [java](unilectures/src/bxt/unilectures/algorithmenunddatenstrukturen/fun/HirschIndex.java)
* Hello-World: [java](unilectures/src/bxt/unilectures/vorkurs/flaechenberechnung/Flaechenberechnung.java), [rs](unilectures.rs/hello.rs), [py](unilectures.py/src/bxt/unilectures/theoinf/fun/helloWorld.py)
