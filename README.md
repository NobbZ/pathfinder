Pathfinder
==========

Ein kleines Projekt zum Wettbewerb der Firmenkontaktmesse.

Kompilieren
-----------

### Vorraussetzungen ###

Die Programmiersprache Haskell muss auf dem System verfügbar sein mit dem Compiler `ghc` und optional dem Paketverwaltungssystem `cabal`.

### Kompilieren mit `Cabal` und `make` ###

```shell
$ cabal configure
$ make
```

### Kompilieren nur mit GHC ###

```shell
$ ghc --make Main.hs -O2 -o pathfinder
```

Starten
-------

### Wie finde ich die ausführbare Datei? ###

#### Wenn mit `cabal` und `make` kompiliert wurde ####

Ausgehend vom Quellverzeichnis kann man das Programm mit `dist/build/pathfinder/pathfinder` aufrufen.

#### Wenn nur mit `GHC` kompiliert wurde ####

Die ausführbare Datei befindet sich direkt im Quellverzeichnis.

### Wie rufe ich das Program auf? ###

Das Programm erwartet einen Pfad (relativ zum Arbeitsverzeichnis oder absolut) zu einem Labyrinth.

```shell
$ pathfinder boards/boards/simplest.board
```

