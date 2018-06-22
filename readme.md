# k-anonymisation de données issues d'Apogée de l'UP13

Changer les paramètres dans le fichier `src/main/scala/main.scala`

Pour compiler, exécuter, installer sbt http://www.scala-sbt.org/ puis :

```
$ sbt
> compile
> run repertoire1/etablissement1.csv repertoire2/etablissement2.csv …
```
Où `etablissement1.csv`, `etablissement2.csv` (etc.) sont des jeux de données issus d'Apogée tels qu'extraits par le script `apogee.py`.

Un (très) bon REPL Scala : Ammonite https://lihaoyi.github.io/Ammonite

Ce qu'il reste à faire :
- Mettre à jour la documentation sur les données.
- Faire plus de généralisation et moins de suppression.
- Trouver un algorithme de clustering pour l'anonymisation des
  coordonnées GPS, puis réintégrer les adresses personnelles dans l'extraction.
- Se donner la capacité de fusionner certaines données après-coup via
  des tables de fusion (par exemple G5PRLS et G5PLS fusionnent en
  G5PLS) qui seront construites dans une interface externe.

Pour générer la documentation (/!\ ancienne) en .epub (installer pandoc) :
```
pandoc -t epub -S -o traces01.epub titre_epub_traces01.txt traces01.md
```
