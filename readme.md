# k-anonymisation de données issues d'Apogée de l'UP13

voir la documentation des données.

/!\ il s'agit d'un code dans l'esprit worksheet les noms de fichiers
sont à modifier directement dans le code source.

Pour compiler, exécuter, installer sbt http://www.scala-sbt.org/ puis :

```
$ sbt
> run
```

Un (très) bon REPL Scala : Ammonite https://lihaoyi.github.io/Ammonite

Ce qu'il reste à faire :
- Décider s'il faut éliminer les données singulières des traces.
- Trouver un algorithme de clustering pour l'anonymisation des
  coordonnées GPS.
- Se donner la capacité de fusionner certaines données après-coup via
  des tables de fusion (par exemple G5PRLS et G5PLS fusionnent en
  G5PLS) qui seront construites dans une interface externe.
