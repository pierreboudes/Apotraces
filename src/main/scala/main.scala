package org.up13.apotraces

import data.Pub._
import data.Traces._
import exploitation.D3._

import java.io._

object Main extends App {
  // Les sections à calculer
  val calculer = Set("ktraces", "projections")
  // constante k
  val k = 5
  // Toutes nos colonnes
  val tout = Vector(
    "LIB_DIPLOME", "NIVEAU_DANS_LE_DIPLOME",
    "LIBELLE_DISCIPLINE_DIPLOME","CODE_SISE_DIPLOME","CODE_ETAPE",
    "LIBELLE_COURT_ETAPE","LIBELLE_LONG_ETAPE","NIVEAU_APRES_BAC",
    "LIBELLE_COURT_COMPOSANTE","CODE_COMPOSANTE",
    "LIBELLE_ACADEMIE_BAC","REGROUPEMENT_BAC","CONTINENT",
    "LIBELLE_REGIME","ANNEE_INSCRIPTION","NIEME_INSCRIPTION", "CODE_INDIVIDU"
  )

  /* On supprime les lignes qui ont des valeurs trop singulières dans
     leurs colonnes jusqu'à ce que chaque cellule apparaisse au moins
   k = 5 fois */

  /* Les colonnes à k-anonymiser */
  val k_anonymiser = Vector(
  "LIB_DIPLOME","NIVEAU_DANS_LE_DIPLOME",
   "LIBELLE_DISCIPLINE_DIPLOME","CODE_SISE_DIPLOME", "CODE_ETAPE",
    "LIBELLE_COURT_ETAPE", "LIBELLE_LONG_ETAPE",
    "NIVEAU_APRES_BAC",
   "LIBELLE_COURT_COMPOSANTE","CODE_COMPOSANTE",
   "LIBELLE_ACADEMIE_BAC","REGROUPEMENT_BAC","CONTINENT",
   "LIBELLE_REGIME","ANNEE_INSCRIPTION" //, "NIEME_INSCRIPTION" //, "CODE_INDIVIDU"
  )

 /* Anonymisation récursive */
  def anonymiser(d1: List[Vector[String]],
    k:Int,
    k_anonymiser: Vector[String]):List[Vector[String]] =
  {
    println("-- anonymiser")
    val rares = k_anonymiser.map( x => {
      val ens = elements_rares(x, k, (tout, d1))
      (x, ens.count(x=>true), ens)
    }).filter(_._2 > 0)
    rares.foreach(t => println((t._1, t._2)))

    if (rares.length > 0) {
      val d2 = rares.foldLeft(d1)((d, rare) =>
        supprimer(tout.indexOf(rare._1), d, rare._3))
      anonymiser(d2, k, k_anonymiser)
    }
    else d1
  }

  val d0 = projeter(tout).map(_.map(_.replaceAllLiterally(".","")))
  println("Nombre initial de lignes : " + d0.length)
  val annees0 = projeter(Vector("ANNEE_INSCRIPTION"), (tout, d0)).groupBy( x=> x ).mapValues(_.length).toList.sortBy(_._1(0))

  /* lancement de la suppression (récursive) */
  val d1 = anonymiser(d0, k, k_anonymiser)
  println("Nouveau nombre de lignes : " + d1.length)
  val supprimees0 = d0.length - d1.length
  println(s"Nombre de lignes supprimées : ${supprimees0}")
  val annees1 = projeter(Vector("ANNEE_INSCRIPTION"), (tout, d1)).groupBy( x => x ).mapValues(_.length).toList.sortBy(_._1(0))

  val annees10 = annees0.zip(annees1).map(x => Vector(x._1._1(0), x._1._2, x._2._2, x._1._2 - x._2._2).map(_.toString)).toList
  annees10.foreach(println)

  ecrire(Vector("ANNEE", "DONNEE_BRUTE", "DONNEE_ANONYME", "PERTE")::annees10, "up13_perte.csv")

  val total_individus = projeter(Vector("CODE_INDIVIDU")).map(_(0)).toSet.size
  println(s"Nombre total d'individus ${total_individus}")

  /* Section questionner les données 1 */
  if (calculer contains ("questions")) {
    /* Pourquoi le code etape ne détermine t'il pas le diplome, la
     composante etc ? */

    val etape = Vector(
      "CODE_ETAPE",
      "LIB_DIPLOME", "NIVEAU_DANS_LE_DIPLOME",
      "LIBELLE_DISCIPLINE_DIPLOME",
      "CODE_SISE_DIPLOME",
      "LIBELLE_COURT_ETAPE",
      // "LIBELLE_LONG_ETAPE",
      "NIVEAU_APRES_BAC",
      "LIBELLE_COURT_COMPOSANTE","CODE_COMPOSANTE"
        //, "LIBELLE_REGIME"
    )

    val paretape = projeter(etape, (tout, d1)).groupBy(_.apply(0)).mapValues(
    _.toVector.transpose.map(_.toSet).filter(_.count(x=>true) > 1)).toList

    val anomalies = paretape.filter(!_._2.isEmpty)

    println(paretape.length, anomalies.length)

    anomalies.foreach(println)
  }

  /* PRODUCTION DES JEUX DE DONNÉES */

  val cols_traces_avec_bac = Vector("CODE_INDIVIDU", "ANNEE_INSCRIPTION",
      "REGROUPEMENT_BAC", "LIBELLE_COURT_COMPOSANTE",
      "LIB_DIPLOME", "NIVEAU_APRES_BAC", "CODE_ETAPE")

  val cols_traces = Vector("CODE_INDIVIDU", "ANNEE_INSCRIPTION",
      "LIBELLE_COURT_COMPOSANTE",
      "LIB_DIPLOME", "NIVEAU_APRES_BAC", "CODE_ETAPE")


  /* Section calcul des traces exactes (1-traces) pour diffusion restreinte  */
  if (calculer contains ("1traces")) {
    ecriretraces(cols_traces_avec_bac, (tout, d0), "../DIFFUSION_RESTREINTE/up13_traces_bac.csv", 0)
    ecriretraces(cols_traces_avec_bac.dropRight(1), (tout, d0), "../DIFFUSION_RESTREINTE/up13_traces_bac_wt_etape.csv", 0)
    ecriretraces(cols_traces_avec_bac.filter(x => ((x != "LIB_DIPLOME") && (x != "CODE_ETAPE"))), (tout, d0), "../DIFFUSION_RESTREINTE/up13_traces_bac_wt_diplome.csv", 0)
    ecrirecursus(cols_traces_avec_bac, (tout, d0), "../DIFFUSION_RESTREINTE/up13_cursus_bac.csv", 0)
    ecrirecursus(cols_traces_avec_bac.dropRight(1), (tout, d0), "../DIFFUSION_RESTREINTE/up13_cursus_bac_wt_etape.csv", 0)
    ecrirecursus(cols_traces_avec_bac.filter(x => ((x != "LIB_DIPLOME") && (x != "CODE_ETAPE"))), (tout, d0), "../DIFFUSION_RESTREINTE/up13_cursus_bac_wt_diplome.csv", 0)
    ecriretraces(cols_traces, (tout, d0), "../DIFFUSION_RESTREINTE/up13_traces.csv", 0)
    ecriretraces(cols_traces.dropRight(1), (tout, d0), "../DIFFUSION_RESTREINTE/up13_traces_wt_etape.csv", 0)
    ecriretraces(cols_traces.filter(x => ((x != "LIB_DIPLOME") && (x != "CODE_ETAPE"))), (tout, d0), "../DIFFUSION_RESTREINTE/up13_traces_wt_diplome.csv", 0)
    ecrirecursus(cols_traces, (tout, d0), "../DIFFUSION_RESTREINTE/up13_cursus.csv", 0)
    ecrirecursus(cols_traces.dropRight(1), (tout, d0), "../DIFFUSION_RESTREINTE/up13_cursus_wt_etape.csv", 0)
    ecrirecursus(cols_traces.filter(x => ((x != "LIB_DIPLOME") && (x != "CODE_ETAPE"))), (tout, d0), "../DIFFUSION_RESTREINTE/up13_cursus_wt_diplome.csv", 0)
  }

  /* Section calcul des traces anonymisées (k-traces)  */
  if (calculer contains ("ktraces")) {
    val trbac = traces(cols_traces_avec_bac, (tout, d1))

    val cardtrbac = trbac.map(_._2)
    println(s"${trbac.length} groupes de traces avec bac")
    println(s"${cardtrbac.filter(_ < 10).sum} traces singulières en ${cardtrbac.filter(_ < 10).length} groupes")
    println(s"${cardtrbac.filter(_ >= 10).sum} traces anonymes en ${cardtrbac.filter(_ >= 10).length} groupes")

    val jeutrbac = trbac.map(
      x => Vector( (if (x._2 < 10) 1 else x._2).toString,
        (x._1(0)(0).toString :: x._1.map(_.drop(1).map(_.replaceAllLiterally(".","")).mkString(".")
        )).mkString(";")))
    val legendebac = Vector("NOMBRE", cols_traces_avec_bac.drop(3).map(_.replaceAllLiterally(".","")).mkString("."))

    ecrire(legendebac::jeutrbac, "up13_traces_bac.csv")
    /* TODO faire la fonction qui fait comme précedemment le bac à
       part */
    ecriretraces(cols_traces_avec_bac.dropRight(1), (tout, d1), "up13_traces_bac_wt_etape.csv", 10)
    ecriretraces(cols_traces_avec_bac.filter(x => ((x != "LIB_DIPLOME") && (x != "CODE_ETAPE"))), (tout, d1), "up13_traces_bac_wt_diplome.csv", 10)

    /* Cursus avec le bac… on met l'année du bac en plus ? */
    ecrirecursus(cols_traces_avec_bac, (tout, d1), "up13_cursus_bac.csv", 10)
    ecrirecursus(cols_traces_avec_bac.dropRight(1), (tout, d1), "up13_cursus_bac_wt_etape.csv", 10)
    ecrirecursus(cols_traces_avec_bac.filter(x => ((x != "LIB_DIPLOME") && (x != "CODE_ETAPE"))), (tout, d1), "up13_cursus_bac_wt_diplome.csv", 10)

    val tr = traces(cols_traces, (tout, d1))

    val cardtr = tr.map(_._2)
    println(s"${tr.length} groupes de traces sans bac")
    println(s"${cardtr.filter(_ < 10).sum} traces singulières en ${cardtr.filter(_ < 10).length} groupes")
    println(s"${cardtr.filter(_ >= 10).sum} traces anonymes en ${cardtr.filter(_ >= 10).length} groupes")

    ecriretraces(cols_traces, (tout, d1), "up13_traces.csv", 10)
    ecriretraces(cols_traces.dropRight(1), (tout, d1), "up13_traces_wt_etape.csv", 10)
    ecriretraces(cols_traces.filter(x => ((x != "LIB_DIPLOME") && (x != "CODE_ETAPE"))), (tout, d1), "up13_traces_wt_diplome.csv", 10)
    ecrirecursus(cols_traces, (tout, d1), "up13_cursus.csv", 10)
    ecrirecursus(cols_traces.dropRight(1), (tout, d1), "up13_cursus_wt_etape.csv", 10)
    ecrirecursus(cols_traces.filter(x => ((x != "LIB_DIPLOME") && (x != "CODE_ETAPE"))), (tout, d1), "up13_cursus_wt_diplome.csv", 10)
  }

  def anonymiser_jeu(colonnes: Vector[String], filename: String, k: Int) = {
    val jeu = projeter(colonnes, (tout, d1))
    val singularites = jeu.groupBy(x => x).mapValues(_.length).toList.filter(_._2 < k)
    val nb_lignes_singulieres = singularites.map(_._2).sum
    val ens_singularites = singularites.map(_._1).toSet
    val jeu_ok = jeu.filter( x => ! ens_singularites.contains(x))

    ecrire(colonnes::melanger(jeu_ok), filename)
    println(s"Fichier $filename créé")
    nb_lignes_singulieres
  }


  def chercher_jeu(colonnes: Vector[String], k: Int, pertemax: Int): Vector[String] = {
    def evaluer_jeu(colonnes: Vector[String], k: Int): Int = {
      val jeu = projeter(colonnes, (tout, d1))
      val singularites = jeu.groupBy(x => x).mapValues(_.length).toList.filter(_._2 < k)
      println(colonnes.last + " : " + singularites.map(_._2).sum)
      singularites.map(_._2).sum
    }
    val possibles = tout.filter(
      c => !colonnes.contains(c)).map(
      c => (evaluer_jeu(colonnes:+c, k), colonnes:+c)).sortBy(_._1)
    if ((!possibles.isEmpty) && ( possibles(0)._1 < pertemax)) {
      println("meilleur choix : " + possibles(0))
      chercher_jeu(possibles(0)._2, k, pertemax)
    }
    else colonnes
  }

  /* Section questionner données 2 */
  if(calculer contains ("questions")) {
    println("Traces_bac.csv contient " + anonymiser_jeu(Vector(
     "REGROUPEMENT_BAC",
     "LIBELLE_COURT_COMPOSANTE",
      "LIB_DIPLOME", "NIVEAU_APRES_BAC", "CODE_ETAPE"), "dave_nule.csv", 5) + " inscriptions singulières")

    println("Traces_bac.csv contient " + anonymiser_jeu(Vector(
     // "REGROUPEMENT_BAC",
     "LIBELLE_COURT_COMPOSANTE",
      "LIB_DIPLOME", "NIVEAU_APRES_BAC", "CODE_ETAPE"), "dave_nule.csv", 5) + " inscriptions singulières")


  println("Recherche d'un jeu contenant l'annee d'inscription")
  chercher_jeu(Vector("ANNEE_INSCRIPTION"), 5, d0.length / 20 - supprimees0)
/*  meilleur choix : (5361,Vector(ANNEE_INSCRIPTION,
    NIVEAU_DANS_LE_DIPLOME, LIBELLE_COURT_COMPOSANTE, CODE_COMPOSANTE,
    NIVEAU_APRES_BAC, LIB_DIPLOME, LIBELLE_REGIME,
    LIBELLE_DISCIPLINE_DIPLOME, CODE_SISE_DIPLOME, LIBELLE_LONG_ETAPE,
    CODE_ETAPE, LIBELLE_COURT_ETAPE)) */

  println("Recherche d'un jeu contenant bac et niveau d'étude par composante")
  chercher_jeu(Vector("REGROUPEMENT_BAC", "NIVEAU_APRES_BAC", "CODE_COMPOSANTE"), 5, d0.length / 20 - supprimees0)

/* Meilleur choix :  Vector(REGROUPEMENT_BAC, NIVEAU_APRES_BAC, CODE_COMPOSANTE,
   LIBELLE_COURT_COMPOSANTE, NIVEAU_DANS_LE_DIPLOME, LIB_DIPLOME,
   LIBELLE_REGIME, CONTINENT) */
  }


  /* Section projections */
  if (calculer contains ("projections")) {
    val jeu0 = Vector(
      "LIB_DIPLOME", "NIVEAU_DANS_LE_DIPLOME",
      "LIBELLE_DISCIPLINE_DIPLOME","CODE_SISE_DIPLOME", "CODE_ETAPE",
      "LIBELLE_COURT_ETAPE", "LIBELLE_LONG_ETAPE",
      "NIVEAU_APRES_BAC",
      "LIBELLE_COURT_COMPOSANTE","CODE_COMPOSANTE",
      "LIBELLE_ACADEMIE_BAC","REGROUPEMENT_BAC","CONTINENT",
      "LIBELLE_REGIME","ANNEE_INSCRIPTION" //, "NIEME_INSCRIPTION" //, "CODE_INDIVIDU"
    )
    val nb_lignes_supprimees0 = anonymiser_jeu(jeu0, "up13_anonyme.csv", k)
    println(s"lignes singulières supprimées du jeu : ${nb_lignes_supprimees0 + supprimees0}")

    println("-- Jeu 1")
    val jeu1 = Vector("CODE_ETAPE", "LIBELLE_COURT_ETAPE", "LIBELLE_LONG_ETAPE", "NIVEAU_APRES_BAC", "LIBELLE_COURT_COMPOSANTE", "LIB_DIPLOME", "LIBELLE_DISCIPLINE_DIPLOME", "CODE_SISE_DIPLOME", "NIVEAU_DANS_LE_DIPLOME")
    val nb_lignes_supprimees1 = anonymiser_jeu(jeu1, "up13_etapes.csv", k)
    println(s"lignes singulières supprimées du jeu : ${nb_lignes_supprimees1 + supprimees0}")

    println("-- Jeu 2 académie du bac niveau diplome")
    val jeu2 = Vector("LIBELLE_ACADEMIE_BAC", "NIVEAU_APRES_BAC", "NIVEAU_DANS_LE_DIPLOME", "CONTINENT", "LIBELLE_REGIME", "LIB_DIPLOME", "LIBELLE_COURT_COMPOSANTE")
    val nb_lignes_supprimees2 = anonymiser_jeu(jeu2, "up13_Academie.csv", k)
    println(s"lignes singulières supprimées du jeu :  ${nb_lignes_supprimees2 + supprimees0}")

    println("-- Jeu 3 continent niveau diplome")
    val jeu3 = Vector("REGROUPEMENT_BAC", "NIVEAU_APRES_BAC",  "LIBELLE_REGIME", "CONTINENT",  "LIBELLE_COURT_COMPOSANTE", "LIB_DIPLOME", "NIVEAU_DANS_LE_DIPLOME")
    val nb_lignes_supprimees3 = anonymiser_jeu(jeu3, "up13_Bac.csv", k)
    println(s"lignes singulières supprimées du jeu :  ${nb_lignes_supprimees3 + supprimees0}")

    println("-- Jeu 4 :  étapes par années")
    val  jeu4 = Vector("ANNEE_INSCRIPTION",
      "LIBELLE_COURT_COMPOSANTE", "NIVEAU_APRES_BAC",
      "LIB_DIPLOME",  "CODE_ETAPE")
    val nb_lignes_supprimees4 = anonymiser_jeu(jeu4, "up13_annees_etapes.csv", k)
    println(s"lignes singulières supprimées du jeu :  ${nb_lignes_supprimees4 + supprimees0}" )
  }
}
