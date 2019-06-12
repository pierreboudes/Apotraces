package org.up13.apotraces

import java.io._

object Main extends App {

  /* ********************** Paramètres ******************* */
  // TODO à déplacer dans un fichier de configuration
  // Les colonnes de données dans l'ordre
  /*
  val entete = "CODE_ETU CODE_POSTAL LIB_DIPLOME NIVEAU_DANS_LE_DIPLOME LIBELLE_DISCIPLINE_DIPLOME CODE_SISE_DIPLOME CODE_CYCLE CODE_ETAPE LIBELLE_COURT_ETAPE LIBELLE_LONG_ETAPE LIBELLE_COURT_COMPOSANTE LIBELLE_ACADEMIE_BAC ANNEE_BAC LIBELLE_CODE_POSTAL_ETB_BAC LIBELLE_COURT_BAC REGROUPEMENT_BAC ANNEE_NAISSANCE LIBELLE_REGIME CODE_REGIME CODE_PAYS_NATIONALITE LIBELLE_NATIONALITE CONTINENT ANNEE_INSCRIPTION NIVEAU".split(' ').toVector
   */

  /* Les colonnes des données sources */
  val entete = "CODE_ETU LIB_DIPLOME NIVEAU_DANS_LE_DIPLOME LIBELLE_DISCIPLINE_DIPLOME CODE_SISE_DIPLOME CODE_CYCLE CODE_ETAPE LIBELLE_COURT_ETAPE LIBELLE_LONG_ETAPE LIBELLE_COURT_COMPOSANTE LIBELLE_ACADEMIE_BAC ANNEE_BAC LIBELLE_COURT_BAC REGROUPEMENT_BAC ANNEE_NAISSANCE LIBELLE_REGIME CODE_REGIME ANNEE_INSCRIPTION REUSSITE".split(' ').toVector


  // Les sections actives
  var calculer: Set[String] = Set(
//    "questions",
    "suppr_singulières",
    "frequences",
    "simpletraces",
    "dictionnaire étapes",
    "traces codes etapes",
    "ktraces",
    "1traces",
    "cohortes pour suivi local",
    "projections",
    "1projections",
  )
  // constante k
  val k = 5
  // Repertoire d'écriture des jeux de données publics */
  val dirpub = "../output2018/"
  // Repertoire d'écriture des jeux de données internes */
  val dir = "../output2018/DIFFUSION_RESTREINTE/"

  /* ********************Fin paramètres ******************* */

  // affichage des sections
  def titre(s: String) = println("# Section : " + s)

  /* On ajoute les colonnes calculées à l'entete : l'université (le nom du fichier de données) et le niveau "bac + niveau" (bac + 1, bac + 2, etc.) calculé à partir du niveau dans le diplôme et du fichier de configuration conf/diplomes2niveaux.csv */

  val tete = "UNIV" +: entete :+ "NIVEAU"

  /* ****** on travaille sur le système de fichiers local ******* */
  val fsdirpub = new File(dirpub);
  val fsdir = new File(dir);
  if (!fsdirpub.exists) fsdirpub.mkdirs
  if (!fsdir.exists) fsdir.mkdirs
  if (!fsdirpub.exists || !fsdir.exists) {
    println("Désolé, je n'arrive pas à créer vos répertoires, j'annule les tâches.")
    calculer = Set()
  }
  /* les jeux de données */
  val datas = args.toList
  val pub = new Pub(tete, datas)
  import pub._

  /* ***** préparation des données ******** */
  /* Suppression du caractère '.' du jeu de données */
  val d0 = projeter(tete).map(_.map(_.replaceAllLiterally(".","")))

  /* Notre jeu de données initial*/
  var d1: List[Vector[String]] = d0
  var supprimees0 = 0
  val total_individus = projeter(Vector("CODE_ETU")).map(_(0)).toSet.size
  println(s"Nombre total d'individus ${total_individus}")

  if (calculer contains "frequences") {
    titre("fréquences")
    val freq = for {
      i <- Range(1,tete.length)
    } yield frequences(tete(i), dir + "Frequences " + tete(i) + ".csv")
    ecrire(Vector("Libellé", "nombre valeurs", "fréquence moyenne", "fréquence minimale") +: freq.toList, dir + "frequences.csv")
  }

  if (calculer contains "suppr_singulières") {
    /* On supprime les lignes qui ont des valeurs trop singulières dans
     leurs colonnes jusqu'à ce que chaque cellule apparaisse au moins
     k = 5 fois dans le jeu initial */

    /* Les colonnes à k-anonymiser */
    val colonnes_k_anonymiser = tete.tail.tail

    /* Anonymisation récursive */
    def anonymiser(d1: List[Vector[String]],
      k:Int,
      colonnes: Vector[String]):List[Vector[String]] =
    {
      println("-- anonymiser récursivement par suppression de lignes à valeurs singulières")
      val rares = colonnes.map( x => {
        val ens = elements_rares(x, k, (tete, d1))
        (x, ens.count(x=>true), ens)
      }).filter(_._2 > 0)

      rares.foreach(t => println((t._1, t._2)))

      if (rares.length > 0) {
        val d2 = rares.foldLeft(d1)((d, rare) =>
          supprimer(tete.indexOf(rare._1), d, rare._3))
        anonymiser(d2, k, colonnes)
      }
      else d1
    }

    /* suppression des valeurs singulières */
    println("Nombre initial de lignes : " + d0.length)
    val annees0 = projeter(Vector("ANNEE_INSCRIPTION"), (tete, d0)).groupBy( x=> x ).mapValues(_.length).toList.sortBy(_._1(0))

    /* lancement de la suppression (récursive) */
    d1 = anonymiser(d0, k, colonnes_k_anonymiser)

    /* */
      val total_individus2 = projeter(Vector("CODE_ETU"), (tete, d1)).map(_(0)).toSet.size
  println(s"Nouveau nombre d'individus : ${total_individus2} (${total_individus - total_individus2} supprimés)")
    /* on écrit un rapport sur les lignes perdues */
    println("Nouveau nombre de lignes : " + d1.length)
    supprimees0 = d0.length - d1.length
    println(s"Nombre de lignes supprimées : ${supprimees0}")
    val annees1 = projeter(Vector("ANNEE_INSCRIPTION"), (tete, d1)).groupBy( x => x ).mapValues(_.length).toList.sortBy(_._1(0))

    val annees10 = annees0.zip(annees1).map(x => Vector(x._1._1(0), x._1._2, x._2._2, x._1._2 - x._2._2).map(_.toString)).toList
    annees10.foreach(println)

    ecrire(Vector("ANNEE", "DONNEE_BRUTE", "DONNEE_ANONYME", "PERTE")::annees10, dir + "up13_perte.csv")
  }

  /* Section questionner les données 1 */
  if (calculer contains ("questions")) {
    titre("questions")
    println("Pourquoi le code etape ne détermine t'il pas le diplome, la composante etc ?")

    val etape = Vector(
      "CODE_ETAPE",
      "LIB_DIPLOME",
      "NIVEAU_DANS_LE_DIPLOME",
      "LIBELLE_DISCIPLINE_DIPLOME",
      "CODE_SISE_DIPLOME",
      "LIBELLE_COURT_ETAPE",
      "LIBELLE_COURT_COMPOSANTE"
    )

    val paretape = projeter(etape, (tete, d1)).groupBy(_.apply(0)).mapValues(
    _.toVector.transpose.map(_.toSet).filter(_.count(x=>true) > 1)).toList

    val anomalies = paretape.filter(!_._2.isEmpty)

    println(paretape.length, anomalies.length)

    anomalies.foreach(println)
  }

  /* PRODUCTION DES JEUX DE DONNÉES */

  /* Traces grossières */
  if (calculer contains "simpletraces") {
    titre("simpletraces")
    ecriretraces(cursus_individuels(
      Vector("CODE_ETU", "ANNEE_INSCRIPTION", "UNIV", "LIB_DIPLOME", "NIVEAU_DANS_LE_DIPLOME", "REUSSITE"),
      (tete, d0),
      Vector("ANNEE_BAC", "REGROUPEMENT_BAC")),
      dirpub + "up13_traces_tres_simples.csv", k)

    ecriretraces(cursus_individuels(
      Vector("CODE_ETU", "ANNEE_INSCRIPTION", "UNIV", "LIBELLE_COURT_COMPOSANTE", "LIB_DIPLOME", "NIVEAU_DANS_LE_DIPLOME"),
      (tete, d0),
      Vector("ANNEE_BAC", "LIBELLE_ACADEMIE_BAC", "REGROUPEMENT_BAC")),
      dirpub + "up13_traces_simples_localites.csv", k)

    ecriretraces(cursus_individuels(
      Vector("CODE_ETU", "ANNEE_INSCRIPTION", "UNIV", "LIBELLE_COURT_COMPOSANTE", "LIB_DIPLOME", "NIVEAU_DANS_LE_DIPLOME", "LIBELLE_LONG_ETAPE"),
      (tete, d0),
      Vector("ANNEE_BAC")),
      dirpub + "up13_traces_simples_sans_bac.csv", k)
  }

  /* dictionnaire des étapes */
  if (calculer contains ("dictionnaire étapes")) {
    titre("dictionnaire étapes")
    val libetapes = Vector("ANNEE_INSCRIPTION", "CODE_ETAPE", "LIBELLE_DISCIPLINE_DIPLOME", "NIVEAU_DANS_LE_DIPLOME", "LIBELLE_COURT_ETAPE", "LIBELLE_LONG_ETAPE");
    val etapes = projeter(libetapes).groupBy(_.apply(1)).values.map({(seq) =>
      val annees_inscription = seq.map(_.apply(0).toInt).sorted
      val minimum = annees_inscription.head.toString
      val maximum = annees_inscription.last.toString
      seq.head.drop(1) ++: Vector(minimum, maximum)}).toList;
    ecrire((libetapes.drop(1) ++: Vector("PREMIERE ANNEE", "DERNIERE ANNEE")) +: etapes, dir + "etapes.csv");
  }
  /* traces des codes étapes */
  if (calculer contains ("traces codes etapes")) {
    titre("traces codes etapes")
    val cursus_id_tr = cursus_individuels(
      Vector("CODE_ETU", "ANNEE_INSCRIPTION", "UNIV", "CODE_ETAPE"),
      (tete, d0),
      Vector("ANNEE_BAC", "REGROUPEMENT_BAC")
    )
    ecriretraces(cursus_id_tr, dirpub + "up13_traces_code_etapes.csv", k)
    ecriretraces( cursus_id_tr.mapValues(_.distinct),
      dirpub + "up13_traces_code_etapes_sans_redoublements.csv", k)
    /* pour calculer les parcours accessibles depuis le bac on ne regarde que les primos */
 //   ecriretraces( cursus_id_tr.mapValues(_.distinct).filter(_._2.apply(1).apply(0) == "primo")
 ecriretraces( cursus_id_tr.filter(_._2.apply(1).apply(0) == "primo").mapValues({
      p => p.head :: p.tail.map(_.tail).distinct
    }),
      dirpub + "up13_traces_code_etapes_primos_sans_redoublements.csv", k)
  }

  val cols_traces_avec_bac = Vector("CODE_ETU", "ANNEE_INSCRIPTION",
      "REGROUPEMENT_BAC", "LIBELLE_COURT_COMPOSANTE",
      "LIB_DIPLOME", "NIVEAU_DANS_LE_DIPLOME", "CODE_ETAPE", "LIBELLE_COURT_ETAPE")

  val cols_traces = Vector("CODE_ETU", "ANNEE_INSCRIPTION",
      "LIBELLE_COURT_COMPOSANTE",
    "LIB_DIPLOME", "NIVEAU_DANS_LE_DIPLOME", "CODE_ETAPE", "LIBELLE_COURT_ETAPE")

  val cols_traces_avec_bac_sans_etapes = Vector("CODE_ETU", "ANNEE_INSCRIPTION",
      "REGROUPEMENT_BAC", "LIBELLE_COURT_COMPOSANTE",
      "LIB_DIPLOME", "NIVEAU_DANS_LE_DIPLOME")

  val cols_traces_avec_bac_sans_diplomes = Vector("CODE_ETU", "ANNEE_INSCRIPTION",
      "REGROUPEMENT_BAC", "LIBELLE_COURT_COMPOSANTE",
      "LIB_DIPLOME", "NIVEAU_DANS_LE_DIPLOME")

  val cols_traces_naissance = Vector("CODE_ETU", "ANNEE_INSCRIPTION", "ANNEE_NAISSANCE",
    "LIBELLE_COURT_COMPOSANTE", // "NIVEAU",
      "LIB_DIPLOME", "NIVEAU_DANS_LE_DIPLOME", "CODE_ETAPE", "LIBELLE_COURT_ETAPE")

  val cols_traces_naissance_light = Vector("CODE_ETU", "ANNEE_INSCRIPTION", "ANNEE_NAISSANCE")

  val cols_traces_etapes = Vector("CODE_ETU", "ANNEE_INSCRIPTION", "NIVEAU", "CODE_ETAPE")

  /* Section calcul des traces exactes (1-traces) pour diffusion restreinte  */
   if (calculer contains ("1traces")) {
     titre("1traces")
     // TODO Pourquoi je me retrouve avec l'académie du Bac et le type du bac avec ces traces ?

     ecriretraces(cols_traces_naissance_light, (tete, d0), dir + "up13_traces_naissance_light.csv", 0)
     ecriretraces(cols_traces_naissance, (tete, d0), dir + "up13_traces_naissance.csv", 0)
     ecriretraces(cols_traces_avec_bac, (tete, d0), dir + "up13_traces_bac.csv", 0)
     ecriretraces(cols_traces_avec_bac_sans_etapes, (tete, d0), dir + "up13_traces_bac_sans_etape.csv", 0)
     ecriretraces(cols_traces_avec_bac.filter(x => ((x != "LIB_DIPLOME") && (x != "CODE_ETAPE"))), (tete, d0), dir + "up13_traces_bac_wt_diplome.csv", 0)
     ecrirecursus(cols_traces_avec_bac, (tete, d0), dir + "up13_cursus_bac.csv", 0)
     ecrirecursus(cols_traces_avec_bac.dropRight(1), (tete, d0), dir + "up13_cursus_bac_sans_etape.csv", 0)
     ecrirecursus(cols_traces_avec_bac.filter(x => ((x != "LIB_DIPLOME") && (x != "CODE_ETAPE"))), (tete, d0), dir + "up13_cursus_bac_wt_diplome.csv", 0)
     ecriretraces(cols_traces, (tete, d0), dir + "up13_traces.csv", 0)
     ecriretraces(cols_traces_etapes, (tete, d0), dir + "up13_traces_etapes.csv", 0, false)
     ecriretraces(cols_traces.dropRight(1), (tete, d0), dir + "up13_traces_sans_etape.csv", 0)
     ecriretraces(cols_traces.filter(x => ((x != "LIB_DIPLOME") && (x != "CODE_ETAPE"))), (tete, d0), dir + "up13_traces_wt_diplome.csv", 0)
     ecrirecursus(cols_traces, (tete, d0), dir + "up13_cursus.csv", 0)
     ecrirecursus(cols_traces_avec_bac_sans_etapes, (tete, d0), dir + "up13_cursus_sans_etape.csv", 0)
     ecrirecursus(cols_traces.filter(x => ((x != "LIB_DIPLOME") && (x != "CODE_ETAPE"))), (tete, d0), dir + "up13_cursus_sans_diplome.csv", 0)

   }

     if (calculer contains ("cohortes pour suivi local")) {
     titre("cohortes pour suivi local")

    val colsmin =  Vector("CODE_ETU", "ANNEE_INSCRIPTION", "NIVEAU", "REUSSITE")

    ecrirecursus(
      colsmin,
      (tete, d0),
      dir + "up13_cohortes_hyper_simples.csv",
      0
    )

    ecrirefilteredcursus(
      colsmin,
      (tete, d0),
      dir + "up13_cohortes_hyper_simples-Licence.csv",
      0,
      Vector("LIB_DIPLOME"),
      { (c) => c.exists(_.exists( (d) => d.last == "Licence" )) }
    )

    ecrirefilteredcursus(
      colsmin,
      (tete, d0),
      dir + "up13_cohortes_hyper_simples-LicPro.csv",
      0,
      Vector("LIB_DIPLOME"),
      { (c) => c.exists(_.exists( (d) => d.last == "Lic Pro" )) }
    )

    ecrirefilteredcursus(
      colsmin,
      (tete, d0),
      dir + "up13_cohortes_hyper_simples-DUT.csv",
      0,
      Vector("LIB_DIPLOME"),
      { (c) => c.exists(_.exists( (d) => d.last == "DUT" )) }
    )

    ecrirefilteredcursus(
      colsmin,
      (tete, d0),
      dir + "up13_cohortes_hyper_simples-Master.csv",
      0,
      Vector("LIB_DIPLOME"),
      { (c) => c.exists(_.exists( (d) => d.last.startsWith("Master") )) }
    )

    ecrirefilteredcursus(
      colsmin,
      (tete, d0),
      dir + "up13_cohortes_hyper_simples-LicenceMaster.csv",
      0,
      Vector("LIB_DIPLOME"),
      { (c) => c.exists(_.exists( (d) => d.last.startsWith("Master") || d.last.startsWith("Licence") )) }
    )

     ecrirefilteredcursus(
      colsmin,
      (tete, d0),
      dir + "up13_cohortes_hyper_simples-Doctorat.csv",
      0,
      Vector("LIB_DIPLOME"),
      { (c) => c.exists(_.exists( (d) => d.last.startsWith("Doctorat") )) }
     )

     ecrirefilteredcursus(
      colsmin,
      (tete, d0),
      dir + "up13_cohortes_hyper_simples-Ingénieur.csv",
      0,
      Vector("LIB_DIPLOME"),
      { (c) => c.exists(_.exists( (d) => d.last.startsWith("Ing") )) }
     )

     /*
    ecrirefilteredcursus(
      colsmin,
      (tete, d0),
      dir + "up13_cohortes_hyper_simples-europe.csv",
      0,
      Vector("CONTINENT"),
      { (c) => c.exists(_.exists( (d) => d.last.startsWith("EUROPE") )) }
    )

    ecrirefilteredcursus(
      colsmin,
      (tete, d0),
      dir + "up13_cohortes_hyper_simples-hors-europe.csv",
      0,
      Vector("CONTINENT"),
      { (c) => c.exists(_.exists( (d) => !d.last.startsWith("EUROPE") )) }
    )
      */
    ecrirefilteredcursus(
      colsmin,
      (tete, d0),
      dir + "up13_cohortes_hyper_simples-naissance-1991.csv",
      0,
      Vector("ANNEE_NAISSANCE"),
      { (c) => c.exists(_.exists( (d) => d.last.startsWith("1991") )) }
    )

     ecrirefilteredcursus(
      colsmin,
      (tete, d0),
      dir + "up13_cohortes_hyper_simples-naissance-1996.csv",
      0,
      Vector("ANNEE_NAISSANCE"),
      { (c) => c.exists(_.exists( (d) => d.last.startsWith("1996") )) }
    )


    ecrirefilteredcursus(
      colsmin,
      (tete, d0),
      dir + "up13_cohortes_hyper_simples-bac.csv",
      0,
      Vector("REGROUPEMENT_BAC"),
      { (c) => c.exists(_.exists( (d) => d.last.startsWith("Bacs") )) }
    )

     ecrirefilteredcursus(
      colsmin,
      (tete, d0),
      dir + "up13_cohortes_hyper_simples-sans-bac.csv",
      0,
      Vector("REGROUPEMENT_BAC"),
      { (c) => c.exists(_.exists( (d) => !d.last.startsWith("Bacs") )) }
    )


     Vector("UFR SMBH", "UFR LLSHS", "UFR DSPS", "IG", "IUTSD", "UFR SEG", "IUTV", "IUTB", "UFR COM").foreach( (comp) => {
       ecrirecursus(
         colsmin,
         (tete, d0.filter(_.apply(tete.indexOf("LIBELLE_COURT_COMPOSANTE")) == comp ) ),
         dir + s"up13_cohortes_hyper_simples-${comp}.csv",
         0
       )

       ecrirefilteredcursus(
         colsmin,
         (tete, d0),
         dir + s"up13_cohortes_hyper_simples-passage-par-${comp}.csv",
         0,
         Vector("LIBELLE_COURT_COMPOSANTE"),
         { (c) => c.exists(_.exists( (d) => d.last == comp )) }
       )

       ecrirefilteredcursus(
         colsmin,
         (tete, d0),
         dir + s"up13_cohortes_hyper_simples-${comp}-uniquement.csv",
         0,
         Vector("LIBELLE_COURT_COMPOSANTE"),
         { (c) => c.forall(_.forall( (d) => d.last == comp )) }
       )
     })

     ecrirefilteredcursus(
      colsmin,
      (tete, d0),
      dir + "up13_cohortes_hyper_simples-2013.csv",
      0,
      Vector("ANNEE_INSCRIPTION"),
      { (c) => c.exists(_.exists( (d) => d.last.startsWith("2013") )) }
    )

     ecrirefilteredcursus(
      colsmin,
      (tete, d0),
      dir + "up13_cohortes_hyper_simples-Bac-2010.csv",
      0,
      Vector("ANNEE_BAC"),
      { (c) => c.exists(_.exists( (d) => d.last.startsWith("2010") )) }
    )

     val ensEtapesAnnees = Set(
       ("2009", "G1INF"),
       ("2009", "G1MAT"),
       ("2009", "G1MIE"),
       ("2009", "G1PC"),
       ("2009", "G1TRC"),
       ("2009", "G1TRC9"),
       ("2009", "E2MIE"),
       ("2009", "G2INF"),
       ("2009", "G2PC"),
       ("2009", "G2SPI"),
       ("2009", "G5PLS"),
       ("2009", "G2CP"),
       ("2010", "G3SI2"),
       ("2010", "G1INF"),
       ("2010", "G1MAT"),
       ("2010", "G1MIE"),
       ("2010", "G1PC"),
       ("2010", "G1TRC"),
       ("2010", "G1TRC9"),
       ("2010", "E2MIE"),
       ("2010", "G2INF"),
       ("2010", "G2PC"),
       ("2010", "G2SPI"),
       ("2010", "G5PLS"),
       ("2011", "G3SI2"),
       ("2011", "G1INF"),
       ("2011", "G1MAT"),
       ("2011", "G1MIE"),
       ("2011", "G1PC"),
       ("2011", "G1TRC"),
       ("2011", "G1TRC9"),
       ("2011", "E2MIE"),
       ("2011", "G2INF"),
       ("2011", "G2PC"),
       ("2011", "G2SPI"),
       ("2011", "G5PLS")
     )
     ecrirefilteredcursus(
      colsmin,
      (tete, d0),
      dir + "up13_cohortes_hyper_simples-enseignant.csv",
      0,
       Vector("ANNEE_INSCRIPTION", "CODE_ETAPE"),
       { (c) => c.exists(_.exists( (d) => {
         val an = d(d.length - 2)
         val etape = d(d.length - 1)
         ensEtapesAnnees contains ((an, etape))
       } )) }
    )

  }

  /* Section calcul des traces anonymisées (k-traces)  */
  if (calculer contains ("ktraces")) {
    titre("ktraces")
    val trbac = traces(cursus_individuels(cols_traces_avec_bac, (tete, d1)))

    val cardtrbac = trbac.map(_._2)
    println(s"${trbac.length} groupes de traces avec bac")
    println(s"${cardtrbac.filter(_ < 10).sum} traces singulières en ${cardtrbac.filter(_ < 10).length} groupes")
    println(s"${cardtrbac.filter(_ >= 10).sum} traces anonymes en ${cardtrbac.filter(_ >= 10).length} groupes")

    val jeutrbac = trbac.map(
      x => Vector( (if (x._2 < 10) 1 else x._2).toString,
        (x._1(0)(0).toString :: x._1.map(_.drop(1).map(_.replaceAllLiterally(".","")).mkString(".")
        )).mkString(";")))
    val legendebac = Vector("NOMBRE", cols_traces_avec_bac.drop(3).map(_.replaceAllLiterally(".","")).mkString("."))

    ecrire(legendebac::jeutrbac, dirpub + "up13_traces_bac.csv")
    /* TODO faire la fonction qui fait comme précedemment le bac à
       part */
    ecriretraces(cols_traces_avec_bac.dropRight(1), (tete, d1), dirpub + "up13_traces_bac_wt_etape.csv", 10)
    ecriretraces(cols_traces_avec_bac.filter(x => ((x != "LIB_DIPLOME") && (x != "CODE_ETAPE"))), (tete, d1), dirpub + "up13_traces_bac_wt_diplome.csv", 10)

    /* Cursus avec le bac… on met l'année du bac en plus ? */
    ecrirecursus(cols_traces_avec_bac, (tete, d1), dirpub + "up13_cursus_bac.csv", 10)
    ecrirecursus(cols_traces_avec_bac.dropRight(1), (tete, d1), dirpub + "up13_cursus_bac_wt_etape.csv", 10)
    ecrirecursus(cols_traces_avec_bac.filter(x => ((x != "LIB_DIPLOME") && (x != "CODE_ETAPE"))), (tete, d1), dirpub + "up13_cursus_bac_wt_diplome.csv", 10)

    val tr = traces(cursus_individuels(cols_traces, (tete, d1)))

    val cardtr = tr.map(_._2)
    println(s"${tr.length} groupes de traces sans bac")
    println(s"${cardtr.filter(_ < 10).sum} traces singulières en ${cardtr.filter(_ < 10).length} groupes")
    println(s"${cardtr.filter(_ >= 10).sum} traces anonymes en ${cardtr.filter(_ >= 10).length} groupes")

    ecriretraces(cols_traces, (tete, d1), dirpub + "up13_traces.csv", 10)
    ecriretraces(cols_traces.dropRight(1), (tete, d1), dirpub + "up13_traces_wt_etape.csv", 10)
    ecriretraces(cols_traces.filter(x => ((x != "LIB_DIPLOME") && (x != "CODE_ETAPE"))), (tete, d1), dirpub + "up13_traces_wt_diplome.csv", 10)
    ecrirecursus(cols_traces, (tete, d1), dirpub + "up13_cursus.csv", 10)
    ecrirecursus(cols_traces_avec_bac_sans_etapes, (tete, d1), dirpub + "up13_cursus_wt_etape.csv", 10)
    ecrirecursus(cols_traces.filter(x => ((x != "LIB_DIPLOME") && (x != "CODE_ETAPE"))), (tete, d1), dirpub + "up13_cursus_wt_diplome.csv", 10)
  }


  def anonymiser_jeu(colonnes: Vector[String], filename: String, k: Int) = {
    val jeu = projeter(colonnes, (tete, d1))
    val singularites = jeu.groupBy(x => x).mapValues(_.length).toList.filter(_._2 < k)
    val nb_lignes_singulieres = singularites.map(_._2).sum
    val ens_singularites = singularites.map(_._1).toSet
    val jeu_ok = jeu.filter( x => ! ens_singularites.contains(x))

    ecrire(colonnes::melanger(jeu_ok), dirpub + filename)
    println(s"Fichier $filename créé")
    nb_lignes_singulieres
  }


  def chercher_jeu(colonnes: Vector[String], k: Int, pertemax: Int): Vector[String] = {
    def evaluer_jeu(colonnes: Vector[String], k: Int): Int = {
      val jeu = projeter(colonnes, (tete, d1))
      val singularites = jeu.groupBy(x => x).mapValues(_.length).toList.filter(_._2 < k)
      println(colonnes.last + " : " + singularites.map(_._2).sum)
      singularites.map(_._2).sum
    }
    val possibles = tete.filter(
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
    NIVEAU_DANS_LE_DIPLOME, LIBELLE_COURT_COMPOSANTE,
    NIVEAU_APRES_BAC, LIB_DIPLOME, LIBELLE_REGIME,
    LIBELLE_DISCIPLINE_DIPLOME, CODE_SISE_DIPLOME, LIBELLE_LONG_ETAPE,
    CODE_ETAPE, LIBELLE_COURT_ETAPE)) */

  println("Recherche d'un jeu contenant bac et niveau d'étude par composante")
  chercher_jeu(Vector("REGROUPEMENT_BAC", "LIBELLE_COURT_COMPOSANTE"), 5, d0.length / 20 - supprimees0)

/* Meilleur choix :  Vector(REGROUPEMENT_BAC, NIVEAU_APRES_BAC,
   LIBELLE_COURT_COMPOSANTE, NIVEAU_DANS_LE_DIPLOME, LIB_DIPLOME,
   LIBELLE_REGIME) */
  }


  /* Section projections */
  if (calculer contains ("projections")) {
    val jeu0 = Vector(
      "LIB_DIPLOME", "NIVEAU_DANS_LE_DIPLOME",
      "LIBELLE_DISCIPLINE_DIPLOME","CODE_SISE_DIPLOME", "CODE_ETAPE",
      "LIBELLE_COURT_ETAPE", "LIBELLE_LONG_ETAPE",
      "LIBELLE_COURT_COMPOSANTE",
      "LIBELLE_ACADEMIE_BAC","REGROUPEMENT_BAC",
      "LIBELLE_REGIME","ANNEE_INSCRIPTION"
    )
    val nb_lignes_supprimees0 = anonymiser_jeu(jeu0, "up13_anonyme.csv", k)
    println(s"lignes singulières supprimées du jeu : ${nb_lignes_supprimees0 + supprimees0}")

    println("-- Jeu 1")
    val jeu1 = Vector("CODE_ETAPE", "LIBELLE_COURT_ETAPE", "LIBELLE_LONG_ETAPE", "LIBELLE_COURT_COMPOSANTE", "LIB_DIPLOME", "LIBELLE_DISCIPLINE_DIPLOME", "CODE_SISE_DIPLOME", "NIVEAU_DANS_LE_DIPLOME")
    val nb_lignes_supprimees1 = anonymiser_jeu(jeu1, "up13_etapes.csv", k)
    println(s"lignes singulières supprimées du jeu : ${nb_lignes_supprimees1 + supprimees0}")

    println("-- Jeu 2 académie du bac niveau diplome")
    val jeu2 = Vector("LIBELLE_ACADEMIE_BAC", "NIVEAU_DANS_LE_DIPLOME", "LIBELLE_REGIME", "LIB_DIPLOME", "LIBELLE_COURT_COMPOSANTE")
    val nb_lignes_supprimees2 = anonymiser_jeu(jeu2, "up13_Academie.csv", k)
    println(s"lignes singulières supprimées du jeu :  ${nb_lignes_supprimees2 + supprimees0}")

    println("-- Jeu 3 continent niveau diplome")
    val jeu3 = Vector("REGROUPEMENT_BAC",  "LIBELLE_REGIME", "LIBELLE_COURT_COMPOSANTE", "LIB_DIPLOME", "NIVEAU_DANS_LE_DIPLOME")
    val nb_lignes_supprimees3 = anonymiser_jeu(jeu3, "up13_Bac.csv", k)
    println(s"lignes singulières supprimées du jeu :  ${nb_lignes_supprimees3 + supprimees0}")

    println("-- Jeu 4 :  étapes par années")
    val  jeu4 = Vector("ANNEE_INSCRIPTION",
      "LIBELLE_COURT_COMPOSANTE",
      "LIB_DIPLOME",  "CODE_ETAPE")
    val nb_lignes_supprimees4 = anonymiser_jeu(jeu4, "up13_annees_etapes.csv", k)
    println(s"lignes singulières supprimées du jeu :  ${nb_lignes_supprimees4 + supprimees0}" )
  }

  /* jeux de données internes */

  if (calculer contains "1projections") {
    val  jeu5 = Vector("ANNEE_INSCRIPTION", "CODE_ETAPE", "REGROUPEMENT_BAC",
      "LIBELLE_COURT_COMPOSANTE"
      )
    val jeu5_data = projeter(jeu5, (tete, d0)).groupBy(x => x).mapValues(_.length).toList.map(x => x._2.toString +: x._1).sortBy(x => (x(1) + x(2)))

    val filename5 = dir + "up13_effectif_annee_etape_bac_continent.csv"
    ecrire(("NB"+:jeu5)::(jeu5_data), filename5)
    println(s"Fichier $filename5 créé")

    val  jeu6 = Vector("ANNEE_INSCRIPTION", "CODE_ETAPE", "LIBELLE_LONG_ETAPE", "LIBELLE_COURT_COMPOSANTE")
    val jeu6_data = projeter(jeu6, (tete, d0)).groupBy(x => x.slice(0,2)).mapValues(x =>(x.length, Vector(x.last(2),x.last(3)))).toList.map(x => x._2._1.toString +: x._1 ++: x._2._2).sortBy(x => (x(2) + " " + x(1)))

    val filename6 = dir + "up13_effectif_annee_etape.csv"
    ecrire(("NB"+:jeu6)::(jeu6_data), filename6)
    println(s"Fichier $filename6 créé")

    val  jeu7 = Vector("ANNEE_INSCRIPTION",
      "LIBELLE_COURT_COMPOSANTE",
      "CODE_ETAPE", "LIBELLE_LONG_ETAPE")
    val nb_lignes_supprimees6 = anonymiser_jeu(jeu7, dir + "up13_etapes.csv", 1)
    println(s"lignes singulières supprimées du jeu :  ${nb_lignes_supprimees6 + supprimees0}" )
  }
}
