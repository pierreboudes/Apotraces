package org.up13.apotraces

import java.io._
import scala.util.{Try, Success, Failure}

class Pub(tete: Vector[String], files: List[String]) {
  // Lecture des données depuis les csv
  def read_csv(filename: String): List[Vector[String]] = {
    val shortname = filename.split('/').last.split('.')(0)
    val source = scala.io.Source.fromFile(filename, "utf-8")
    val corps = source.getLines.toList
    source.close()
    def parse_line(ligne:String): Vector[String]= shortname+:ligne.split('|').toVector.map(
      _.replaceAllLiterally("."," ").replaceAllLiterally(";"," "))
    corps.map(parse_line)
  }

  lazy val private_data: (Vector[String], List[Vector[String]]) =
    (tete, files.map(read_csv).flatten)

  def projeter(xs: Vector[String],
    donnee: (Vector[String], List[Vector[String]]) = private_data): List[Vector[String]] = {
    val (entete, src) = donnee
    val indices = xs.map((s) => {
      val x = entete.indexOf(s)
      if (x == -1) {
        println("DEBUG colonne inconnue : " + s)
        -1
      }
      else
        x
    }
    ).filter((x) => x >= 0)
    src.map(x => indices.map(x.apply))
  }

  def melanger(lignes:List[Vector[String]]): List[Vector[String]] = {
    import scala.util.Random
    Random.shuffle(lignes)
  }

  /* trouver les valeurs rares d'une colonne libel (la rareté est le fait d'être en nombre inférieur à un seuil donné) */
  def elements_rares(libel:String,
    seuil: Int = 5,
    donnee: (Vector[String], List[Vector[String]]) = private_data) =
    projeter(Vector(libel), donnee).groupBy(x => x).mapValues(_.length).toList.filter(_._2 < seuil).map(_._1.apply(0)).toSet

  /* frequences */
  def frequences(libel: String, filename: String): Vector[String] = {
    val liste = projeter(Vector(libel)).groupBy((x) => x).mapValues(_.length).toList.sortBy(
      - _._2)
    val minimum = liste.last._2
    val moyenne: Double = liste.map(_._2).sum * 1.0 / liste.length
     val  vliste= liste.map(p => Vector(p._1(0).toString, p._2.toString))
    ecrire(Vector(libel, "nombre") :: vliste,  filename)
    Vector(libel, vliste.length.toString, "%.2f".format(moyenne), minimum.toString)
  }

  /* supprimer */
  def supprimer(offset:Int, donnee:List[Vector[String]], indesirables: Set[String]): List[Vector[String]] = {
    donnee.filter( x => !indesirables.contains(x(offset)))
  }

  def ecrire(lignes:List[Vector[String]], filename: String, escape: Boolean = false) {
    import java.io._
    val file = new File(filename)
    val bw = new BufferedWriter(new FileWriter(file))
    if (escape) {
      lignes.foreach(x => bw.write(x.map(_.replaceAllLiterally(";","")).mkString(";") + "\n"))
    } else {
      lignes.foreach(x => bw.write(x.mkString(";") + "\n"))
    }
    bw.close
  }

  /**************** Les traces ********************/

  /* cols : un vecteur de d'intitulés de colonnes avec :
     - en première position un ID (ou tout autre valeur de regroupement),
     - en seconde position une ANNEE (ou toute autre valeur pouvant être ordonnée),
     - puis à partir de la troisième position des attributs divers.
     La paire (ID, ANNEE) est normalement une clé

     d: la donnée de départ sous la forme d'une paire :
     (légende: Vector[String], data: List[Vector[String]])
   */
  def cursus_individuels(cols: Vector[String], d: (Vector[String], List[Vector[String]]),
    bac: Vector[String] = Vector("ANNEE_BAC", "LIBELLE_ACADEMIE_BAC", "REGROUPEMENT_BAC", "LIBELLE_COURT_BAC")): Map[String, List[Vector[String]]] = {
    /* sortie: Map(id -> List(Vector("Bac", …), Vector("primo","univ",L1", …), …), …)*/
    val lenbac = bac.length
    val lencols = cols.length
    val lignes = projeter(cols ++: bac, d)
    val annees = projeter(Vector(cols(1)), d).map(_.apply(0).toInt)
    val min_annee = annees.min
    val max_annee = annees.max
    def anon_annee(s:String, annee_bac: Int):String = s.toInt match {
      case `max_annee` => "en cours"
      case `annee_bac` => "primo"
      case `min_annee` => "tronqué"
      case _ => "fait"
    }
    lignes.groupBy(_.apply(0)  /* par ID */
    ).mapValues(seq => {
      val s = seq.sortBy(_.apply(1)) /* par ANNEE */
      val annee_bac = Try(s(0)(lencols).toInt).getOrElse(0)
      val insc_bac = s(0).drop(lencols + 1)
      val cursus = s.map((t) => (anon_annee(t(1), annee_bac), t.drop(2).dropRight(lenbac)))
      insc_bac +: cursus.map(t => t._1 +: t._2)
    })
  }

    // On regroupe les traces identiques
  def traces(cursus_id: Map[String, List[Vector[String]]]):  List[(List[Vector[String]], Int)] = cursus_id.values.toList.groupBy(x => x).mapValues(_.length).toList.sortBy(_._2)

  def ecriretraces(cursus_id: Map[String, List[Vector[String]]], filename: String, seuil: Int) = {
    var log_perte = 0: Int;
    val jeu = traces(cursus_id).map(
      x => Vector( (if (x._2 < seuil) {log_perte += x._2 - 1; 1} else x._2).toString,
        x._1.map(_.mkString(".")
        ).mkString(";")))
    val legende = Vector("NOMBRE", "INSCRIPTIONS")
    println(s"fichier ${filename} en création, \t perte = ${log_perte}")
    ecrire(legende::(melanger(jeu).sortBy(-1 * _.apply(0).toInt)), filename, false)
  }

    /* écriture des traces avec effacement des cardinalités < un seuil */
  def ecriretraces(cols: Vector[String], d: (Vector[String], List[Vector[String]]), filename: String, seuil: Int) = {
    var log_perte = 0: Int;
    val jeu = traces(cursus_individuels(cols, d)).map(
      x => Vector( (if (x._2 < seuil) {log_perte += x._2 - 1; 1} else x._2).toString,
        x._1.map(_.mkString(".")
        ).mkString(";")))
    val legende = Vector("NOMBRE", ("AN" +: cols.drop(2)).mkString("."))
    println(s"fichier ${filename} en création, \t perte = ${log_perte}")
    ecrire(legende::(melanger(jeu).sortBy(-1 * _.apply(0).toInt)), filename, false)
  }

  def idcursus(cols: Vector[String], d: (Vector[String], List[Vector[String]])): Map[String, Vector[Option[Vector[String]]]] = {
    val lignes = projeter(cols, d)

    /* porteuse : toutes les années présentes */
    val porteuse = projeter(Vector(cols(1)), d).toSet.toVector.map((x:Vector[String]) => x(0)).sorted

    lignes.groupBy(_.apply(0)).mapValues( seq => {
        val cursus = seq.map(t => (t(1), t.drop(2))).toMap
        porteuse.map(an => cursus.get(an)) // Some(data) ou None
      })
  }

  // On regroupe les cursus identiques
  def cursus(cols: Vector[String], d: (Vector[String], List[Vector[String]])):  List[(Vector[Option[Vector[String]]], Int)] =
    idcursus(cols, d).values.toList.groupBy(x => x).mapValues(_.length).toList.sortBy(_._2)

  /* Point d'entrée pour les cursus
   cols : id (ou tout autre clé de regroupement), annee (ou tout
   autre pouvant être ordonné), et des attributs divers
   d: la donnée initiale
   filename: ou écrire la donnée
   seuil: le seuil d'anonymisation en français ;)
   en dessous du seuil on ne compte qu'une seule ligne, au dessus on donne
   le vrai décompte.
   */
  def ecrirecursus(cols: Vector[String], d: (Vector[String], List[Vector[String]]), filename: String, seuil: Int) = {
    /* porteuse : toutes les années présentes (recalcul…) */
    val porteuse = projeter(Vector(cols(1)), d).toSet.toVector.map((x:Vector[String]) => x(0)).sorted

    var log_perte = 0: Int; // compteur de perte
    val jeu = cursus(cols, d).map(// par ligne
      x => Vector((if (x._2 < seuil) {log_perte += x._2 - 1; 1} else x._2).toString + ";" +
        x._1.map({
          ov => ov match {
            case None => ""
            case Some(v) => v.mkString(".")
          }
        }).mkString(";"))
    )
    val legende = Vector("NOMBRE", porteuse.mkString(";"), cols.drop(2).map(_.replaceAllLiterally(".","")).mkString("."))
    println(s"fichier ${filename} en création, \t perte = ${log_perte}")
    ecrire(legende::(melanger(jeu).sortBy(_.apply(0))), filename)
  }

}
