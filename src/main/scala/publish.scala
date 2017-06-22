package data

import java.io._

object Pub {
  // Lecture des données depuis le csv
  lazy val private_data =  {
    val source = scala.io.Source.fromFile("../NE_PAS_DIFFUSER/Basev2utf8_simple.csv", "utf-8")
    val tete :: corps = source.getLines.toList
    source.close()
    val entete = tete.split(";").toVector
    def parse_line(ligne:String): Vector[String]= ligne.split(";").toVector
     val data = corps.map(parse_line)
    (entete, data)
  }

  def projeter(xs: Vector[String],
    donnee: (Vector[String], List[Vector[String]]) = private_data): List[Vector[String]] = {

    val (entete, src) = donnee
    val indices = xs.map(entete.indexOf)

    src.map(x => indices.map(x.apply))
  }

  def melanger(lignes:List[Vector[String]]): List[Vector[String]] = {
    import scala.util.Random

    Random.shuffle(lignes)
  }

  def elements_rares(libel:String,
    seuil: Int = 5,
    donnee: (Vector[String], List[Vector[String]]) = private_data) =
    projeter(Vector(libel), donnee).groupBy(x => x).mapValues(_.length).toList.filter(_._2 < seuil).map(_._1.apply(0)).toSet

  def supprimer(offset:Int, donnee:List[Vector[String]], indesirables: Set[String]): List[Vector[String]] = {
    donnee.filter( x => !indesirables.contains(x(offset)))
  }

  def ecrire(lignes:List[Vector[String]], filename: String) {
    import java.io._
    val file = new File("../output/" + filename)
    val bw = new BufferedWriter(new FileWriter(file))
    lignes.foreach(x => bw.write(x.map(_.replaceAllLiterally(".","")).mkString(";") + "\n"))
    bw.close
  }
}

object Traces {
  /* cols : id (ou tout autre clé de regroupement), annee (ou tout
     autre pouvant être ordonné), et des attributs divers
     d: la donnée initiale
   */
  def idtraces(cols: Vector[String], d: (Vector[String], List[Vector[String]])) = {
    val lignes = Pub.projeter(cols, d)

    lignes.groupBy(_.apply(0)) /* par CODE_INDIVU */
      .mapValues( seq => {
        val s = seq.sortBy(_.apply(1)) // par ANNEE_INSCRIPTION
        val annee_debut = s(0)(1)
        /* on ne conserve qu'une étape par
         année (la dernière étape) */
        val trace = s.map( {
           case t => (t(1), t.drop(2))
        } ).toMap.toList.sortBy(_._1).map(_._2)
        trace
      })
  }
  // On regroupe les traces identiques
  def traces(cols: Vector[String], d: (Vector[String], List[Vector[String]])):  List[(List[Vector[String]], Int)] = idtraces(cols, d).values.toList.groupBy(x => x).mapValues(_.length).toList.sortBy(_._2)

  def ecriretraces(cols: Vector[String], d: (Vector[String], List[Vector[String]]), filename: String, seuil: Int) = {
    var log_perte = 0: Int;
    val jeu = traces(cols, d).map(
      x => Vector( (if (x._2 < seuil) {log_perte += x._2 - 1; 1} else x._2).toString,
        x._1.map(_.map(_.replaceAllLiterally(".","")).mkString(".")
        ).mkString(";")))
    val legende = Vector("NOMBRE", cols.drop(2).map(_.replaceAllLiterally(".","")).mkString("."))
    println(s"fichier ${filename} en création, \t perte = ${log_perte}")
    Pub.ecrire(legende::(Pub.melanger(jeu).sortBy(_.apply(0))), filename)
  }

  def idcursus(cols: Vector[String], d: (Vector[String], List[Vector[String]])): Map[String, Vector[Option[Vector[String]]]] = {
    val lignes = Pub.projeter(cols, d)

    /* porteuse : toutes les années présentes */
    val porteuse = Pub.projeter(Vector(cols(1)), d).toSet.toVector.map((x:Vector[String]) => x(0)).sorted

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
    val porteuse = Pub.projeter(Vector(cols(1)), d).toSet.toVector.map((x:Vector[String]) => x(0)).sorted

    var log_perte = 0: Int; // compteur de perte
    val jeu = cursus(cols, d).map(// par ligne
      x => Vector((if (x._2 < seuil) {log_perte += x._2 - 1; 1} else x._2).toString + ";" +
        x._1.map({
          ov => ov match {
            case None => ""
            case Some(v) => v.map(_.replaceAllLiterally(".","")).mkString(".")
          }
        }).mkString(";"))
    )
    val legende = Vector("NOMBRE", porteuse.mkString(";"), cols.drop(2).map(_.replaceAllLiterally(".","")).mkString("."))
    println(s"fichier ${filename} en création, \t perte = ${log_perte}")
    Pub.ecrire(legende::(Pub.melanger(jeu).sortBy(_.apply(0))), filename)
  }

}


object Garbage {
      def trouver_adresse(lib1: String, lib2: String, lib3: String) {
    val estadresse = """[\s]*[\d]*[\s]*(rue|RUE|Rue|avenue|Avenue|AVENUE|place|Place|PLACE|pl|Pl|PL|impasse|Impasse|IMPASSE|route|Route|ROUTE|boulevard|Boulevard|BOULEVARD|bd|Bd|BD)*""".r
      }

    def mkadresses =  {
    val source = scala.io.Source.fromFile("adressesbrutes.csv", "utf-8")
    val lignes = source.getLines().toSet.toList
    source.close()
    val file = new File("adressesU_v2.csv")
    val bw = new BufferedWriter(new FileWriter(file))
    lignes.foreach(s=>bw.write(s+"\n"))
    bw.close()
  }

  val file = new File("adresses.txt")
  val bw = new BufferedWriter(new FileWriter(file))
  bw.write("hello")
  bw.close()

}
