package data

import java.io._

object Pub {
  // Lecture des données depuis le csv
  lazy val private_data =  {
    val source = scala.io.Source.fromFile("../NE_PAS_DIFFUSER/Basev1utf8_simple.csv", "utf-8")
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
    lignes.foreach(x => bw.write(x.mkString(";") + "\n"))
    bw.close
  }
}

object Traces {
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
    val jeu = traces(cols, d).map(
      x => Vector( (if (x._2 < seuil) 1 else x._2).toString,
        x._1.map(_.map(_.replaceAllLiterally(".","")).mkString(".")
        ).mkString(";")))
    val legende = Vector("NOMBRE", cols.drop(2).map(_.replaceAllLiterally(".","")).mkString("."))
    Pub.ecrire(legende::jeu, filename)
  }
}