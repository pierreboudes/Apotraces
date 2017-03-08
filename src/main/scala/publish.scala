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


  def projeter(xs: Vector[String]) : List[Vector[String]] = {
    import scala.util.Random
    val (entete, src) = private_data
    val indices = xs.map(entete.indexOf)

    xs :: Random.shuffle(src.map(x => indices.map(x.apply)))
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

-  lazy val idtraces = {
-    lignes.groupBy("CODE_INDIVIDU") /* par id */
-      .mapValues( seq => {
-       val s = seq.sortBy(/* tri par année et semestre */
-         t => (t._4, t._5)
-       )
-        val annee_debut = s(0)._4
-        /* on ne conserve que l'année de début et une étape par
-         année (la dernière étape) */
-        val trace = s.map({case t => (t._4, t._2) }).toMap.toList.sortBy(_._1).map(_._2)
-
-        (annee_debut,  trace)
-      }
-    )
-    /* on supprime les traces qui ont une année de début trop ancienne */
-      .filter({case (k, (annee_debut, trace)) => annee_debut > 2006})
-      .mapValues(_._2) // on ne conserve que la liste des étapes */
-  }
-
-  lazy val traces = idtraces.values.toList.groupBy(x => x).mapValues(_.length).toList.sortBy(_._2)
-}
}
