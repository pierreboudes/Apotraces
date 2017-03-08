gpackage data

import java.io._

object Jpo { /* nouvelle donnée */
  def strtolist(s:String): Array[String] = {
    val res = s.split(";")


    val estadresse = """[\s]*[\d]*[\s]*(rue|RUE|Rue|avenue|Avenue|AVENUE|place|Place|PLACE|pl|Pl|PL|impasse|Impasse|IMPASSE|route|Route|ROUTE|boulevard|Boulevard|BOULEVARD|bd|Bd|BD)*""".r
    res
  }

  def mkadresses =  {
    val source = scala.io.Source.fromFile("adressesbrutes.csv", "utf-8")
    val lignes = source.getLines().toSet.toList
    source.close()
    lignes
    val file = new File("adressesU_v2.csv")
    val bw = new BufferedWriter(new FileWriter(file))
    lignes.foreach(s=>bw.write(s+"\n"))
    bw.close()
  }

  val file = new File("adresses.txt")
  val bw = new BufferedWriter(new FileWriter(file))
  bw.write("hello")
  bw.close()


    def mketapestree {/* pour d3js */
    val source = scala.io.Source.fromFile("etapes.csv", "utf-8")
    val lignes = source.getLines().toList.map(
      s => ("UP13."+ s.replaceAllLiterally(".", "")).split(";").toVector)
    val composantes = lignes.map(_.apply(0)).toSet.toList
    val diplomes = lignes.map(t => t(0) + "." + t(1)).toSet.toList
    val niveaux = lignes.map(t => t(0) + "." + t(1) + "." + t(2)).toSet.toList
    val etapes = (lignes.map(v => (v.patch(3,Vector(""),1).mkString("."), v.last))
      .groupBy(x=>x).mapValues(_.length).toList.filter(_._2 > 0) // filter off
      .map(t => (t._1._1, t._2)))

    val tout = (Vector("UP13,")++:
      composantes.map(_ + ",")++:
      diplomes.map(_ + ",")++:
      niveaux.map(_ + ",")++:
      etapes.map(s => s._1 + "," + s._2)).sorted

    import java.io._
    val file = new File("etapestree.csv")
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write("id,value\n")
    tout.foreach(s => bw.write(s + "\n"))
    bw.close
  }

  def mketapestreedotted {/* pour d3js */
    val source = scala.io.Source.fromFile("etapes.csv", "utf-8")
    val lignes = source.getLines().toList.map(
      s => ("UP13."+ s.replaceAllLiterally(".", "")).split(";").toVector)
    val composantes = lignes.map(_.apply(0)).toSet.toList
    val diplomes = lignes.map(t => t(0) + "." + t(1)).toSet.toList
    val niveaux = lignes.map(t => t(0) + "." + t(1) + "." + t(2)).toSet.toList
    val etapes = (lignes.map(v => (v.patch(3,Vector("·"),1).mkString("."), v.last))
      .groupBy(x=>x).mapValues(_.length).toList.filter(_._2 > 0) // filter off
      .map(t => (t._1._1 + "•" * (t._2 / 50), t._2)))

    val tout = (Vector("UP13,")++:
      composantes.map(_ + ",")++:
      diplomes.map(_ + ",")++:
      niveaux.map(_ + ",")++:
      etapes.map(s => s._1 + "," + s._2)).sorted

    import java.io._
    val file = new File("etapestree.csv")
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write("id,value\n")
    tout.foreach(s => bw.write(s + "\n"))
    bw.close
  }

   def mkniveauxtree {/* pour d3js */
    val source = scala.io.Source.fromFile("etapes.csv", "utf-8")
    val lignes = source.getLines().toList.map(
      s => ("UP13."+ s.replaceAllLiterally(".", "")).split(";").toVector)
    val composantes = lignes.map(_.apply(0)).toSet.toList
    val diplomes = lignes.map(t => t(0) + "." + t(1)).toSet.toList
    val niveaux = (lignes.map(v => (v(0) + "." + v(1) + "." + v(2), v(2)))
      .groupBy(x=>x).mapValues(_.length).toList.filter(_._2 > 0) // filter off
      .map(t => (t._1._1, t._2)))

    val tout = (Vector("UP13,")++:
      composantes.map(_ + ",")++:
      diplomes.map(_ + ",")++:
      niveaux.map(s => s._1 + "," + s._2)).sorted

    import java.io._
    val file = new File("niveauxtree.csv")
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write("id,value\n")
    tout.foreach(s => bw.write(s + "\n"))
    bw.close
   }

   def mkniveauxtreedotted {/* pour d3js */
    val source = scala.io.Source.fromFile("etapes.csv", "utf-8")
    val lignes = source.getLines().toList.map(
      s => ("UP13."+ s.replaceAllLiterally(".", "")).split(";").toVector)
    val composantes = lignes.map(_.apply(0)).toSet.toList
    val diplomes = lignes.map(t => t(0) + "." + t(1)).toSet.toList
    val niveaux = (lignes.map(v => (v(0) + "." + v(1) + "." + v(2) + "·", v(2)))
      .groupBy(x=>x).mapValues(_.length).toList.filter(_._2 > 0) // filter off
      .map(t => (t._1._1 + "•" * (t._2 / 50), t._2)))

    val tout = (Vector("UP13,")++:
      composantes.map(_ + ",")++:
      diplomes.map(_ + ",")++:
      niveaux.map(s => s._1 + "," + s._2)).sorted

    import java.io._
    val file = new File("niveauxtree.csv")
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write("id,value\n")
    tout.foreach(s => bw.write(s + "\n"))
    bw.close
   }

  def mkcomposantestreedotted {/* pour d3js */
    val source = scala.io.Source.fromFile("etapes.csv", "utf-8")
    val lignes = source.getLines().toList.map(
      s => ("UP13."+ s.replaceAllLiterally(".", "")).split(";").toVector)
    val composantes = lignes.map(_.apply(0)).toSet.toList
    val diplomes = (lignes.map(v => (v(0) + "." + v(1) + "·", v(1)))
      .groupBy(x=>x).mapValues(_.length).toList.filter(_._2 > 0) // filter off
      .map(t => (t._1._1 + "•" * (t._2 / 200), t._2)))

    val tout = (Vector("UP13,")++:
      composantes.map(_ + ",")++:
      diplomes.map(s => s._1 + "," + s._2)).sorted

    import java.io._
    val file = new File("composantesdiplomestree.csv")
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write("id,value\n")
    tout.foreach(s => bw.write(s + "\n"))
    bw.close
  }

   def mkcomposantestree {/* pour d3js */
    val source = scala.io.Source.fromFile("etapes.csv", "utf-8")
    val lignes = source.getLines().toList.map(
      s => ("UP13."+ s.replaceAllLiterally(".", "")).split(";").toVector)
    val composantes = lignes.map(_.apply(0)).toSet.toList
    val diplomes = (lignes.map(v => (v(0) + "." + v(1), v(1)))
      .groupBy(x=>x).mapValues(_.length).toList.filter(_._2 > 0) // filter off
      .map(t => (t._1._1, t._2)))

    val tout = (Vector("UP13,")++:
      composantes.map(_ + ",")++:
      diplomes.map(s => s._1 + "," + s._2)).sorted

    import java.io._
    val file = new File("composantesdiplomestree.csv")
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write("id,value\n")
    tout.foreach(s => bw.write(s + "\n"))
    bw.close
  }
}


object Apo {
  def strtotuple(s:String): (Int, String, Int, Int, Int) = {
    /* "(83456078, \"B9DM\", 111, 2006, 2)," -> tuple */
    val pattern = "\\(([0-9]+), \"([A-Z0-9]+)\", ([0-9]+), ([0-9]+), ([0-9]+)\\),".r
    val pattern(id, etape, version, annee, semestre) = s
    /* (83456078, "B9DM", 111, 2006, 2) */
    (id.toInt, etape, version.toInt, annee.toInt, semestre.toInt)
  }

  lazy val lignes =  {
    val source = scala.io.Source.fromFile("apotraces.txt", "utf-8")
    val lignes = source.getLines().toList.map(strtotuple)
    source.close()
    lignes
  }

  lazy val etapes = lignes.map(_._2).toSet

  lazy val idtraces = {
    lignes.groupBy(_._1) /* par id */
      .mapValues( seq => {
       val s = seq.sortBy(/* tri par année et semestre */
         t => (t._4, t._5)
       )
        val annee_debut = s(0)._4
        /* on ne conserve que l'année de début et une étape par
         année (la dernière étape) */
        val trace = s.map({case t => (t._4, t._2) }).toMap.toList.sortBy(_._1).map(_._2)

        (annee_debut,  trace)
      }
    )
    /* on supprime les traces qui ont une année de début trop ancienne */
      .filter({case (k, (annee_debut, trace)) => annee_debut > 2006})
      .mapValues(_._2) // on ne conserve que la liste des étapes */
  }

  lazy val traces = idtraces.values.toList.groupBy(x => x).mapValues(_.length).toList.sortBy(_._2)
}

object Etp {
  def cmpstrtotuple(s:String): (String, (String, String, Int)) = {
    /* 901; UFR; UFR DE DROIT; SCIENCES POLITIQUES…; UFR DSPS; UFR DSPS; 93430; 93079 */
    val pattern = "([0-9A-Z]+); ([0-9A-Z]+); ([^;]+); ([^;]+); ([^;]+); ([0-9]+); ([0-9]+)".r
    val pattern(composante, typecmp, x, y, lib, cp, cc) = s
    /* ("901", ("UFR", "UFR DSPS", 93079)) */
    (composante, (typecmp, lib, cc.toInt))
  }

  lazy val cmplignes =  {
    val source = scala.io.Source.fromFile("composantes.txt", "utf-8")
    val lignes = source.getLines().toList.map(cmpstrtotuple)
    source.close()
    lignes
  }

  lazy val composantes = cmplignes.toMap

  def cgestrtotuple(s:String): (String, String, String) = {
    /* 100; L5TID5; 902
       200; L5TID5; 902 */
    val pattern = "([0-9A-Z]+); ([0-9A-Z]+); ([0-9A-Z]+)".r
    val pattern(cge, etape, composante) = s
    (cge, etape, composante)
  }

  lazy val cgelignes =  {
    val source = scala.io.Source.fromFile("etape_cge.txt", "utf-8")
    val lignes = source.getLines().toList.map(cgestrtotuple)
    source.close()
    lignes
  }

  lazy val etape_composante = cgelignes.map({
    case (cge, etape, composante) => (etape, composante)
  }).toMap

  def etpstrtotuple(s:String): (String, (Int, String, String, String, String, (String, String, Int))) = {
    /* B3SVI; 1; L; Lic Sciences… Physio Annee 3; Lic SV pa. BCP an3 */
    val pattern = "([0-9A-Z]+); ([0-9]+); ([LMD]?); ([^;]+); ([^;]+)".r
    val pattern(etape, cycle, lmd, libellong, libelcourt) = s
    /* ("B3SVI", (1, "L", "Licence…", "Lic …", "903", ("UFR", "UFR
     DSPS", 93079)))*/
    val composante =  etape_composante.getOrElse(etape, "000")
    (etape, (cycle.toInt, lmd, libellong, libelcourt, composante,
      composantes.getOrElse(composante, ("XXX", "", 0))))
  }

  lazy val etplignes =  {
    val source = scala.io.Source.fromFile("etapes.txt", "utf-8")
    val lignes = source.getLines().toList.map(etpstrtotuple)
    source.close()
    lignes
  }

  def usecmp(etp: String): String = {
    /* remplace un code etape par un code forgé à partir de la */
    /* composante (moins précis, plus général, donc plus anonyme */

    val etape = etapes.getOrElse(etp, (0, "X", "", "", "000", ("UFO", "OVNI", 91100)))

    val forgedetp =  etape._5 + "_" + etape._1 + etape._2

    forgedetp
  }

  lazy val etapes = etplignes.toMap
}
