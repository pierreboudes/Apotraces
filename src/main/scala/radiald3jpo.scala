package data

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
