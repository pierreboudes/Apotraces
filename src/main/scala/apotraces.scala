package data

import java.io._


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

object MainApo {

//  Apo.traces.foreach(x => println(x._1.mkString("-") + " " + x._2))

  val traces1 = Apo.traces.filter(_._2 > 4)

  val pretraces2 = Apo.traces.filter(_._2 <= 4)

  val newtraces2 = pretraces2.map( x => (x._1.map(Etp.usecmp), x._2) ).groupBy(_._1).mapValues(_.map(_._2).sum).toList.sortBy(_._2)
  val traces2 = newtraces2.filter(_._2 > 4)
  val pretraces3 = newtraces2.filter(_._2 <= 4)

  val newtraces3 = pretraces3.map( x => (x._1.map(_.split('_')(1)), x._2)).groupBy(_._1).mapValues(_.map(_._2).sum).toList.sortBy(_._2)
  val traces3 = newtraces3.filter(_._2 > 4)
  val pretraces4 = newtraces3.filter(_._2 <= 4)

  val traces4 = pretraces4.map( x => (x._1.map(y => "X"), x._2)).groupBy(_._1).mapValues(_.map(_._2).sum).toList.sortBy(_._2)

  // affichage de  (traces1 ::: traces2 ::: traces3 ::: traces4)

/*  traces1.foreach(x => println(x._1.mkString("-") + " " + x._2))
  traces2.foreach(x => println(x._1.mkString("-") + " " + x._2))
  traces3.foreach(x => println(x._1.mkString("-") + " " + x._2))
 traces4.foreach(x => println(x._1.mkString("-") + " " + x._2)) */

  //  écriture des traces
  val file = new File("TRACES.txt")
  val bw = new BufferedWriter(new FileWriter(file))
  traces1.foreach(x => bw.write(x._1.mkString("-") + " " + x._2+"\n"))
  traces2.foreach(x => bw.write(x._1.mkString("-") + " " + x._2+"\n"))
  traces3.foreach(x => bw.write(x._1.mkString("-") + " " + x._2+"\n"))
  traces4.foreach(x => bw.write(x._1.mkString("-") + " " + x._2+"\n"))
  bw.close


  println("traces détaillées : " + traces1.map(_._2).sum)
  println("traces ébauchées : " + traces2.map(_._2).sum)
  println("traces réduites : " + traces3.map(_._2).sum)
  println("traces opaques : " + traces4.map(_._2).sum)
  println("Done!")
}
