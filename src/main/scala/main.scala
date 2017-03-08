package org.toto.helloworld

import data.Apo
import data.Etp
import java.io._

object Main extends App {

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
