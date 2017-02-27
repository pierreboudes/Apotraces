package data


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
