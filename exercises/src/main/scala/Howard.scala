/**
 * Created by howard.fackrell on 10/20/15.
 */
object Howard {

  def main(args: Array[String]) : Unit = {


    val item = "this is my favorite: \"http://www.youtube.com/abcdefg\""

    val pos = item.indexOf("youtube.com")

    val begin = item.lastIndexOf("\"", pos)
    val end = item.indexOf("\"", pos+1)
    val url = item.substring(begin+1, end)

    println(s"pos:$pos begin:$begin end:$end url:$url")



//    val pat = """(atg([atg][atg][atg])*?(taa|tga|taa))""".r
    val pat = """(?=(ATG((?!ATG)[ATGC]{3})*(TAA|TAG|TGA)))""".r

    val it = pat.findAllMatchIn("""ATGAAATGAAAA""".toUpperCase()).mkString("\n")
//    val it = pat.findAllMatchIn("""ccatgccctaataaatgtctgtaatgtaga""".toUpperCase()).mkString("\n")
//    val it = pat.findAllMatchIn("""CATGTAATAGATGAATGACTGATAGATATGCTTGTATGCTATGAAAATGTGAAATGACCCA""".toLowerCase()).mkString("\n")

    println(it)

  }

}
