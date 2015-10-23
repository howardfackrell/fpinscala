/**
 * Created by howard.fackrell on 10/20/15.
 */
object Howard {

  def main(args: Array[String]) : Unit = {
    val a = List("a", "b", "c")

    println(a.foldLeft("-"){
      (b, s) => b + s
    })
  }

}
