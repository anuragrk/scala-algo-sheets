object Permutations {

  def main(args: Array[String]): Unit = {
    val name: String = "anurag"
     //stringPermutations(name)
    val output = permutations(name.toList)
    output.foreach(x => println(x.mkString))
  }

  def permutations[T](lst: List[T]): List[List[T]] = lst match {
    case Nil => List(Nil)
    case x :: xs => permutations(xs).flatMap { perm =>
      (0 to xs.length) map { num =>
        (perm.take(num)) ++ List(x) ++ (perm.drop(num))
      }
    }
  }

  def stringPermutations(str: String): Unit = {
    stringPermutations("", str)
  }

  private def stringPermutations(prefix: String, str: String): Unit = {
    val n = str.length
    if (str.isEmpty)  println( prefix)
    else {
       for( i<- 0 to n-1)
         stringPermutations(prefix + str.charAt(i), str.slice(0, i) + str.slice(i + 1, n))
    }
  }


}
