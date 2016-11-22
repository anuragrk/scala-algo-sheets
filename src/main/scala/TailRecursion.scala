
object TailRecursion {

  def main(args: Array[String]): Unit = {
    println("Enter number:")
    val lines = io.Source.stdin.getLines()
    val num = lines.next().toLong
    println("Factorial  -> " + computeFact(num))

  }
  //compute the factorial of a number using tail-recursion
  def computeFact(n: Long): Long ={
    def internalFact(accumulator: Long,number: Long): Long ={
      if(number == 0 || number == 1) accumulator
      else internalFact(number*accumulator,number-1)
    }
    internalFact(1,n)
  }

}
