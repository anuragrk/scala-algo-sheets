/**
  *  The functions f : X → Y and g : Y → Z can be composed to yield a function which maps x in X to g(f(x)) in Z.
  *  "Function Composition" is applying one function to the results of another.
  */
object FunctionComposition {

  def f(x: Int) = x * x
  def g(x: Int) = 2*x + 1

  val fComposeG = f _ compose g _
  val gComposeF = g _ compose f _

  def main(args: Array[String]): Unit = {
    println("Enter value of x: ")
    val lines = io.Source.stdin.getLines()
    val x = lines.next().toInt
    println("f(g(x)) -> " + fComposeG(x))
    println("g(f(x)) -> " + gComposeF(x))
  }
}
