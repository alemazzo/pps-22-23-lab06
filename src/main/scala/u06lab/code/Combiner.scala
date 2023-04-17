package u06lab.code

/** 1) Implement trait Functions with an object FunctionsImpl such that the code in TryFunctions works correctly. */

trait Functions:
  def sum(a: List[Double]): Double

  def concat(a: Seq[String]): String

  def max(a: List[Int]): Int // gives Int.MinValue if a is empty

object FunctionsImpl extends Functions:
  override def sum(a: List[Double]): Double = combine(a)
  override def concat(a: Seq[String]): String = combine(a)
  override def max(a: List[Int]): Int = combine(a)

  private given Combiner[Double] = Combiner(0.0, _ + _)
  private given Combiner[String] = Combiner("", _ + _)
  private given Combiner[Int] = Combiner(Int.MinValue, Integer.max)

  private def combine[A: Combiner](a: Seq[A]): A = a match
    case Nil => summon[Combiner[A]].unit
    case x :: xs => summon[Combiner[A]].combine(x, combine(xs))

trait Combiner[A]:
  def unit: A
  def combine(a: A, b: A): A

object Combiner:
  def apply[A](a: A, combiner: (A, A) => A): Combiner[A] =
    new Combiner[A]:
      override def unit: A = a
      override def combine(a: A, b: A): A = combiner(a, b)

@main def checkFunctions(): Unit =
  val f: Functions = FunctionsImpl
  println(f.sum(List(10.0, 20.0, 30.1))) // 60.1
  println(f.sum(List())) // 0.0
  println(f.concat(Seq("a", "b", "c"))) // abc
  println(f.concat(Seq())) // ""
  println(f.max(List(-10, 3, -5, 0))) // 3
  println(f.max(List())) // -2147483648


