package u04lab
import u03.Sequences.*
import Sequence.*
import u03.Optionals.*

/*  Exercise 5:
 *  - Generalise by ad-hoc polymorphism logAll, such that:
 *  -- it can be called on Sequences but also on Optional, or others...
 *  -- it does not necessarily call log, but any function with analogous type
 *  - Hint: introduce a type class Traversable[T[_]]], capturing the ability of calling a
 *    "consumer function" on all elements (with type A) of a datastructure T[A]
 *    Note Traversable is a 2-kinded trait (similar to Filterable, or Monad)
 *  - Write givens for Traversable[Optional] and Traversable[Sequence]
 *  - Show you can use the generalisation of logAll to:
 *  -- log all elements of an Optional, or of a Traversable
 *  -- println(_) all elements of an Optional, or of a Traversable
 */

object Ex5Traversable:

  def log[A](a: A): Unit = println("The next element is: " + a)

  def logAll[A](seq: Sequence[A]): Unit = seq match
    case Cons(h, t) => log(h); logAll(t)
    case _          => ()

  trait Traversable[T[_]]:
    extension [A](seq: T[A]) def forEach(f: A => Unit): Unit

  given Traversable[Sequence] with
    extension [A](seq: Sequence[A])
      def forEach(f: A => Unit): Unit = seq match
        case Cons(h, t) => f(h); t.forEach(f)
        case _          => ()

  given Traversable[Optional] with
    extension [A](opt: Optional[A])
      def forEach(f: A => Unit): Unit = opt match
        case Optional.Just(a) => f(a)
        case _                => ()

  def traversableLogAll[A](t: Sequence[A]): Unit =
    t.forEach(log)

  def traversablePrintLogAll[A](t: Optional[A]): Unit =
    t.forEach(println)

@main def main: Unit =
  val seq = Cons(10, Cons(20, Cons(30, Nil())))
  Ex5Traversable.logAll(seq)
  val jus = Optional.Just(10)
  Ex5Traversable.traversableLogAll(seq)
  val nil = Optional.Empty()
  Ex5Traversable.traversablePrintLogAll(nil)
  val seqOfOpt = Cons(Optional.Just(10), Cons(Optional.Just(20), Nil()))
  Ex5Traversable.logAll(seqOfOpt)
