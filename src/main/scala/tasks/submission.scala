//TASK 1

package tasks.adts

/*  Exercise 1:
 *  Complete the implementation of ComplexADT trait below, so that it passes
 *  the test in ComplexTest.
 */

object Ex1ComplexNumbers:

  trait ComplexADT:
    type Complex
    def complex(re: Double, im: Double): Complex
    extension (complex: Complex)
      def re(): Double
      def im(): Double
      def sum(other: Complex): Complex
      def subtract(other: Complex): Complex
      def asString(): String

  object BasicComplexADT extends ComplexADT:

    // Change assignment below: should probably define a case class and use it?

    case class Complex(re: Double, im: Double)

    def complex(re: Double, im: Double): Complex = Complex(re, im)
    extension (complex: Complex)
      def re(): Double = complex match
        case Complex(re, im) => re
      def im(): Double = complex match
        case Complex(re, im) => im
      def sum(other: Complex): Complex = (complex, other) match
        case (Complex(re0, im0), Complex(re1, im1)) =>
          Complex(re0 + re1, im0 + im1)
      def subtract(other: Complex): Complex = (complex, other) match
        case (Complex(re0, im0), Complex(re1, im1)) =>
          Complex(re0 - re1, im0 - im1)
      def asString(): String =
        extension (d: Double)
          def punctuationToString(writePositive: Boolean = false): String =
            d match
              case d if d > 0 && writePositive => " + " + d
              case d if d < 0                  => " - " + -d
              case _                           => "" + d

          def isNotZero: Boolean = d match
            case d if d != 0 => true
            case _           => false
        complex match
          case Complex(re, im) if re.isNotZero && im.isNotZero =>
            re.punctuationToString() + im.punctuationToString(true) + "i"
          case Complex(re, im) if re.isNotZero => re + ""
          case Complex(re, im) if im.isNotZero => im + "i"
          case Complex(re, im)                 => re.punctuationToString()

//TASK 2

package tasks.adts
import u03.Sequences.Sequence.*
import u03.Sequences.*
import u03.Optionals.*
import u03.Optionals.Optional.*
import u02.AlgebraicDataTypes.Person

/*  Exercise 2:
 *  Implement the below trait, and write a meaningful test.
 *  Suggestion:
 *  - reuse Sequences and Optionals as imported above
 *  - Course is a simple case classes with just the name
 *  - Teacher is a case class with name and sequence of courses
 *  - School is a case class with (sequences of) teachers and courses
 *  - add/set methods below create the new school
 */

object SchoolModel:

  trait SchoolModule:
    type School
    type Teacher
    type Course
    extension (school: School)
      def addTeacher(name: String): School
      def addCourse(name: String): School
      def teacherByName(name: String): Optional[Teacher]
      def courseByName(name: String): Optional[Course]
      def nameOfTeacher(teacher: Teacher): String
      def nameOfCourse(course: Course): String
      def setTeacherToCourse(teacher: Teacher, course: Course): School
      def coursesOfATeacher(teacher: Teacher): Sequence[Course]

  object SchoolModuleImpl extends SchoolModule:

    case class Course(name: String)
    case class Teacher(name: String, courses: Sequence[Course])
    case class School(
        teachers: Sequence[Teacher],
        courses: Sequence[Course]
    )

    extension (school: School)
      def addTeacher(name: String): School =
        School(Cons(Teacher(name, Nil()), school.teachers), school.courses)
      def addCourse(name: String): School =
        School(school.teachers, Cons(Course(name), school.courses))
      def teacherByName(name: String): Optional[Teacher] =
        school.teachers match {
          case Cons(h, _) if school.nameOfTeacher(h) == name => Just(h)
          case _                                             => Empty()
        }
      def courseByName(name: String): Optional[Course] =
        school.courses match {
          case Cons(h, _) if school.nameOfCourse(h) == name => Just(h)
          case _                                            => Empty()
        }
      def nameOfTeacher(teacher: Teacher): String =
        teacher.name
      def nameOfCourse(course: Course): String =
        course.name
      def setTeacherToCourse(teacher: Teacher, course: Course): School =
        School(
          Sequence.map(school.teachers)(t =>
            val tName = school.nameOfTeacher(t)
            if (tName == school.nameOfTeacher(teacher))
              Teacher(tName, Cons(course, school.coursesOfATeacher(t)))
            else t
          ),
          school.courses
        )
      def coursesOfATeacher(teacher: Teacher): Sequence[Course] =
        teacher.courses

//TASK 3

package tasks.adts

import u03.Sequences.*
import u03.Optionals.*

/*  Exercise 3:
 *  Implement a Stack ADT
 *  Suggestion:
 *  - push adds an element and returns the new stack
 *  - pop returns:
 *  -- empty optional is stack is empty
 *  -- a pair of top of the stack and the new stack after removal if not empty
 */
object Ex3Stacks:

  trait StackADT:
    type Stack[A]
    def empty[A]: Stack[A] // factory
    extension [A](stack: Stack[A])
      def push(a: A): Stack[A]
      def pop(a: A): Optional[(A, Stack[A])]
      def asSequence(): Sequence[A]

  object StackImpl extends StackADT:
    case class Stack[A](sequence: Sequence[A])
    def empty[A]: Stack[A] = Stack(Sequence.Nil())
    extension [A](stack: Stack[A])
      def push(a: A): Stack[A] =
        Stack(Sequence.Cons(a, stack.sequence))
      def pop(a: A): Optional[(A, Stack[A])] =
        stack.sequence match {
          case Sequence.Cons(h, t) if h == a => Optional.Just((h, Stack(t)))
          case _                             => Optional.Empty()
        }
      def asSequence(): Sequence[A] = stack.sequence

//TASK 4

package u04lab
import u03.Sequences.*
import Sequence.*

/*  Exercise 4:
 *  - Complete the implementation of ad-hoc polymorphic sumAll, using summable.sum and summable.zero
 *  - Write givens also for Summable[Double], Summable[String]
 *  - Uncomment in the main and check if everything works
 */

object Ex4Summables:

  def sumAllInt(seq: Sequence[Int]): Int = seq match
    case Cons(h, t) => h + sumAllInt(t)
    case _          => 0

  trait Summable[A]:
    def sum(a1: A, a2: A): A
    def zero: A

  def sumAll[A: Summable](seq: Sequence[A]): A =
    val summable = summon[Summable[A]]
    seq match
      case Cons(h, t) => summable.sum(h, sumAll(t))
      case Nil()      => summable.zero

  given Summable[Int] with
    def sum(a1: Int, a2: Int): Int = a1 + a2
    def zero: Int = 0

  given Summable[Double] with
    def sum(a1: Double, a2: Double): Double = a1 + a2
    def zero: Double = 0.0

  given Summable[String] with
    def sum(a1: String, a2: String): String = a1 + a2
    def zero: String = ""

  // write givens for Summable[Double] and Summable[String]

  @main def trySummables =
    val si = Cons(10, Cons(20, Cons(30, Nil())))
    println:
      sumAllInt(si) // 60

    println:
      sumAll(si) // 60

    val sd = Cons(10.0, Cons(20.0, Cons(30.0, Nil())))
    println:
      sumAll(sd) // 60.0

    val ss = Cons("10", Cons("20", Cons("30", Nil())))
    println:
      sumAll(ss) // "102030"

//TASK 5

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

//TASK 6

package tasks.monads

import u04.monads.Monads.Monad
import u04.monads.Monads.Monad

/** Exercise 6: This module contains the implementation of a Try monad, which is
  * a monad that represents a computation that may fail. Try to follow these
  * steps: \- Look at the implementation of Try, that is similar to the one of
  * Optional \- Try go define the Monad instance for Try \- flatMap should
  * consider only the Success case \- in case of Failure, it should return the
  * exception (fail fast) \- Verify that the main works as expected
  */
object Ex6TryModel:
  private enum TryImpl[A]:
    case Success(value: A)
    case Failure(exception: Throwable)

  opaque type Try[A] = TryImpl[A]

  def success[A](value: A): Try[A] = TryImpl.Success(value)
  def failure[A](exception: Throwable): Try[A] = TryImpl.Failure(exception)
  def exec[A](expression: => A): Try[A] = try success(expression)
  catch failure(_)

  extension [A](m: Try[A])
    def getOrElse[B >: A](other: B): B = m match
      case TryImpl.Success(value) => value
      case TryImpl.Failure(_)     => other

  given Monad[Try] with
    override def unit[A](value: A): Try[A] = success(value)

    extension [A](m: Try[A])
      override def flatMap[B](f: A => Try[B]): Try[B] = m match
        case TryImpl.Success(a) => f(a)
        case TryImpl.Failure(exception) => failure(exception)

@main def main: Unit =
  import Ex6TryModel.*

  val result = for
    a <- success(10)
    b <- success(30)
  yield a + b

  assert(result.getOrElse(-1) == 40)
  println(result.getOrElse(-1))

  val result2 = for
    a <- success(10)
    b <- failure(new RuntimeException("error"))
    c <- success(30)
  yield a + c

  assert(success(20).map(_ + 10).getOrElse(-1) == 30)
  assert(result2.getOrElse(-1) == -1)
  println(result2.getOrElse(-1))


  val result3 = for
    a <- exec(10)
    b <- exec(new RuntimeException("error"))
    c <- exec(30)
  yield a + c


