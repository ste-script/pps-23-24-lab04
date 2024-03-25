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
