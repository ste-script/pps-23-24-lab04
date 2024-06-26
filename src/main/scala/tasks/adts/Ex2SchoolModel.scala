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
