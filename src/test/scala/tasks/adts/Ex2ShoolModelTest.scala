package tasks.adts

import org.junit.*
import org.junit.Assert.*
import tasks.adts.SchoolModel.*
import tasks.adts.SchoolModel.SchoolModuleImpl.*
import u03.Sequences.Sequence.*
import u03.Optionals.Optional.*
import u03.Optionals.Optional
class Ex2ShoolModelTest {

  val module = SchoolModuleImpl
  val emptySchool = module.School(Nil(), Nil())

  @Test def testAddTeacher() =
    val teacherSchool: School = emptySchool.addTeacher("Pianini")
    val teacher = teacherSchool.teacherByName("Pianini")
    assertEquals(Just(Teacher("Pianini", Nil())), teacher)

  @Test def testAddCourse() =
    val courseSchool: School = emptySchool.addCourse("Math")
    val course = courseSchool.courseByName("Math")
    assertEquals(Just(Course("Math")), course)

  @Test def testNameOfTeacher =
    val teacher = Teacher("Pianini", Nil())
    assertEquals("Pianini", emptySchool.nameOfTeacher(teacher))

  @Test def testNameOfCourse =
    val course = Course("Math")
    assertEquals("Math", emptySchool.nameOfCourse(course))

  @Test def testSetTeacherToCourse() =
    val teacherSchool: School = emptySchool.addTeacher("Pianini")
    val courseSchool: School = teacherSchool.addCourse("Math")
    val teacher = teacherSchool.teacherByName("Pianini")
    val course = courseSchool.courseByName("Math")
    val newSchool =
      courseSchool.setTeacherToCourse(
        Optional.orElse(teacher, Teacher("", Nil())),
        Optional.orElse(course, Course(""))
      )
    assertEquals(
      Just(Teacher("Pianini", Cons(Course("Math"), Nil()))),
      newSchool.teacherByName("Pianini")
    )
    assertEquals(
      Cons(Course("Math"), Nil()),
      newSchool.coursesOfATeacher(
        Optional.orElse(newSchool.teacherByName("Pianini"), Teacher("", Nil()))
      )
    )
}
