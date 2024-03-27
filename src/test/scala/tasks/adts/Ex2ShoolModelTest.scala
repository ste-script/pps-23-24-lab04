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
}
