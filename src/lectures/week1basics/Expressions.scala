package lectures.week1basics

object Expressions extends App{
    class Instructor(val id: Int, val name: String, val surname: String) {
    def fullName: String = name.capitalize + " " + surname.capitalize
  }

  class Course2(val courseID: Int, val title: String, val releaseYear: String, val instructor: Instructor) {
    def getID: String = courseID.toString + instructor.id
    def isTaughtBy(newInstructor:Instructor):Boolean = if(newInstructor == instructor) true else false
    def copyCourse(newReleaseYear:String): Course2 = new Course2(courseID, title, newReleaseYear, instructor)
  }



  class Person(val name: String, occupation: String) {
    def worksAs(jobName: String): String = s"$name is a $jobName"
    def isDeveloper: String = worksAs("Scala Developer")
  }

  val bob: Person = new Person("Bob", "Developer")
  println(bob.isDeveloper)


  class Person2(val name : String) {
    def unary_+ : Person2 = new Person2(s"$name NoSurname")

  }
  val person = new Person2("Bob")
  println((+person).name)

  case class Course(title: String, instructor: String)

  object Course {
    def apply(instructor: String): Course = Course("AdvancedScala", instructor)
  }

  val scalaCourse = Course("Scala", "Bob")
  println(scalaCourse)

}
