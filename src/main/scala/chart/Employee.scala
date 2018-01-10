package chart

case class Employee(nom: String, id: Int) {
  val name = EmployeeName(nom)

  override def toString() = s"$name ($id)"
}

object Employee {
  def apply(parsed: ParsedEmployee) = {
    new Employee(parsed.name, parsed.employeeID)
  }
}