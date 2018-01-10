package chart

class ParsedEmployee(val managerID: Option[Int], val name: String, val employeeID: Int) {
  def isBigBoss = managerID == None
}

object ParsedEmployee {
  def apply(fields: Array[String]) = {
    new ParsedEmployee(
      if ("".equals(fields(2))) None else Some(fields(2).toInt),
      fields(1),
      fields(0).toInt
    )
  }
}