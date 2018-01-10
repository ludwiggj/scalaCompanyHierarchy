package chart

import Utils.trimLeadingAndTrailingSpaces

case class EmployeeName(name: String) {

  override def equals(other: Any) = {
    other match {
      case employeeName: EmployeeName =>
        strippedAndInLowerCase == employeeName.strippedAndInLowerCase
      case _ => false
    }
  }

  override def toString() = name

  override def hashCode: Int =
    41 * (
      41 + strippedAndInLowerCase.hashCode
      )

  def strippedAndInLowerCase: String = {
    trimLeadingAndTrailingSpaces(name).replaceAll("[ ]{2,}", " ").toLowerCase
  }
}