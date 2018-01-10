package chart

object Utils {
  def trimLeadingAndTrailingSpaces(str: String) = {
    str.dropWhile(' '.equals(_)).reverse.dropWhile(' '.equals(_)).reverse
  }
}