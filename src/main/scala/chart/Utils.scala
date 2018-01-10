package chart

object Utils {
  def trimLeadingAndTrailingSpaces(str: String) = {
    str.dropWhile(' ' == _).reverse.dropWhile(' ' == _).reverse
  }
}