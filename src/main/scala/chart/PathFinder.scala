package chart

object PathFinder {
  def main (args: Array[String]) {
    if (args.size != 3) {
      throw new IllegalArgumentException("Usage: PathFinder <filename> <fromEmployeeName> <toEmployeeName>")
    } else {
      println(Company(args(0)).findAllPaths(args(1), args(2)))
    }
  }
}