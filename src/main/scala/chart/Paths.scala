package chart

class Paths(val list: List[Path]) {
  def ::(x: Path): Paths = {
    Paths(x :: list)
  }

  def ++(x: Paths): Paths = {
    Paths(list ++ x.list)
  }

  def whichContain(fromEmployee: EmployeeName, toEmployee: EmployeeName): Paths = {
    Paths(for {
      path <- list if (path.matches(fromEmployee, toEmployee))
    } yield path)
  }

  def asListOfStrings(): List[String] = {
    for {
      path <- list
    } yield path.toString
  }
}

object Paths {
  def apply(paths: List[Path]) = {
    new Paths(paths)
  }

  def apply() = {
    new Paths(List())
  }

  def flatten(pathList: List[Paths]) = {
    Paths((for {
      paths <- pathList
    } yield paths.list).flatten[Path])
  }
}