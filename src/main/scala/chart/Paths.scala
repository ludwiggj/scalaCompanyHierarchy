package chart

import scala.collection.mutable
import scala.collection.mutable.{Set, HashMap}

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

  def shortest: Path = {
    val pathsByLength = new HashMap[Int, Set[Path]] with mutable.MultiMap[Int, Path]
    list.foreach { path => pathsByLength.addBinding(path.length(), path) }

    pathsByLength.toSeq.sortBy(_._1).head._2.head
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