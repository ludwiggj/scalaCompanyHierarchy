package chart

class Path(path: List[Node]) {

  def ::(x: Node): Path = {
    Path(x :: path)
  }

  def reverse(): Path = {
    Path(path.reverse)
  }

  def length(): Int = {
    path.length
  }

  def matches(fromName: EmployeeName, toName: EmployeeName) = {
    (path.size >= 2) && path.head.name == fromName && path.takeRight(1)(0).name == toName
  }

  private def convertPair(pair: (Option[Node], Option[Node])): String = {
    val (node1Option, node2Option) = pair
    val node1 = node1Option.get

    node2Option match {
      case Some(node2) => node1 + (if (node1.level > node2.level) " -> " else " <- ")
      case None => node1 + ""
    }
  }

  override def toString = {
    if (path.isEmpty) "" else {
      val pathWithOptionalNodes: List[Option[Node]] = path map (Some(_))

      val pathWithPointers = for {
        pair <- pathWithOptionalNodes zipAll(pathWithOptionalNodes.tail, None, None)
      } yield convertPair(pair)

      pathWithPointers.mkString
    }
  }
}

object Path {
  def apply(nodes: List[Node]) = {
    new Path(nodes)
  }

  def apply() = {
    new Path(List())
  }
}