package chart

case class Node(employee: Employee)
               (val level: Integer, val parent: Option[Node] = None, var children: List[Node] = List()) {
  override def toString() = s"$employee"

  def addChild(child: Node) = {
    children = child :: children
  }

  def id = employee.id

  def name = employee.name
}