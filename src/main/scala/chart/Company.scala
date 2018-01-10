package chart

import java.io.FileNotFoundException
import java.io.File

import chart.Utils._

import scala.collection.mutable.{MultiMap, Set, HashMap}
import scala.io.Source
import scala.util.{Failure, Success, Try}

class Company(root: Node) {

  def findEmployee(employee: String): List[Node] = {
    val employeeName = EmployeeName(employee)

    def walkTree(current: Node): List[Node] = {
      val matches = if (employeeName == current.name) {
        List(current)
      } else {
        List()
      }

      val descendantMatches = for {
        child <- current.children
        node <- walkTree(child)
      } yield node

      matches ++ descendantMatches
    }

    val employees = walkTree(root)
    if (employees.size > 0) employees else throw new EmployeeNotFoundException(employee)
  }

  private def pathThroughChildNodes(childNodes: List[Node], pathToCurrentNode: Path, nodesVisitedSoFar: List[Node]): Paths = {
    Paths.flatten(for {
      child <- (childNodes diff nodesVisitedSoFar)
    } yield findAllPaths(child, pathToCurrentNode, nodesVisitedSoFar))
  }

  private def pathThroughParentNode(parentNode: Option[Node], pathToCurrentNode: Path, nodesVisitedSoFar: List[Node]): Paths = {
    def parentExistsAndHasNotYetBeenVisited = {
      (parentNode.isDefined) && !nodesVisitedSoFar.contains(parentNode.get)
    }
    if (parentExistsAndHasNotYetBeenVisited)
      findAllPaths(parentNode.get, pathToCurrentNode, nodesVisitedSoFar)
    else Paths()
  }

  private def findAllPaths(currentNode: Node, pathToPrecedingNode: Path, nodesVisitedSoFar: List[Node]): Paths = {
    val pathToCurrentNode = currentNode :: pathToPrecedingNode
    val nodesVisitedSoFarPlusMe = currentNode :: nodesVisitedSoFar

    pathToCurrentNode.reverse ::
      (pathThroughChildNodes(currentNode.children, pathToCurrentNode, nodesVisitedSoFarPlusMe) ++
        pathThroughParentNode(currentNode.parent, pathToCurrentNode, nodesVisitedSoFarPlusMe))
  }

  def findAllPaths(fromEmployeeName: String, toEmployeeName: String): Paths = {
    val startNodes = for {
      startNodes <- Try(findEmployee(fromEmployeeName))
      endNodes <- Try(findEmployee(toEmployeeName))
    } yield startNodes

    startNodes match {

      case Success(startNodes) =>

        val paths = Paths.flatten(for {
          startNode <- startNodes
        } yield findAllPaths(startNode, Path(), List()))

        paths.whichContain(EmployeeName(fromEmployeeName), EmployeeName(toEmployeeName))

      case Failure(ex) =>
        throw ex
    }
  }

  def findAllPathsAsListOfStrings(fromEmployeeName: String, toEmployeeName: String): List[String] = {
    findAllPaths(fromEmployeeName, toEmployeeName).asListOfStrings()
  }

  def findShortestPath(fromEmployeeName: String, toEmployeeName: String): String = {
    findAllPaths(fromEmployeeName, toEmployeeName).shortest.toString
  }
}

object Company {
  type EmployeeStore = HashMap[Int, Set[Employee]] with MultiMap[Int, Employee]

  def apply(root: Node) = {
    new Company(root);
  }

  def apply(filename: String) = {
    try {
      buildCompany(parseFile(filename))
    } catch {
      case ex: FileNotFoundException => {
        val canonicalPathToFile =
          new File("dummy.txt").getCanonicalPath.replaceFirst("dummy.txt", filename)
        throw new FileNotFoundException(s"Could not open file ${canonicalPathToFile}")
      }
    }
  }

  def parseFile(filename: String): (Employee, EmployeeStore) = {

    val lines = Source.fromFile(filename).getLines().drop(1)
    val employees = (for {
      line <- lines
      splitLine = line.split("\\|").drop(1).map(trimLeadingAndTrailingSpaces(_))
    } yield ParsedEmployee(splitLine)).toList

    val bigBoss = Employee(employees.find(_.isBigBoss).get)
    val workers = employees.filterNot(_.isBigBoss)

    val employeeHierarchy = new HashMap[Int, Set[Employee]] with MultiMap[Int, Employee]

    for {
      worker <- workers
    } employeeHierarchy.addBinding(worker.managerID.get, Employee(worker))

    (bigBoss, employeeHierarchy)
  }

  private def buildCompany(employees: (Employee, EmployeeStore)): Company = {
    val (root, employeeStore) = employees
    val rootNode = Node(root)(0)

    def buildTreeLevel(currentNode: Node): Unit = {
      for {
        children <- employeeStore.get(currentNode.id)
        child <- children
      } {
        val childNode = Node(child)(currentNode.level + 1, Some(currentNode))
        currentNode.addChild(childNode)
        buildTreeLevel(childNode)
      }
    }

    buildTreeLevel(rootNode)

    Company(rootNode)
  }
}