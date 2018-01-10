package chart

import java.io.FileNotFoundException

import org.scalatest.{FlatSpec, Matchers}

class TraversalTest extends FlatSpec with Matchers {

  "Path from Dangermouse to Super Ted" should "return the correct path" in {
    testTypicalCompanyDangermouseToSuperTed(aTypicalCompany())
  }

  "Path from Dangermouse to Super Ted" should "return the correct path when loaded from file" in {
    testTypicalCompanyDangermouseToSuperTed(Company("companyTypical.txt"))
  }

  def testTypicalCompanyDangermouseToSuperTed(company: Company): Unit = {

    val allPaths = company.findAllPaths("Dangermouse", "Super Ted")

    allPaths should contain(
      "Dangermouse (1) <- Invisible Woman (3) <- Super Ted (15)"
    )

    allPaths.size should equal(1)
  }

  "Path from Batman to Catwoman" should "display arrows pointing to common manager" in {
    testTypicalCompanyBatmanToCatwoman(aTypicalCompany())
  }

  "Path from Batman to Catwoman" should "display arrows pointing to common manager when loaded from file" in {
    testTypicalCompanyBatmanToCatwoman(Company("companyTypical.txt"))
  }

  def testTypicalCompanyBatmanToCatwoman(company: Company) = {
    val allPaths = company.findAllPaths("Batman", "Catwoman")

    allPaths should contain(
      "Batman (16) -> Black Widow (6) <- Catwoman (17)"
    )

    allPaths.size should equal(1)
  }

  "Path from an employee to someone she manages" should "point to the manager" in {
    testTypicalCompanyDangermouseToGonzoTheGreat(aCompanyWithTwoEmployees)
  }

  "Path from an employee to someone she manages" should "point to the manager when loaded from file" in {
    testTypicalCompanyDangermouseToGonzoTheGreat(Company("companyWithTwoEmployees.txt"))
  }

  def testTypicalCompanyDangermouseToGonzoTheGreat(company: Company) = {
    val allPaths = company.findAllPaths("Dangermouse", "Gonzo the Great")

    allPaths should contain(
      "Dangermouse (1) <- Gonzo the Great (2)"
    )

    allPaths.size should equal(1)
  }

  "Path from an employee to her manager" should "point to the manager" in {
    testTypicalCompanyGonzoTheGreatToDangermouse(aCompanyWithTwoEmployees())
  }

  "Path from an employee to her manager" should "point to the manager when loaded from file" in {
    testTypicalCompanyGonzoTheGreatToDangermouse(Company("companyWithTwoEmployees.txt"))
  }

  def testTypicalCompanyGonzoTheGreatToDangermouse(company: Company) = {
    val allPaths = company.findAllPaths("Gonzo the Great", "Dangermouse")

    allPaths should contain(
      "Gonzo the Great (2) -> Dangermouse (1)"
    )

    allPaths.size should equal(1)
  }

  "Path from two identical employees" should "return both paths" in {
    testCompanyWithTwoEmployeesWithSameManagerBatmanToDangermouse(aCompanyWithTwoIdenticalEmployeesWithTheSameManager())
  }

  "Path from two identical employees" should "return both paths when loaded from file" in {
    testCompanyWithTwoEmployeesWithSameManagerBatmanToDangermouse(Company("companyWithTwoIdenticalEmployeesWithTheSameManager.txt"))
  }

  def testCompanyWithTwoEmployeesWithSameManagerBatmanToDangermouse(company: Company) = {
    val allPaths = company.findAllPaths("Batman", "Dangermouse")

    allPaths should contain theSameElementsAs (
      List(
        "Batman (16) -> Gonzo the Great (2) -> Dangermouse (1)",
        "Batman (666) -> Gonzo the Great (2) -> Dangermouse (1)"
      )
      )

    allPaths.size should equal(2)
  }

  "Path to two identical employees" should "return both paths" in {
    testCompanyWithTwoEmployeesWithSameManagerDangermouseToBatman(aCompanyWithTwoIdenticalEmployeesWithTheSameManager())
  }

  "Path to two identical employees" should "return both paths when loaded from file" in {
    testCompanyWithTwoEmployeesWithSameManagerDangermouseToBatman(Company("companyWithTwoIdenticalEmployeesWithTheSameManager.txt"))
  }

  def testCompanyWithTwoEmployeesWithSameManagerDangermouseToBatman(company: Company) = {
    val allPaths = company.findAllPaths("Dangermouse", "Batman")

    allPaths should contain theSameElementsAs (
      List(
        "Dangermouse (1) <- Gonzo the Great (2) <- Batman (16)",
        "Dangermouse (1) <- Gonzo the Great (2) <- Batman (666)"
      )
      )

    allPaths.size should equal(2)
  }

  "Path from one identical employee to another" should "return both paths" in {
    testCompanyWithTwoEmployeesWithSameManagerBatmanToBatman(aCompanyWithTwoIdenticalEmployeesWithTheSameManager())
  }

  "Path from one identical employee to another" should "return both paths when loaded from file" in {
    testCompanyWithTwoEmployeesWithSameManagerBatmanToBatman(Company("companyWithTwoIdenticalEmployeesWithTheSameManager.txt"))
  }

  def testCompanyWithTwoEmployeesWithSameManagerBatmanToBatman(company: Company) = {
    val allPaths = company.findAllPaths("Batman", "Batman")

    allPaths should contain theSameElementsAs (
      List(
        "Batman (666) -> Gonzo the Great (2) <- Batman (16)",
        "Batman (16) -> Gonzo the Great (2) <- Batman (666)"
      )
      )

    allPaths.size should equal(2)
  }

  "Attempt to find path when first employee does not exist" should "throw an EmployeeNotFoundException" in {
    testCannotFindFirstEmployee(aCompanyWithTwoEmployees())
  }

  "Attempt to find path when first employee does not exist" should "throw an EmployeeNotFoundException when loaded from file" in {
    testCannotFindFirstEmployee(Company("companyWithTwoEmployees.txt"))
  }

  def testCannotFindFirstEmployee(company: Company) = {
    intercept[EmployeeNotFoundException] {
      company.findAllPaths("WhoIsHe", "Dangermouse")
    }
  }

  "Attempt to find path when second employee does not exist" should "throw an EmployeeNotFoundException" in {
    testCannotFindSecondEmployee(aCompanyWithTwoEmployees())
  }

  "Attempt to find path when second employee does not exist" should "throw an EmployeeNotFoundException when loaded from file" in {
    testCannotFindSecondEmployee(Company("companyWithTwoEmployees.txt"))
  }

  def testCannotFindSecondEmployee(company: Company) = {
    intercept[EmployeeNotFoundException] {
      company.findAllPaths("Dangermouse", "WhoIsHe")
    }
  }

  "Path from identical employees to identical employees" should "return all paths" in {
    testCompanyWithTwoPairsOfIdenticalEmployeesBatmanToAnimal(aCompanyWithTwoPairsOfIdenticalEmployees())
  }

  "Path from identical employees to identical employees" should "return all paths when loaded from file" in {
    testCompanyWithTwoPairsOfIdenticalEmployeesBatmanToAnimal(Company("companyWithTwoPairsOfIdenticalEmployees.txt"))
  }

  def testCompanyWithTwoPairsOfIdenticalEmployeesBatmanToAnimal(company: Company) = {
    val allPaths = company.findAllPaths("Batman", "Animal")

    allPaths should contain theSameElementsAs (
      List(
        "Batman (666) -> Gonzo the Great (2) -> Dangermouse (1) <- Animal (21) <- Animal (37)",
        "Batman (666) -> Gonzo the Great (2) -> Dangermouse (1) <- Animal (21)",
        "Batman (16) -> Gonzo the Great (2) -> Dangermouse (1) <- Animal (21) <- Animal (37)",
        "Batman (16) -> Gonzo the Great (2) -> Dangermouse (1) <- Animal (21)"
      )
      )

    allPaths.size should equal(4)
  }

  "Path from identical employees to identical employees in opposite direction" should "return all paths" in {
    testCompanyWithTwoPairsOfIdenticalEmployeesAnimalToBatman(aCompanyWithTwoPairsOfIdenticalEmployees())
  }

  "Path from identical employees to identical employees in opposite direction" should "return all paths when loaded from file" in {
    testCompanyWithTwoPairsOfIdenticalEmployeesAnimalToBatman(Company("companyWithTwoPairsOfIdenticalEmployees.txt"))
  }

  def testCompanyWithTwoPairsOfIdenticalEmployeesAnimalToBatman(company: Company) = {
    val allPaths = company.findAllPaths("Animal", "Batman")

    allPaths should contain theSameElementsAs (
      List(
        "Animal (37) -> Animal (21) -> Dangermouse (1) <- Gonzo the Great (2) <- Batman (666)",
        "Animal (21) -> Dangermouse (1) <- Gonzo the Great (2) <- Batman (666)",
        "Animal (37) -> Animal (21) -> Dangermouse (1) <- Gonzo the Great (2) <- Batman (16)",
        "Animal (21) -> Dangermouse (1) <- Gonzo the Great (2) <- Batman (16)"
      )
      )

    allPaths.size should equal(4)
  }

  "Finding an employee that exists in the company" should "return the employee" in {
    testFindEmployee(aTypicalCompany())
  }

  "Finding an employee that exists in the company" should "return the employee when loaded from file" in {
    testFindEmployee(Company("companyTypical.txt"))
  }

  def testFindEmployee(company: Company) = {
    val employees = company.findEmployee("Super Ted")

    employees should contain(Node(Employee("Super Ted", 15))(-1))

    employees.size should equal(1)
  }

  "Finding an employee when there are two identically named employees" should "return both employees" in {
    testFindMultipleEmployees(aCompanyWithTwoIdenticalEmployeesWithDifferentManagers())
  }

  "Finding an employee when there are two identically named employees" should "return both employees when loaded from file" in {
    testFindMultipleEmployees(Company("companyWithTwoIdenticalEmployeesWithDifferentManagers.txt"))
  }

  def testFindMultipleEmployees(company: Company) = {
    val employees = company.findEmployee("Batman")

    employees should contain theSameElementsAs (
      List(
        Node(Employee("Batman", 16))(-1),
        Node(Employee("Batman", 666))(-1))
      )

    employees.size should equal(2)
  }

  "Finding an employee who does not work for the company" should "throw an EmployeeNotFoundException" in {
    testFindMissingEmployee(aTypicalCompany())
  }

  "Finding an employee who does not work for the company" should "throw an EmployeeNotFoundException when loaded from file" in {
    testFindMissingEmployee(Company("companyTypical.txt"))
  }

  def testFindMissingEmployee(company: Company) = {
    intercept[EmployeeNotFoundException] {
      company.findEmployee("Not there")
    }
  }

  "Stripped name of employee [ Gonzo   the Great  ]" should "be [gonzo the great]" in {
    EmployeeName(" Gonzo   the Great  ").stripped should equal("gonzo the great")
  }

  "Stripped name of employee [Gonzo the Great]" should "be [gonzo the great]" in {
    EmployeeName("Gonzo the Great").stripped should equal("gonzo the great")
  }

  "Stripped name of employee [gonzo the GREAT]" should "be [gonzo the great]" in {
    EmployeeName("gonzo the GREAT").stripped should equal("gonzo the great")
  }

  "Stripped name of employee [gOnZO]" should "be [gonzo]" in {
    EmployeeName("gOnZO").stripped should equal("gonzo")
  }

  "Stripped name of employee [Gon Zot Heg Reat]" should "be [gon zot heg reat]" in {
    EmployeeName("Gon Zot Heg Reat").stripped should equal("gon zot heg reat")
  }

  "Paths between gonzos" should "return all paths" in {
    val company = aCompanyOfGonzos();

    val allPaths = company.findAllPaths("gonzo the great", "gonzo the great")

    allPaths should contain theSameElementsAs (
      List(
        " Gonzo   the Great   (1) <- Gon Zot Heg Reat (2) <- gonzo the GREAT (3)",
        " Gonzo   the Great   (1) <- Gon Zot Heg Reat (2) <- gonzo the GREAT (3) <- gOnZO (4) <- Gonzo the Great (5)",
        "gonzo the GREAT (3) -> Gon Zot Heg Reat (2) ->  Gonzo   the Great   (1)",
        "gonzo the GREAT (3) <- gOnZO (4) <- Gonzo the Great (5)",
        "Gonzo the Great (5) -> gOnZO (4) -> gonzo the GREAT (3)",
        "Gonzo the Great (5) -> gOnZO (4) -> gonzo the GREAT (3) -> Gon Zot Heg Reat (2) ->  Gonzo   the Great   (1)")
      )
    allPaths.size should equal(6)
  }

  "Paths between gonzos" should "return all paths when loaded from file" in {
    val company = Company("companyOfGonzos.txt");

    val allPaths = company.findAllPaths("gonzo the great", "gonzo the great")

    allPaths should contain theSameElementsAs (
      List(
        "Gonzo   the Great (1) <- Gon Zot Heg Reat (2) <- gonzo the GREAT (3)",
        "Gonzo   the Great (1) <- Gon Zot Heg Reat (2) <- gonzo the GREAT (3) <- gOnZO (4) <- Gonzo the Great (5)",
        "gonzo the GREAT (3) -> Gon Zot Heg Reat (2) -> Gonzo   the Great (1)",
        "gonzo the GREAT (3) <- gOnZO (4) <- Gonzo the Great (5)",
        "Gonzo the Great (5) -> gOnZO (4) -> gonzo the GREAT (3)",
        "Gonzo the Great (5) -> gOnZO (4) -> gonzo the GREAT (3) -> Gon Zot Heg Reat (2) -> Gonzo   the Great (1)")
      )
    allPaths.size should equal(6)
  }

  "Attempt to load a file that is not there" should "throw an exception" in {
    intercept[FileNotFoundException] {
      Company("aFileThatIsNotThere.txt")
    }
  }

  private def aTypicalCompany() = {
    Company(aTypicalCompanyRoot())
  }

  def aTypicalCompanyRoot(): Node = {
    val root = Node(Employee("Dangermouse", 1))(0)

    val gonzo = Node(Employee("Gonzo the Great", 2))(root.level + 1, Some(root))
    root.addChild(gonzo)

    val invisibleWoman = Node(Employee("Invisible Woman", 3))(root.level + 1, Some(root))
    root.addChild(invisibleWoman)

    val blackWidow = Node(Employee("Black Widow", 6))(gonzo.level + 1, Some(gonzo))
    gonzo.addChild(blackWidow)

    val batman = Node(Employee("Batman", 16))(blackWidow.level + 1, Some(blackWidow))
    blackWidow.addChild(batman)

    val catWoman = Node(Employee("Catwoman", 17))(blackWidow.level + 1, Some(blackWidow))
    blackWidow.addChild(catWoman)

    val hitGirl = Node(Employee("Hit Girl", 12))(invisibleWoman.level + 1, Some(invisibleWoman))
    invisibleWoman.addChild(hitGirl)

    val superTed = Node(Employee("Super Ted", 15))(invisibleWoman.level + 1, Some(invisibleWoman))
    invisibleWoman.addChild(superTed)
    root
  }

  private def aCompanyWithTwoIdenticalEmployeesWithDifferentManagers() = {
    val root = aTypicalCompanyRoot()
    val anotherBatman = Node(Employee("Batman", 666))(root.level + 1, Some(root))
    root.addChild(anotherBatman)
    Company(root)
  }

  private def aCompanyWithTwoEmployees() = {
    val root = Node(Employee("Dangermouse", 1))(0)

    val gonzo = Node(Employee("Gonzo the Great", 2))(root.level + 1, Some(root))
    root.addChild(gonzo)

    Company(root)
  }

  private def aCompanyWithTwoIdenticalEmployeesWithTheSameManager() = {
    val root = Node(Employee("Dangermouse", 1))(0)

    val gonzo = Node(Employee("Gonzo the Great", 2))(root.level + 1, Some(root))
    root.addChild(gonzo)

    val batman = Node(Employee("Batman", 16))(gonzo.level + 1, Some(gonzo))
    gonzo.addChild(batman)

    val anotherBatman = Node(Employee("Batman", 666))(gonzo.level + 1, Some(gonzo))
    gonzo.addChild(anotherBatman)

    Company(root)
  }

  private def aCompanyWithTwoPairsOfIdenticalEmployees() = {
    val root = Node(Employee("Dangermouse", 1))(0)

    val gonzo = Node(Employee("Gonzo the Great", 2))(root.level + 1, Some(root))
    root.addChild(gonzo)

    val batman = Node(Employee("Batman", 16))(gonzo.level + 1, Some(gonzo))
    gonzo.addChild(batman)

    val anotherBatman = Node(Employee("Batman", 666))(gonzo.level + 1, Some(gonzo))
    gonzo.addChild(anotherBatman)

    val animal = Node(Employee("Animal", 21))(root.level + 1, Some(root))
    root.addChild(animal)

    val anotherAnimal = Node(Employee("Animal", 37))(animal.level + 1, Some(animal))
    animal.addChild(anotherAnimal)

    Company(root)
  }

  private def aCompanyOfGonzos() = {
    val headGonzo = Node(Employee(" Gonzo   the Great  ", 1))(0)

    val gonzo1 = Node(Employee("Gon Zot Heg Reat", 2))(headGonzo.level + 1, Some(headGonzo))
    headGonzo.addChild(gonzo1)

    val gonzo2 = Node(Employee("gonzo the GREAT", 3))(gonzo1.level + 1, Some(gonzo1))
    gonzo1.addChild(gonzo2)

    val gonzo3 = Node(Employee("gOnZO", 4))(gonzo2.level + 1, Some(gonzo2))
    gonzo2.addChild(gonzo3)

    val gonzo4 = Node(Employee("Gonzo the Great", 5))(gonzo3.level + 1, Some(gonzo3))
    gonzo3.addChild(gonzo4)

    Company(headGonzo)
  }
}