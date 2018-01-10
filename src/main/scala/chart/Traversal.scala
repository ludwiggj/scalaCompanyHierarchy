package chart

object Main extends App {
    val company = Company("company.txt")

    println(company.findAllPaths("Dangermouse", "Super Ted"))
}