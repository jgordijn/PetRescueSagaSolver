package petrescuesagasolver

object PetRescueSagaBoardReader {
  def readBoard(fileName: String): Board = {
    val fileLines: Vector[String] = scala.io.Source.fromFile(fileName).getLines().toVector

    val yUpper = fileLines.size
    val xUpper = fileLines.head.length
    val allBlocks: Vector[Block] = (for{
      y <- 0 until yUpper
      x <- 0 until xUpper
    } yield {
      val line = fileLines(y)
      val charOfLine = line.charAt(x)
      Block(charOfLine, x, y, charOfLine != ' ', false)
    }).toVector


    Board(allBlocks.grouped(fileLines.head.length).toVector)
  }
}
