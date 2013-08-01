package petrescuesagasolver

import akka.actor.Actor

case class Read(fileName: String)

class PetRescueSagaBoardReader extends Actor {
  def receive = {
    case Read(fileName) => {
      println (s"received Read($fileName)")
      val b = PetRescueSagaBoardReader.readBoard(fileName)
      println("done")
      sender ! b
    }
  }
}

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
