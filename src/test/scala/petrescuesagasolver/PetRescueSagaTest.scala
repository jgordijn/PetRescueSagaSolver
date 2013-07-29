package petrescuesagasolver

import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

class PetRescueSagaTest extends FunSuite {
  val g = 'g'
  val r = 'r'
  val board = Board(Vector(
    Vector(Block(g,0,0,true,false), Block(g,1,0,true,false), Block(r,2,0,true,false), Block(g,3,0,true,false)),
    Vector(Block(g,0,1,true,false), Block(g,1,1,true,false), Block(r,2,1,true,false), Block(g,3,1,true,false))
  ))

  val redBlock1 = Block(r, 2, 0, true, false)
  val redBlock2 = Block(r, 2, 1, true, false)


  test("group function") {
    val startTime = System.currentTimeMillis()
    val board = PetRescueSagaBoardReader.readBoard("doc/level3.txt")
    val endTime = System.currentTimeMillis()
    println("generate took: " + (endTime - startTime))
    println(board)
    assert(board === this.board)
  }

  test("click on block") {
    val (newBoard, score) = board.clickOnBlock(redBlock1)
    assert(score === 40)
    val expectedBoard = Board(Vector(
      Vector(Block(g,0,0,true,false), Block(g,1,0,true,false), Block(g,2,0,true,false), Block(' ',3,0,false,false)),
      Vector(Block(g,0,1,true,false), Block(g,1,1,true,false), Block(g,2,1,true,false), Block(' ',3,1,false,false))
    ))

    println("Expected:")
    expectedBoard.printBoard()
    println("Actual:")
    newBoard.printBoard()
    assert(newBoard === expectedBoard)
  }

  test("get field for blocks") {
    val fieldToBeRemoved = board.getFieldForBlock(redBlock1)
    assert(Set(redBlock1, redBlock2) === fieldToBeRemoved)
  }

  test("remove field gives new board with the fields replaced by empt blocks") {
    val newBoard = board.removeField(Set(redBlock1, redBlock2))
    val expectedBoard = Board(Vector(
      Vector(Block(g,0,0,true,false), Block(g,1,0,true,false), Block(' ',2,0,false,false), Block(g,3,0,true,false)),
      Vector(Block(g,0,1,true,false), Block(g,1,1,true,false), Block(' ',2,1,false,false), Block(g,3,1,true,false))
    ))
    assert(newBoard === expectedBoard)
  }

  test("drop blocks, drops block by gravity and then shift them to left as much as possible") {
    val boardWithEmptySpaces = Board(Vector(
      Vector(Block(g,0,0,true,false), Block(g,1,0,true,false), Block(' ',2,0,false,false), Block(g,3,0,true,false)),
      Vector(Block(g,0,1,true,false), Block(g,1,1,true,false), Block(' ',2,1,false,false), Block(g,3,1,true,false))
    ))
    val expectedBoard = Board(Vector(
      Vector(Block(g,0,0,true,false), Block(g,1,0,true,false), Block(g,2,0,true,false), Block(' ', 3, 0, false, false)),
      Vector(Block(g,0,1,true,false), Block(g,1,1,true,false), Block(g,2,1,true,false), Block(' ', 3, 1, false, false))
    ))

    val newBoard = boardWithEmptySpaces.dropBlocks

    println("Start:")
    boardWithEmptySpaces.printBoard()
    println("Expected:")
    expectedBoard.printBoard()
    println("Actual:")
    newBoard.printBoard()

    assert(newBoard === expectedBoard)
  }

  test("drop blocks: drop block down") {
    val boardWithEmptySpaces = Board(Vector(
      Vector(Block(g,0,0,true,false), Block(g,1,0,true,false)),
      Vector(Block(g,0,1,true,false), Block(' ',1,1,false,false))
    ))
    val expectedBoard = Board(Vector(
      Vector(Block(g,0,0,true,false), Block(' ',1,0,false,false)),
      Vector(Block(g,0,1,true,false), Block(g,1,1,true,false))
    ))

    val newBoard = boardWithEmptySpaces.dropBlocks

    assert(newBoard === expectedBoard)
  }

  test("drop blocks: shift block left") {
    val boardWithEmptySpaces = Board(Vector(
      Vector(Block(g,0,0,true,false), Block(' ',1,0,false,false), Block(' ',2,0,false,false)),
      Vector(Block(g,0,1,true,false), Block(' ',1,1,false,false), Block(g,2,1,true,false))
    ))
    val expectedBoard = Board(Vector(
      Vector(Block(g,0,0,true,false), Block(' ',1,0,false,false), Block(' ',2,0,false,false)),
      Vector(Block(g,0,1,true,false), Block(g,1,1,true,false), Block(' ',2,1,false,false))
    ))

    val newBoard = boardWithEmptySpaces.dropBlocks

    println("ASDSAdsad")
    println("Start:")
    boardWithEmptySpaces.printBoard()
    println("Expected:")
    expectedBoard.printBoard()
    println("Actual:")
    newBoard.printBoard()

    assert(newBoard === expectedBoard)
  }

  test("switch") {
    val b = Board(Vector(Vector(Block(g,0,0,true,false), Block(g,1,0,true,false), Block(' ' ,2,0,false,false), Block(g,3,0,true,false)), Vector(Block(g,0,1,true,false), Block(g,1,1,true,false), Block(g,2,1,true,false), Block(' ',3,1,false,false))))
    val expectedBoard = Board(Vector(
                    Vector(Block(g,0,0,true,false), Block(g,1,0,true,false), Block(g,2,0,true,false), Block(' ',3,0,false,false)),
                    Vector(Block(g,0,1,true,false), Block(g,1,1,true,false), Block(g,2,1,true,false), Block(' ',3,1,false,false))))
    val newBoard = b.switchBlocks(Block(g,3,0,true,false), Block(' ',2,0,false,false))

    println("ASDSAdsad")
    println("Start:")
    b.printBoard()
    println("Expected:")
    expectedBoard.printBoard()
    println("Actual:")
    newBoard.printBoard()

    assert(newBoard === expectedBoard)
  }
}