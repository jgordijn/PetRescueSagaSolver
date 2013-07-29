package petrescuesagasolver

import scala.annotation.tailrec


object Main extends App {
  val board = PetRescueSagaBoardReader.readBoard("doc/level2.txt")
  val mostValuablePlay = measureAndPrintRunningTime(generateSolution(board))
  val moves = mostValuablePlay._1

  type Score = Int
  val score = mostValuablePlay._2

  printMoves(board, moves.reverse)
  println("final score: " + score)

  def measureAndPrintRunningTime[A](func: => A) = {
    val startTime = System.currentTimeMillis()
    val result = func
    val endTime = System.currentTimeMillis()
    println("generate took: " + (endTime - startTime))
    result
  }

  def printMoves(board: Board, moves: List[Block]): Unit = {
    board.printBoard()

    if (moves.nonEmpty) {
      println("Making move: " + moves.head)
      val newBoard = board.clickOnBlock(moves.head)._1
      printMoves(newBoard, moves.tail)
    }
  }

  def generateSolution(board: Board): (List[Block], Score) = {
    @tailrec
    def generateSolution(boardWithMoveList: List[(Board, List[Block], Score)], maximum: (List[Block], Score)): (List[Block], Score) = {
      if (boardWithMoveList.isEmpty) {
        maximum
      }
      else if (boardWithMoveList.head._1.validMoves.isEmpty) {
        val board = boardWithMoveList.head._1
        val moves = boardWithMoveList.head._2
        val score = getCorrectedScoreForBoard(board, boardWithMoveList.head._3)
        if (maximum._2 < score)
          generateSolution(boardWithMoveList.tail, (moves, score))
        else
          generateSolution(boardWithMoveList.tail, maximum)
      }
      else {
        val validMoves = boardWithMoveList.head._1.validMoves
        val movesToGetToThisBoard = boardWithMoveList.head._2
        val nextBoardsWithMovesList: Set[(Board, List[Block], Int)] = validMoves.map {
          block: Block =>
            val newBoardWithScore: (Board, Int) = boardWithMoveList.head._1.clickOnBlock(block)
            (newBoardWithScore._1, block :: movesToGetToThisBoard, newBoardWithScore._2 + boardWithMoveList.head._3)
        }
        generateSolution(boardWithMoveList.tail ++ nextBoardsWithMovesList.toList, maximum)
      }
    }
    generateSolution(List((board, List(), 0)), (List(), 0))
  }

  private def getCorrectedScoreForBoard(board: Board, score: Score): Score = {
    if (board.hasNoColoredBlocks)
      score + 20000
    else
      score
  }
}

case class Block(colorCode: Char, x: Int, y: Int, filledWithColor: Boolean, var alreadyProcessed: Boolean)

case class Board(board: Vector[Vector[Block]]) {
  val upperY = board.size
  val upperX = board.head.size

  lazy val validMoves: Set[Block] = {
    def validMoves(fromBlocks: Set[Block], acc: Set[Block]): Set[Block] = {
      if (fromBlocks.isEmpty) acc
      else {
        val fieldOfBlock = getFieldForBlock(fromBlocks.head)
        val nextFromBlocks = fromBlocks.filter(!fieldOfBlock.contains(_))
        if (fieldOfBlock.size > 1)
          validMoves(nextFromBlocks, acc + fromBlocks.head)
        else
          validMoves(nextFromBlocks, acc)
      }
    }
    validMoves(allBlocksWithColor, Set())
  }

  lazy val allBlocksWithColor: Set[Block] = board.flatten.filter(_.filledWithColor).toSet

  def getFieldForBlock(block: Block): Set[Block] = {
    @tailrec
    def getFieldForBlock(blockStream: Stream[Block], acc: Set[Block]): Set[Block] = {
      val newStream = blockStream.dropWhile(b => acc.contains(b))
      if (newStream.isEmpty) {
        acc
      }
      else {
        val block = newStream.head
        val blocksAroundBlockWithSameColor: Set[Block] = getBlocksAroundBlockWithSameColor(block)
        getFieldForBlock(blockStream.tail ++ blocksAroundBlockWithSameColor.toStream, acc + block)
      }
    }

    getFieldForBlock(Stream(block), Set())
  }

  def clickOnBlock(block: Block): (Board, Int) = {
    val fieldToBeRemoved = getFieldForBlock(block)
    val score = getScoreIfFieldIsRemoved(fieldToBeRemoved)
    val boardWithFieldRemoved = removeField(fieldToBeRemoved)
    val boardWithEverythingDropped = boardWithFieldRemoved.dropBlocks
    (boardWithEverythingDropped, score)
  }

  def dropBlocks: Board = {
    def slideRow(block: Block): Board = {
      def slideBlock(block: Option[Block], board: Board): Board = {
        block match {
          case None => board
          case Some(Block(' ', _, _, _, _)) => board
          case Some(block) => {
            val blockLeftOf = board.getBlockLeftOf(block)
            val blockAboveOf = board.getBlockAbove(block)
            val boardWithBlocksSwitched = board.switchBlocks(block, blockLeftOf.get)
            slideBlock(blockAboveOf, boardWithBlocksSwitched)
          }
        }
      }
      slideBlock(Some(block), this).dropBlocks
    }

    def dropBlock(block: Block): Board = {
      val blockBelow = getBlockBelow(block)
      val boardWithBlocksSwitched = switchBlocks(block, blockBelow.get)
      boardWithBlocksSwitched.dropBlocks
    }

    val blockToSlideOption = findSlidableBlock
    if (blockToSlideOption.isDefined) {
      val blockToSlide = blockToSlideOption.get
      slideRow(blockToSlide)
    }
    else {
      val blockToDropOption = findDroppableBlock
      if (blockToDropOption.isDefined) {
        val blockToDrop = blockToDropOption.get
        dropBlock(blockToDrop)
      }
      else {
        this
      }
    }
  }

  def removeField(field: Set[Block]): Board = {
    Board(board.map {
      boardLine => boardLine.map {
        blockOfBoardLine =>
          if (field.contains(blockOfBoardLine)) Block(' ', blockOfBoardLine.x, blockOfBoardLine.y, filledWithColor = false, alreadyProcessed = false)
          else blockOfBoardLine
      }
    })
  }

  def hasNoColoredBlocks: Boolean = {
    allBlocksWithColor.isEmpty
  }

  private def getScoreIfFieldIsRemoved(removeableField: Set[Block]): Int = removeableField.size * removeableField.size * 10

  private def findDroppableBlock: Option[Block] = {
    for {
      boardLine <- board
      blockOnLine <- boardLine
      blockBelow = getBlockBelow(blockOnLine)
      if blockBelow.isDefined && !blockBelow.get.filledWithColor && blockOnLine.filledWithColor
    } {
      return Some(blockOnLine)
    }
    None
  }

  private def findSlidableBlock: Option[Block] = {
    def hasColoredBlockInLine(block: Block): Boolean = {
      allBlocksWithColor.exists(aBlock => aBlock.x == block.x)
    }

    for {
      boardLine <- board
      blockOnLine <- boardLine
      blockLeft = getBlockLeftOf(blockOnLine)
      if !getBlockBelow(blockOnLine).isDefined && blockLeft.isDefined && !blockLeft.get.filledWithColor && !hasColoredBlockInLine(blockLeft.get) && blockOnLine.filledWithColor
    } {
      return Some(blockOnLine)
    }
    None
  }

  def switchBlocks(block1: Block, block2: Block): Board = {
    val newBlock1 = Block(block2.colorCode, block1.x, block1.y, block2.filledWithColor, block2.alreadyProcessed)
    val newBlock2 = Block(block1.colorCode, block2.x, block2.y, block1.filledWithColor, block1.alreadyProcessed)
    val newBoardLine1 = board(block1.y).updated(block1.x, newBlock1)
    val newBoard = board.updated(block1.y, newBoardLine1)
    val newBoardLine2 = newBoard(block2.y).updated(block2.x, newBlock2)
    val retVal = Board(newBoard.updated(block2.y, newBoardLine2))
    retVal
  }

  private def getBlocksAroundBlockWithSameColor(block: Block): Set[Block] = {
    getBlocksAroundBlock(block).filter(_.colorCode == block.colorCode)
  }

  private def getBlocksAroundBlock(block: Block): Set[Block] = {
    val optionSetOfBlocksAround = Set(getBlockBelow(block), getBlockAbove(block), getBlockLeftOf(block), getBlockRightOf(block))
    optionSetOfBlocksAround.filter(_.isDefined).map(_.get)
  }

  private def getBlockBelow(block: Block) = getBlockAtPos(block.x, block.y + 1)

  private def getBlockAbove(block: Block) = getBlockAtPos(block.x, block.y - 1)

  private def getBlockLeftOf(block: Block) = getBlockAtPos(block.x - 1, block.y)

  private def getBlockRightOf(block: Block) = getBlockAtPos(block.x + 1, block.y)

  private def getBlockAtPos(x: Int, y: Int): Option[Block] = {
//    if (x < 0 || x >= upperX || y < 0 || y >= upperY) None
//    else Some(board(y)(x))
    board.lift(y).flatMap(_.lift(x))
  }

  def printBoard(): Unit = {
    board.map {
      boardLine => boardLine.map {
        block => block.colorCode match {
          case 'p' => print(Console.MAGENTA_B + " ")
          case 'y' => print(Console.YELLOW_B + " ")
          case 'g' => print(Console.GREEN_B + " ")
          case 'r' => print(Console.RED_B + " ")
          case ' ' => print(Console.BLACK_B + " ")
        }
      }
        println()
    }

    print(Console.RESET)
  }
}
