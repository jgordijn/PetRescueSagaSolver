package petrescuesagasolver

import scala.annotation.tailrec
import akka.actor._
import java.util.UUID
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import akka.util.Timeout

import scala.Some
import akka.routing.RoundRobinRouter

case object Start
case class Solution(board: Board, score: Int, steps: List[(Block, Board)])
case class Stepped(board: Board, score: Int, clickedBlocks: List[(Block, Board)])

case class Start(board: Board, score: Int, steps: List[(Block, Board)])

object MainAkka extends App {
  val system = ActorSystem("MySystem")
  val master = system.actorOf(Props[Master], name = "master")
  println("sending start")
  system.scheduler.scheduleOnce(500.seconds) {
    println("SHUTDOWN!!!!")
    system.shutdown()
  }
  master ! Start
}

class MoveMemory(master: ActorRef) extends Actor {
  val moveActor = context.actorOf(Props[SimpleMoveActor].withRouter(RoundRobinRouter(nrOfInstances = 10)), name = "mover")
  var memoryMap = collection.mutable.Map[(Block, Board), (Board, Int, List[(Block, Board)])]()


  def receive = {
    case Move(block, (board, score), steps) => {
      val possibleSolution = memoryMap get(block, board)
      if(possibleSolution isDefined) {
        println("HIT")
        val solution = possibleSolution.get
        sender ! Solution(solution._1, score + solution._2, solution._3 ++ steps)
      }  else {
        moveActor ! Move(block, (board, score), steps)
      }
    }
    case Solution(board, score, steps) => {
      if(!steps.isEmpty) memoryMap put ((steps.head._1, board), (board, score, steps))
      master ! Solution(board, score, steps)
    }
//    case Stepped(board, score, steps) =>  {
//      memoryMap put ((steps.head._1, board), (board, score, steps))
//      master ! Stepped(board, score, steps)
//    }
    case x => master ! x

  }
}

class Master extends Actor {
  val reader = context.actorOf(Props[PetRescueSagaBoardReader], name = "reader")

//  val fieldActor = context.actorOf(Props[FieldActor].withRouter(RoundRobinRouter(nrOfInstances = 10)), name = "field")
//  val moveActor = context.actorOf(Props(new MoveMemory(self)), name = "memoryActor")
//  val moveActor = context.actorOf(Props[SimpleMoveActor].withRouter(RoundRobinRouter(nrOfInstances = 10)), name = "mover")
  val moveActor = context.actorOf(Props[SimpleMoveActor], name = "mover")

  var maxScore = 0
  var startBoard: Board = null
  var outStandingRequests = 0
  var maxSolution: Solution = null

  def receive = {
    case Start(Board(b), score: Int, steps: List[(Block, Board)]) => {
      for {
        boardLine <- b
        block <- boardLine
      } {
        if(block.colorCode != ' ') {
          moveActor ! Move(block, (Board(b), score), steps)
          outStandingRequests = outStandingRequests + 1
        }
      }
      println("1: " + outStandingRequests)
    }

    case ReadResult(board) => {
      println("STARTING!")
      board.printBoard()
      startBoard = board
      self ! Start(board, 0, Nil)
    }
    case Start => {
      reader ! Read("doc/level2.txt")
    }
    case Stepped(b, s, steps) => {
      outStandingRequests = outStandingRequests - 1
      println("2: " + outStandingRequests)
      self ! Start(b, s, steps)
    }
    case Solution(Board(bs), score, steps) => {
      outStandingRequests = outStandingRequests - 1
      println("3: " + outStandingRequests)
      if (score > maxScore) {
        println("NEW MAX SOLUTION")
        startBoard.printBoard()
        steps.reverse.map( (block: (Block, Board)) => {
          println("MOVE " + block._1)
          block._2.printBoard()
        })
        println(s"\nScore: $score")
        println(bs)
        Board(bs).printBoard()
        maxSolution = Solution(Board(bs), score, steps)
      }
      maxScore = Math.max(score, maxScore)
      if(outStandingRequests == 0) {
        println("This is the SOLUTION: " + maxScore)
        startBoard.printBoard()
        maxSolution.steps.reverse.map( (block: (Block, Board)) => {
          println("MOVE " + block._1)
          block._2.printBoard()
        })
        println(s"\nScore: $maxScore")
        println(bs)
        maxSolution.board.printBoard()
        context.system.shutdown();
      }
    }
    case x => throw new UnsupportedOperationException(s"Do not know: $x")
  }
}

object Main extends App {
  val board = PetRescueSagaBoardReader.readBoard("doc/level3.txt")
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
      val moved = board.clickOnBlock(moves.head)
      println(s"Making move: ${moves.head} (${moved._2}})")
      val newBoard = moved._1
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

  def getBlocksAroundBlock(block: Block): Set[Block] = {
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
