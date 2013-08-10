package petrescuesagasolver

import akka.actor._
import scala.Some

case class ApplyDropRules(board: Board, score: Int, steps: List[(Block, Board)])
case class ApplySlideRules(board: Board, score: Int, steps: List[(Block, Board)])
case class Dropped(board: Board, score: Int, steps: List[(Block, Board)])
case class Slid(board: Board, score: Int, steps: List[(Block, Board)])
case class Crushed(board: Board, fieldSize: Int, score: Int, steps: List[(Block, Board)])
case class Move(block: Block, board: (Board, Int), steps: List[(Block, Board)])
case class Crush(block: Block, board: Board, score: Int, steps: List[(Block, Board)])
case class CrushBlocks(blocks: Set[Block], board: Board, score: Int, steps: List[(Block, Board)] )

class SimpleMoveActor extends Actor with ActorLogging {
  def moveScore(fieldSize: Int) = fieldSize * fieldSize * 10


  def crush(color: Char, crushBlocks: List[Block], board: Board, fieldSize: Int): (Board, Int) = {
    crushBlocks match {
      case blk :: blks => {
        if (board.board(blk.y)(blk.x).colorCode == color) {
          val newb = board.board.updated(blk.y, board.board(blk.y).updated(blk.x, Block(' ', blk.x, blk.y, false, false)))
          val neighBors = board.getBlocksAroundBlock(blk)
          val ret = crush(color, blks ++ neighBors, Board(newb), fieldSize + 1)
          ret
        } else {
          val ret = crush(color, blks, board, fieldSize)
          ret
        }
      }
      case Nil => (board, fieldSize)
    }
  }

  def crushBlocks(blocks: List[Block], board: Board, score: Int) : Board = {
    blocks match {
      case Nil => board
      case blk :: blks => {
        val newb = board.board.updated(blk.y, board.board(blk.y).updated(blk.x, Block(' ', blk.x, blk.y, false, false)))
        crushBlocks(blks, Board(newb), score)
      }
    }
  }

  // x = column, y = row
  def filterColumn(xColumn: Int, board: Board) = {
    {for {y <- board.upperY -1 to 0 by -1} yield board.board(y)(xColumn)}.map(_.colorCode).filter(_ != ' ')
  }
  def putNewColumn(x: Int, row: Seq[Char], board: Board): Board = {
    def putNewColumn(x: Int, row: Seq[Char], board: Board, y: Int): Board = {
      if (y >= 0) {
        val blockColor = row.lift(board.upperY - y - 1)
        blockColor match {
          case None => {
            val newBoard = board.board.updated(y, board.board(y).updated(x, Block(' ', x, y, false, false)))
            putNewColumn(x, row, Board(newBoard), y - 1)
          }
          case Some(color) => {
            val newBoard = board.board.updated(y, board.board(y).updated(x, Block(color, x, y, true, false)))
            putNewColumn(x, row, Board(newBoard), y - 1)
          }
        }
      }
      else {
        board
      }
    }
    putNewColumn(x, row, board, board.upperY - 1)
  }
  def dropFloatingBlocksDown(board: Board): Board = {
    def dropFloatingBlocksDownForColumn(x: Int, board: Board): Board = {
      if(x < board.upperX) dropFloatingBlocksDownForColumn(x + 1, putNewColumn(x, filterColumn(x, board), board))
      else board
    }
    dropFloatingBlocksDownForColumn(0, board)
  }

  def findEmptyColumn(board: Board) : Option[Block] = {
    val indexEmpty = board.board.last.indexWhere(_.colorCode == ' ')
    if(indexEmpty == -1) None
    else {
      val indexNextSolid = board.board.last.indexWhere(_.colorCode != ' ', indexEmpty)
      if(indexNextSolid == -1) None
      else Some(board.board.last(indexEmpty))
    }
  }
  def findLastNonEmpty(board: Board) : Option[Block] = {
    board.board.last.reverse.find(_.colorCode != ' ')
  }

  def removeColumn(board: Board, x: Int) : Board = {
    def removeColumn(board: Board, x: Int, row: Int) : Board = {
      if(row < board.upperY) {
        var newRowColors = board.board(row).filterNot(_.x == x).map(_.colorCode)
        val newRow : Vector[Block] = for{x <- (0 until board.upperX).toVector} yield {
          val color = newRowColors lift x
          color match {
            case None => {
              Block(' ', x, row, false, false)
            }
            case Some(color) => {
              Block(color, x, row, true, false)
            }
          }
        }
        removeColumn(Board(board.board.updated(row, newRow)), x, row + 1)
      }
      else board
    }
    removeColumn(board, x, 0)
  }

  def slideAllEmptyColumns(board: Board) : Board = {
    val emptyColumn = findEmptyColumn(board)
    emptyColumn match {
      case None => board
      case Some(block) => slideAllEmptyColumns(removeColumn(board, block.x))
    }
  }


  def receive = {
    case Move(block, (board, score), steps) => {
      val (newBoard, blocksCrushed) = crush(block.colorCode, List(block), board, 0)
      if(blocksCrushed > 1) {
        val newBoardDropped = dropFloatingBlocksDown(newBoard)
        val newBoardSlid = slideAllEmptyColumns(newBoardDropped)
        sender ! Stepped(newBoardSlid, score + moveScore(blocksCrushed), (block, board) :: steps)
      } else {
        sender ! Solution(board, score, steps)
      }
    }
  }
}

class MoveActor(master: ActorRef) extends Actor with ActorLogging {
  val crushActor = context.actorOf(Props(new CrushActor(self)))

  def receive = {
    case Move(block, (board, score), steps) => {
      crushActor ! Crush(block, board, score, (block, board) :: steps)
    }
    case Crushed(board, fieldSize, score, steps) => {
      if (fieldSize == 0) {
        master ! Solution(board, score, steps.tail)
      }
      else {
        master ! Stepped(board, score, (steps.head._1, board) :: steps.tail)
      }
    }
  }
}

class CrushActor(master: ActorRef) extends Actor {
  val dropActor = context.actorOf(Props[DropActor])
  val slideActor = context.actorOf(Props[SlideActor])

  def moveScore(fieldSize: Int) = fieldSize * fieldSize * 10


  def crush(color: Char, crushBlocks: List[Block], board: Board, fieldSize: Int): (Board, Int) = {
    crushBlocks match {
      case blk :: blks => {
        if (board.board(blk.y)(blk.x).colorCode == color) {
          val newb = board.board.updated(blk.y, board.board(blk.y).updated(blk.x, Block(' ', blk.x, blk.y, false, false)))
          val neighBors = board.getBlocksAroundBlock(blk)
          crush(color, blks ++ neighBors, Board(newb), fieldSize + 1)
        } else {
          crush(color, blks, board, fieldSize)
        }
      }
      case Nil => (board, fieldSize)
    }
  }

  def crushBlocks(blocks: List[Block], board: Board, score: Int) : Board = {
    blocks match {
      case Nil => board
      case blk :: blks => {
        val newb = board.board.updated(blk.y, board.board(blk.y).updated(blk.x, Block(' ', blk.x, blk.y, false, false)))
        crushBlocks(blks, Board(newb), score)
      }
    }
  }

  def receive = {
    case Crush(block, board, score, steps) => {
      val (newBoard, blocksCrushed) = crush(block.colorCode, List(block), board, 0)
      if(blocksCrushed > 1) {
//        sender ! Crushed(newBoard, blocksCrushed, score, steps)
        dropActor ! ApplyDropRules(board, score + moveScore(blocksCrushed), steps)
      } else {
        sender ! Crushed(board, 0, score, steps)
//        self ! PoisonPill
      }
    }
    case CrushBlocks(blocks, board, score, steps) => {
      val newBoard = crushBlocks(blocks.to[List], board, score)
      if(blocks.size == 0) {
        sender ! Crushed(board, 0, score, steps)
      } else {
        dropActor ! ApplyDropRules(board, score + moveScore(blocks.size), steps)
      }

    }
    case Dropped(board, score, steps) => {
      slideActor ! ApplySlideRules(board, score, steps)
    }
    case Slid(board, score, steps) => {
//      master ! Stepped(board, score, (steps.head._1, board) :: steps.tail)
      sender ! Crushed(board, 1, score, steps)
//      self ! PoisonPill
    }
  }
}

class DropActor extends Actor {
  // x = column, y = row
  def filterColumn(xColumn: Int, board: Board) = {
    {for {y <- board.upperY -1 to 0 by -1} yield board.board(y)(xColumn)}.map(_.colorCode).filter(_ != ' ')
  }
  def putNewColumn(x: Int, row: Seq[Char], board: Board): Board = {
    def putNewColumn(x: Int, row: Seq[Char], board: Board, y: Int): Board = {
      if (y >= 0) {
        val blockColor = row.lift(board.upperY - y - 1)
        blockColor match {
          case None => {
            val newBoard = board.board.updated(y, board.board(y).updated(x, Block(' ', x, y, false, false)))
            putNewColumn(x, row, Board(newBoard), y - 1)
          }
          case Some(color) => {
            val newBoard = board.board.updated(y, board.board(y).updated(x, Block(color, x, y, true, false)))
            putNewColumn(x, row, Board(newBoard), y - 1)
          }
        }
      }
      else {
        board
      }
    }
    putNewColumn(x, row, board, board.upperY - 1)
  }
  def dropFloatingBlocksDown(board: Board): Board = {
    def dropFloatingBlocksDownForColumn(x: Int, board: Board): Board = {
      if(x < board.upperX) dropFloatingBlocksDownForColumn(x + 1, putNewColumn(x, filterColumn(x, board), board))
      else board
    }
    dropFloatingBlocksDownForColumn(0, board)
  }

  def receive = {
    case ApplyDropRules(board, score, steps) => {
      sender ! Dropped(dropFloatingBlocksDown(board), score, steps)
    }
  }
}


class SlideActor extends Actor {

  def findEmptyColumn(board: Board) : Option[Block] = {
    val indexEmpty = board.board.last.indexWhere(_.colorCode == ' ')
    if(indexEmpty == -1) None
    else {
      val indexNextSolid = board.board.last.indexWhere(_.colorCode != ' ', indexEmpty)
      if(indexNextSolid == -1) None
      else Some(board.board.last(indexEmpty))
    }
  }
  def findLastNonEmpty(board: Board) : Option[Block] = {
    board.board.last.reverse.find(_.colorCode != ' ')
  }

  def removeColumn(board: Board, x: Int) : Board = {
    def removeColumn(board: Board, x: Int, row: Int) : Board = {
      if(row < board.upperY) {
        var newRowColors = board.board(row).filterNot(_.x == x).map(_.colorCode)
        val newRow : Vector[Block] = for{x <- (0 until board.upperX).toVector} yield {
          val color = newRowColors lift x
          color match {
            case None => {
              Block(' ', x, row, false, false)
            }
            case Some(color) => {
              Block(color, x, row, true, false)
            }
          }
        }
        removeColumn(Board(board.board.updated(row, newRow)), x, row + 1)
      }
      else board
    }
    removeColumn(board, x, 0)
  }

  def slideAllEmptyColumns(board: Board) : Board = {
    val emptyColumn = findEmptyColumn(board)
    emptyColumn match {
      case None => board
      case Some(block) => slideAllEmptyColumns(removeColumn(board, block.x))
    }
  }



  def receive = {
    case ApplySlideRules(board, score, steps) => {
      sender ! Slid(slideAllEmptyColumns(board), score, steps)
    }
  }
}
