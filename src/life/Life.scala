/**
 * @author Marconi Lanna
 */

package life

import util.{Point, Size}

import java.io.{FileReader, Reader, StringReader}

class Life private (board: Board) {
  private var current = board

  def size = current.size
  def isAlive(point: Point) = current.isAlive(point)
  def isAlive(i: Int, j: Int): Boolean = isAlive(Point(i, j))

  def evolve(generations: Int = 1) = {
    for (_ <- 0 until generations) {
      current = current.evolve(countNeighbors)
    }
  }

  /**
   * A trivial neighbor-counting Life strategy.
   * Please note that the Board is free to ignore this helper
   * method and implement its own evolve algorithm optimized
   * to its particular data structure.
   */
  private def countNeighbors(point: Point): Boolean = {
    /*
     * Cells may have 3, 5, or 8 neighbors if they are, respectively,
     * a corner, an edge, or an inner cell.
     */

    // In Scala, the end of a Range is exclusive, so we have to add 1 more
    val rangeX = math.max(0, point.x - 1) until math.min(point.x + 2, size.width)
    val rangeY = math.max(0, point.y - 1) until math.min(point.y + 2, size.height)

    val neighbors = rangeX.map { m =>
      rangeY count { n =>
        !(m == point.x && n == point.y) && isAlive(m, n)
      }
    }.sum

    /*
     * The rules for evolution are:
     * 1. Cells with two neighbors stay the same.
     * 2. Cells with three neighbors are born (or stay alive).
     * 3. All other cells die.
     */

    neighbors == 3 || (neighbors == 2 && isAlive(point))
  }
}

object Life {
  def fromFile(file: String, factory: BoardFactory = QuadTreeBoard): Life = {
    val board = parse(new FileReader(file), factory)
    new Life(board)
  }

  def apply(pattern: String, factory: BoardFactory = QuadTreeBoard): Life = {
    val board = parse(new StringReader(pattern), factory)
    new Life(board)
  }

  private def parse(input: Reader, factory: BoardFactory): Board = {
    var board: Board = null
    Parser.parse(input) {
      size => board = factory(size)
    }{
      point => board = board.spawn(point)
    }
    board
  }
}

trait Board {
  def size: Size

  def spawn(point: Point): Board
  def spawn(i: Int, j: Int): Board = spawn(Point(i, j))

  def kill(point: Point): Board
  def kill(i: Int, j: Int): Board = kill(Point(i, j))

  def isAlive(point: Point): Boolean
  def isAlive(i: Int, j: Int): Boolean = isAlive(Point(i, j))

  def evolve(strategy: Point => Boolean): Board
}

trait BoardFactory {
  def apply(size: Size): Board
  def apply(width: Int, height: Int): Board = apply(Size(width, height))
}
