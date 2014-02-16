/**
 * @author Marconi Lanna
 */

package life

import QuadTreeBoard.{Cell, leaf}

import quadtree.{GenericLeaf, QuadTree}
import util.{Point, Size}

class QuadTreeBoard private (board: QuadTree[Cell]) extends Board {
  def size = board.size

  def spawn(point: Point) = if (isAlive(point)) this
      else new QuadTreeBoard(board.set(point, leaf))

  def kill(point: Point) = if (! isAlive(point)) this
      else new QuadTreeBoard(board.delete(point))

  def isAlive(point: Point) = ! board.get(point).isEmpty

  def evolve(strategy: Point => Boolean): Board = {
    val end = board.midpoint * 2 - 1
    new QuadTreeBoard(traverse(emptyBoard, strategy, 0, 0, end, end, 0))
  }

  private def emptyBoard = QuadTree[Cell](size)

  private def traverse(quadtree: QuadTree[Cell], strategy: Point => Boolean
      , topLeft: Point, bottomRight: Point, level: Int): QuadTree[Cell] = {

    val (tl, br) = (topLeft, bottomRight)

    val insideBoard = tl.x < size.width && tl.y < size.height
    val singleCell = tl == br
    def born = strategy(tl)
    def traverseChildren = ! board.isEmpty(tl, level) || traverseEmpty(tl, br, level)

    if (insideBoard && singleCell && born) quadtree.set(tl, leaf)
    else if (insideBoard && ! singleCell && traverseChildren) {
      val midX = (tl.x + br.x) / 2
      val midY = (tl.y + br.y) / 2
      val l = level + 1
      traverse(
        traverse(
          traverse(
            traverse(quadtree
                , strategy, midX + 1, midY + 1, br.x, br.y, l)
                , strategy, tl.x    , tl.y    , midX, midY, l)
                , strategy, tl.x    , midY + 1, midX, br.y, l)
                , strategy, midX + 1, tl.y    , br.x, midY, l)
    } else quadtree
  }

  private def traverse(quadtree: QuadTree[Cell], strategy: Point => Boolean
      , x0: Int, y0: Int, x1: Int, y1: Int, level: Int): QuadTree[Cell] =
      traverse(quadtree, strategy, Point(x0, y0), Point(x1, y1), level)

  private def traverseEmpty(topLeft: Point, bottomRight: Point, level: Int): Boolean = {
    /*
     * To decide whether an empty quadrant needs to be traversed,
     * we need to check the neighboring quadrants at the cardinal
     * directions (N, E, S, W). There is no need to check the
     * neighbors in the ordinal directions (NE, SE, SW, NW) because
     * single cells at the diagonals cannot affect an empty quadrant
     * just by themselves, as the following diagram illustrates:
     *
     * ----------
     * |..|..|..|
     * |.*|N.|*.|
     * ----------
     * |.W|..|E.|
     * |..|..|..|
     * ----------
     * |.*|S.|*.|
     * |..|..|..|
     * ----------
     *
     * Where '*' represent live cells.
     * 'N', 'W', 'E', and 'S' are the cells visited below.
     */

    val (tl, br) = (topLeft, bottomRight)
    val (w, h) = (size.width - 1, size.height - 1)

    (tl.x > 0 && ! board.isEmpty(tl.x - 1, tl.y    , level)) || // W
    (tl.y > 0 && ! board.isEmpty(tl.x    , tl.y - 1, level)) || // N
    (br.x < w && ! board.isEmpty(br.x + 1, tl.y    , level)) || // E
    (br.y < h && ! board.isEmpty(tl.x    , br.y + 1, level))    // S
  }
}

object QuadTreeBoard extends BoardFactory {
  private object cell
  private type Cell = cell.type
  private val leaf = GenericLeaf(cell)

  def apply(size: Size) = new QuadTreeBoard(QuadTree[Cell](size))
}
