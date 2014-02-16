/**
 * @author Marconi Lanna
 */

package test.life

import life.QuadTreeBoard
import util.Point

class QuadTreeBoardTest extends org.scalatest.FunSuite {
  test("Spawn a cell") {
    val board = QuadTreeBoard(2, 2)
    val result = board.spawn(0, 0)
    assert(  result.isAlive(0, 0))
    assert(! result.isAlive(0, 1))
    assert(! result.isAlive(1, 0))
    assert(! result.isAlive(1, 1))
  }

  test("Kill a cell") {
    val cell = Point(6,6)

    val board = QuadTreeBoard(7, 7).spawn(cell)
    assert(board.isAlive(cell))

    val result = board.kill(cell)
    assert(! result.isAlive(cell))
  }
}
