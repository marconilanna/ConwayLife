/**
 * @author Marconi Lanna
 */

package test.life

import life.Life

class LifeTest extends org.scalatest.FunSuite {
  val block = """4 4
1 1
**
**"""

  val glider = """6 6
1 1
.*
..*
***"""

  test("Loading a block in a 4x4 grid") {
    val life = Life(block)

    assert(  life.isAlive(1, 1))
    assert(  life.isAlive(1, 2))
    assert(  life.isAlive(2, 1))
    assert(  life.isAlive(2, 2))

    assert(! life.isAlive(0, 0))
    assert(! life.isAlive(0, 1))
    assert(! life.isAlive(0, 2))
    assert(! life.isAlive(0, 3))

    assert(! life.isAlive(1, 0))
    assert(! life.isAlive(1, 3))
    assert(! life.isAlive(2, 0))
    assert(! life.isAlive(2, 3))

    assert(! life.isAlive(3, 0))
    assert(! life.isAlive(3, 1))
    assert(! life.isAlive(3, 2))
    assert(! life.isAlive(3, 3))
  }

  test("A block in a 4x4 grid must evolve to itself") {
    val expected = Life(block)
    val result = Life(block)
    result.evolve()

    for (i <- 0 until expected.size.width) {
      for (j <- 0 until expected.size.height) {
        assert(result.isAlive(i, j) === expected.isAlive(i, j))
      }
    }
  }

  test("A glider must travel diagonally at c/4 speed.") {
    val expected = Life(glider)
    val result = Life(glider)
    result.evolve(4)

    for (i <- 0 until expected.size.width - 1) {
      for (j <- 0 until expected.size.height - 1) {
        assert(result.isAlive(i + 1, j + 1) === expected.isAlive(i, j))
      }
    }
  }
}
