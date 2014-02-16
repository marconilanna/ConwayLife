/**
 * @author Marconi Lanna
 */

package test.life

import life.Parser
import util.Point

import scala.collection.mutable

import java.io.{FileReader, StringReader}

class ParserTest extends org.scalatest.FunSuite {
  test("Minimal board") {
    var called = false
    Parser.parse(new StringReader("1, 1")) { size =>
      assert(size.width === 1)
      assert(size.height === 1)
      assert(! called)
      called = true
    }{ point =>
      fail()
    }
    assert(called)
  }

  test("Board with a single live cell") {
    var called1 = false
    var called2 = false
    Parser.parse(new StringReader("1, 1\n*")) { size =>
      assert(size.width === 1)
      assert(size.height === 1)
      assert(! called1)
      called1 = true
    }{ point =>
      assert(point.x === 0)
      assert(point.y === 0)
      assert(! called2)
      called2 = true
    }
    assert(called1)
    assert(called2)
  }

  test("Board with a 'block', loaded from file") {
    var called = false
    var calls = 0
    val points = mutable.Set[Point]()
    val expected = mutable.Set(Point(1, 1), Point(1, 2), Point(2, 1), Point(2, 2))
    Parser.parse(new FileReader("samples/readme.txt")) { size =>
      assert(size.width === 4)
      assert(size.height === 4)
      assert(! called)
      called = true
    }{ point =>
      points += point
      calls = calls + 1
    }
    assert(called)
    assert(calls === 4)
    assert(points === expected)
  }

  test("Board with a 'glider', loaded from file") {
    var called = false
    var calls = 0
    val points = mutable.Set[Point]()
    val expected = mutable.Set(Point(1, 3), Point(2, 1), Point(2, 3), Point(3, 2), Point(3, 3))
    Parser.parse(new FileReader("samples/glider.txt")) { size =>
      assert(size.width === 50)
      assert(size.height === 50)
      assert(! called)
      called = true
    }{ point =>
      points += point
      calls = calls + 1
    }
    assert(called)
    assert(calls === 5)
    assert(points === expected)
  }
}
