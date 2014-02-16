/**
 * @author Marconi Lanna
 */

package test.quadtree

import quadtree.{GenericLeaf, QuadTree}
import util.Point

class QuadTreeTest extends org.scalatest.FunSuite {
  val leaf = GenericLeaf(42)

  test("New quadtree must be empty") {
    val qt = QuadTree[Int](100, 100)
    assert(qt.isEmpty)
  }

  test("Invalid dimensions must throw IllegalArgumentException") {
    intercept[IllegalArgumentException] {
      QuadTree[Int](0, 11)
    }

    intercept[IllegalArgumentException] {
      QuadTree[Int](22, -1)
    }
  }

  test("Invalid coordinates must throw IllegalArgumentException") {
    val qt = QuadTree[Int](42, 1492)

    intercept[IllegalArgumentException] {
      qt.get(42, 666)
    }

    intercept[IllegalArgumentException] {
      qt.set(1, 1500, leaf)
    }

    intercept[IllegalArgumentException] {
      qt.delete(5, -1)
    }
  }

  test("Get on an empty quadtree must return nothing") {
    val qt = QuadTree[Int](512, 512)
    val result = qt.get(65, 127)
    assert(result.isEmpty)
  }

  test("Populated quadtree must not be empty") {
    val qt = QuadTree[Int](255, 257)
    val result = qt.set(64, 64, leaf)
    assert(! result.isEmpty)
  }

  test("Get on an populated quadtree must return something") {
    val point = Point(126, 126)
    val qt = QuadTree[Int](127, 127)
    val result = qt.set(point, leaf)
    assert(result.get(point) === leaf.get(point))
  }

  test("Setting a point must not affect its neighbors") {
    val qt = QuadTree[Int](256, 256)
    val result = qt.set(33, 15, leaf)
    assert(result.get(32, 14).isEmpty)
    assert(result.get(32, 15).isEmpty)
    assert(result.get(32, 16).isEmpty)
    assert(result.get(33, 14).isEmpty)
    assert(result.get(33, 16).isEmpty)
    assert(result.get(34, 14).isEmpty)
    assert(result.get(34, 15).isEmpty)
    assert(result.get(34, 16).isEmpty)
  }

  test("Updating a node must change its content") {
    val point = Point(1999, 1999)
    val qt = QuadTree[Int](2000, 2000).set(point, leaf)

    val expected = GenericLeaf(13)
    val result = qt.set(point, expected)
    assert(! result.isEmpty)
    assert(result.get(point) === expected.get(point))
  }

  test("Delete on empty quadtree must not throw exception") {
    val qt = QuadTree[Int](5, 5)
    val result = qt.delete(0, 0)
    assert(result.isEmpty)
  }

  test("Deleted node must be empty") {
    val point = Point(2, 2)
    val qt = QuadTree[Int](3, 3).set(point, leaf)
    val result = qt.delete(point)
    assert(result.isEmpty)
    assert(result.get(point).isEmpty)
  }

  test("isEmpty at different levels") {
    val point = Point(14, 14)
    val qt = QuadTree[Int](16, 16)
    val result = qt.set(point, leaf)
    assert(! result.isEmpty(point, 0))
    assert(! result.isEmpty(point, 1))
    assert(! result.isEmpty(point, 2))
    assert(! result.isEmpty(point, 3))

    val neighbor = Point(13, 13)
    assert(! result.isEmpty(neighbor, 0))
    assert(! result.isEmpty(neighbor, 1))
    assert(! result.isEmpty(neighbor, 2))
    assert(result.isEmpty(neighbor, 3))
  }
}
