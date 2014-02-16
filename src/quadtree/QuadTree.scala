/**
 * A simple quadtree implementation.
 *
 * http://en.wikipedia.org/wiki/Quadtree
 *
 * @author Marconi Lanna
 */

package quadtree

import util.{Point, Size}

import scala.annotation.switch

trait QuadTree[+A] {
  def size: Size
  def midpoint: Int
  def isEmpty: Boolean

  /**
   * Whether the quadrant at level `level` (zero being the root)
   * that contains the point `(i, j)` is empty.
   */
  def isEmpty(point: Point, level: Int): Boolean
  def isEmpty(i: Int, j: Int, level: Int): Boolean = isEmpty(Point(i, j), level)

  def get(point: Point): Option[A]
  def get(i: Int, j: Int): Option[A] = get(Point(i, j))

  def set[B >: A](point: Point, value: Leaf[B]): QuadTree[B]
  def set[B >: A](i: Int, j: Int, value: Leaf[B]): QuadTree[B] = set(Point(i, j), value)

  def delete(point: Point) = set(point, Empty)
  def delete(i: Int, j: Int) = set(i, j, Empty)

  def getOrElse[B >: A](default: => QuadTree[B]): QuadTree[B] = this
}

object QuadTree {
  def apply[A](size: Size): QuadTree[A] = Node(size, Point(0, 0))
  def apply[A](width: Int, height: Int): QuadTree[A] = apply(Size(width, height))

  private[quadtree] val empty: Seq[QuadTree[Nothing]] = Array(Empty, Empty, Empty, Empty)
}

trait Leaf[+A] extends QuadTree[A] {
  // To avoid creating a new `Size` object every time `size` is called
  private val s = Size(1, 1)
  def size = s
  def midpoint = throw new UnsupportedOperationException
  def isEmpty = false

  def isEmpty(point: Point, level: Int) = {
    assert(level >= 0)
    isEmpty
  }

  def set[B >: A](point: Point, value: Leaf[B]) = value
}

case class GenericLeaf[+A](value: A) extends Leaf[A] {
  // To avoid creating a new `Some` object every time `get` is called
  private val v = Some(value)
  def get(point: Point) = v
  override def toString = s"[$value]"
}

object Empty extends Leaf[Nothing] {
  override def size = throw new UnsupportedOperationException
  override def isEmpty = true

  def get(point: Point) = None

  override def set[B](point: Point, value: Leaf[B]) = throw new UnsupportedOperationException

  override def getOrElse[B](default: => QuadTree[B]): QuadTree[B] = default

  override def toString = "[]"
}

case class Node[+A] private[quadtree]
    ( size : Size
    , private val topLeft   : Point
    , private val quadrants : Seq[QuadTree[A]] = QuadTree.empty
    )
    extends QuadTree[A] {

  assert(quadrants.length == 4)

  /**
   * Determines to which quadrant a point belongs, as follows:
   *
   * ---------
   * | 0 | 1 |
   * ---------
   * | 2 | 3 |
   * ---------
   */
  private def index(point: Point): Int = {
    require(point.x >= topLeft.x)
    require(point.y >= topLeft.y)
    require(point.x <  topLeft.x + size.width)
    require(point.y <  topLeft.y + size.height)

    (if (point.x < topLeft.x + midpoint) 0 else 1) +
    (if (point.y < topLeft.y + midpoint) 0 else 2)
  }

  private def lastLevel = midpoint == 1

  /*
   * A quadtree always represents a square with sides 2^k, k > 0.
   * The quadrants are half that side, regardless of how many elements they actually contain.
   */
  val midpoint = {
    val maxSide = math.max(size.width, size.height)
    var side = 2
    while (side < maxSide) side *= 2
    side / 2
  }

  def isEmpty = quadrants.forall(_.isEmpty)

  def isEmpty(point: Point, level: Int) = {
    assert(level >= 0)
    if (level == 0) isEmpty
    else quadrants(index(point)).isEmpty(point, level - 1)
  }

  def get(point: Point): Option[A] = quadrants(index(point)).get(point)

  def set[B >: A](point: Point, value: Leaf[B]): QuadTree[B] = {
    val idx = index(point)

    val update: QuadTree[B] = if (lastLevel) value else quadrants(idx).getOrElse {
      val (x, y) = (topLeft.x, topLeft.y)
      val (w, h) = (size.width, size.height)
      val mp = midpoint

      val (i, j, m, n) = (idx: @switch) match {
        case 0 => (     x, y     ,     mp,     mp)
        case 1 => (mp + x, y     , w - mp,     mp)
        case 2 => (     x, y + mp,     mp, h - mp)
        case 3 => (mp + x, y + mp, w - mp, h - mp)
      }

      Node(Size(m, n), Point(i, j))
    }.set(point, value)

    copy(quadrants = quadrants.updated(idx, if (update.isEmpty) Empty else update))
  }
}
