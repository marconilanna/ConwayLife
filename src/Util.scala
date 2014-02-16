/**
 * @author Marconi Lanna
 */

package util

case class Point(x: Int, y: Int) {
  require(x >= 0)
  require(y >= 0)
}

case class Size(width: Int, height: Int) {
  require(width > 0)
  require(height > 0)
}
