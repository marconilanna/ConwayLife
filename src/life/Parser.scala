/**
 * @author Marconi Lanna
 */

package life

import util.{Point, Size}

import java.io.{BufferedReader, Reader}
import java.text.ParseException

object Parser {
  def parse(input: Reader)(setSize: Size => Unit)(setPoint: Point => Unit) = {
    val parseInts = """(\d+)\D+(\d+)""".r

    // Let's keep side effects contained to the smallest scope possible
    object line {
      private val reader = new BufferedReader(input)
      private var line = ""
      def l = line
      def trim = line.trim
      def isEmpty = trim.isEmpty
      def isComment = {
        val t = trim
        ! t.isEmpty && t(0) == '#'
      }
      def read() = {line = reader.readLine(); line}
      def close() = reader.close()
    }

    // Skip comments and blank lines
    while (line.read() != null && (line.isEmpty || line.isComment)) {}

    // Parse width and height
    val size: Size = line.l match {
      case parseInts(w, h) => Size(w.toInt, h.toInt)
      case _ => throw new ParseException(
          "Invalid input format: no width or height specified", -1)
    }

    setSize(size)

    line.read()

    if (line.l != null) {
      // Parse optional x and y
      val topLeft: Point = line.l match {
        case parseInts(x, y) => line.read(); Point(x.toInt, y.toInt)
        case _ => Point(0, 0)
      }

      var j = 0
      while (line.l != null && topLeft.y + j < size.height) {
        if (! line.isComment) {
          line.l.zipWithIndex foreach {
            case (c, i) =>
              if (c != ' ' && c != '.' && topLeft.x + i < size.width)
                setPoint(Point(topLeft.x + i, topLeft.y + j))
          }
          j = j + 1
        }
        line.read()
      }
    }
    line.close()
  }
}
