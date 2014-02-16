/**
 * @author Marconi Lanna
 */

package life

import util.Size

import java.io.{OutputStreamWriter, PrintWriter, Writer}

object PrettyPrinter {
  def apply(life: Life, writer: Writer = new OutputStreamWriter(System.out)
      , live: String = "*", dead: String = ".", lineNumbers: Boolean = false) = {

    val out = new PrintWriter(writer)
    val Size(width, height) = life.size

    for (j <- 0 until height) {

      if (lineNumbers) out.print(f"$j%5d ")

      for (i <- 0 until width) {
        out.print(if (life.isAlive(i, j)) live else dead)
      }

      out.println()
    }

    out.flush()
  }
}
