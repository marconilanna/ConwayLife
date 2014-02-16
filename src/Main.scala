/**
 * @author Marconi Lanna
 */

import life.{Life, PrettyPrinter}

import System.{currentTimeMillis => now}

import java.io.{File, FileWriter, OutputStreamWriter}
import java.text.SimpleDateFormat

object Main {
  def main(args: Array[String]) {
    if (args.isEmpty) print(usageInformation)
    else {
      val inputFile = args(0)

      val life = Life.fromFile(inputFile)

      val runs = parseInputParameters(args.drop(1))

      val folder = "output/" + outputFolder(fileName(inputFile))
      val folderCreated = new File(folder).mkdirs()

      if (!folderCreated) System.err.println(
          s"Could not create output folder $folder/. Defaulting to stdout.")

      var counter = 0

      runs foreach { n =>
        val start = now
        life.evolve(n)
        val end = now

        counter += n

        val writer = if (folderCreated) new FileWriter(f"$folder/$counter%04d.txt")
            else new OutputStreamWriter(System.out)

        PrettyPrinter(life, writer, lineNumbers = true)
        println(f"$n generation${if (n > 1) "s" else ""} run in ${end - start}%,dms.")
      }

      if (folderCreated) println(s"Output saved to the $folder/ folder.")
    }
  }

  private def parseInputParameters(input: Seq[String]): Seq[Int] = {
    val runs = input flatMap { n =>
      try Some(n.toInt).filter(_ > 0)
      catch {
        case _: NumberFormatException =>
          System.err.println(s"""Not a number, ignoring "$n".""")
          None
      }
    }

    if (runs.isEmpty) Seq(1) else runs
  }

  private def fileName(file: String) = new File(file).getName

  private val dateFormat = "yyyy-MM-dd_HH.mm.ss"
  private val formatter = new SimpleDateFormat(dateFormat)
  private def outputFolder(name: String, time: Long = now) = s"${formatter.format(time)}_$name"

  private val usageInformation =
"""Usage: java -jar life.jar pattern_file [runs...]

   pattern_file    A text file containing a life pattern.
                   See samples/readme.txt for input format description.

   runs...         The number of generations to run before taking a snapshot.
                   Snapshots are saved to the output/ folder.
                   An input of "2 2 2 2 2" will run the simulation for 10
                   generations taking 5 snapshots, one each other generation.
                   If not specified, defaults to 1.
"""
}
