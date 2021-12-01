package aoc.file_loader

import scala.io.Source

object FileLoader {
  def loadFile(path: String): List[String] = {
    val fullPath = System.getProperty("user.dir") + "/src/main/aoc/" + path

    val file = Source.fromFile(fullPath)
    val data = file.getLines().toList

    file.close()
    data
  }
}
