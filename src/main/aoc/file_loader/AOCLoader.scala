package aoc.file_loader

object AOCLoader {
  def loadExample(dirname: String): List[String] = {
    FileLoader.loadFile(dirname + "/example.txt")
  }

  def loadActual(dirname: String): List[String] = {
    FileLoader.loadFile(dirname + "/test.txt")
  }
}
