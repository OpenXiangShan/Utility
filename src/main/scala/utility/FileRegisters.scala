package utility

object FileRegisters {
  var files: Seq[(String, () => String)] = Nil

  def add(filename: String, contents: => String): Unit = {
    files = (filename, () => contents) +: files
  }

  def contains(filename: String): Boolean = {
    files.foldLeft(false)((t, s) => {s._1 == filename | t})
  }
}

