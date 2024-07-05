package utility

import java.io.{File, FileWriter}

object FileRegisters {
  var files: Seq[(String, String, () => String)] = Nil

  def add(filename: String, contents: => String): Unit = {
    add("", filename, contents)
  }

  def add(filedir:String, filename:String, contents: => String) :Unit = {
    val fn = () => contents
    files = (filedir, filename, fn) +: files
  }

  def contains(filename: String): Boolean = {
    files.count(_._2 == filename) != 0
  }

  def write(fileDir: String = "./build", filePrefix: String = ""): Unit = {
    files.foreach { case (fd, fn, fc) =>
      writeOutputFile(fileDir, fd, filePrefix + fn, fc())
    }
  }

  def writeOutputFile(td: String, fd: String, fn:String, fc: String): File = {
    val dirStr = if(fd == "") td else s"$td/$fd"
    val dir = new File(dirStr)
    if(!dir.exists()) require(dir.mkdirs())
    val fname = s"$dirStr/$fn"
    val f = new File(fname)
    val fw = new FileWriter(f)
    fw.write(fc)
    fw.close()
    f
  }
}

