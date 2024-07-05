package utility.mbist
import utility.FileRegisters
import utility.mbist.Mbist.PipelineBaseNode
import java.util.regex.Pattern
import scala.collection.mutable

abstract class MbistFileGenerator {
  def generate(prefix: String): Unit
}

class MbistCsvGen(val intf: InterfaceInfo, val pip: MbistPipeline, val csvName: String) extends MbistFileGenerator {
  def generate(prefix: String): Unit = {
    val fileName = s"$prefix$csvName.csv"
    println(s"Generating $fileName")
    var contents = "\"INTF Name\", \"INTF Addr\", \"INTF Data\", \"INTF Array\", \"INTF Be\", \"Has TpSRAM\"\n"
    contents += intf.toString + '\n'
    contents += "\"SRAM Name\",\"SRAM Type\",\"SRAM array\",\"pipeline depth\",\"bitWrite\",\"bank addr\",\"selectOH width\",\"foundry\",\"SRAM Inst\"\n"
    val pipPath = pip.pathName.split("\\.").init.map(_ + ".").reduce(_ + _)
    val pattern = Pattern.compile(pipPath)
    def removeSubstring(str: String): String = {
      val matcher = pattern.matcher(str)
      matcher.replaceAll("")
    }
    val node = pip.myNode
    node.ramParamsBelongToThis
      .zip(node.array_id)
      .zip(node.array_depth)
      .foreach({
        case ((p, id), depth) =>
          contents += removeSubstring(p.holder.pathName) + p.nodeSuffix + ","
          contents += p.vname + ".v,"
          contents += id.toString + ","
          contents += (depth * 2 + p.latency).toString + ","
          contents += (if (p.bitWrite) "true," else "false,")
          contents += p.bankRange + ","
          contents += p.nodeNum + ","
          contents += p.foundry + ","
          contents += p.sramInst
          contents += "\n"
      })
    FileRegisters.add(fileName, contents)
  }
}

object MbistFileManager {
  private val generators = mutable.ArrayBuffer[MbistFileGenerator]()
  def addGenerator(gen: MbistFileGenerator): Unit = {
    generators += gen
  }
  def write(prefix: String): Unit = {
    generators.foreach(_.generate(prefix))
  }
}
