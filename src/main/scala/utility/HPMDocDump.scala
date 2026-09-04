package utility

import java.io.PrintWriter
import java.nio.file.{Files, Path, Paths}

import scala.collection.mutable

object HPMDocDump {
  private case class EventSet(counterRange: String, events: Seq[PerfEventInfo])

  private val setOrder = Seq(
    "Frontend perfEvents Set",
    "Backend perfEvents Set",
    "MemBlock perfEvents Set",
    "L2 Cache perfEvents Set",
  )
  private val eventSets = mutable.Map.empty[String, EventSet]

  private def hpmCsvPath: Path =
    Paths.get(sys.env.getOrElse("NOOP_HOME", ".")).resolve("build").resolve("hpm.csv")

  private def csvEscape(value: String): String =
    "\"" + value.replace('\r', ' ').replace('\n', ' ').trim.replace("\"", "\"\"") + "\""

  private def writeCSV(): Unit = {
    val path = hpmCsvPath
    Files.createDirectories(path.getParent)
    val writer = new PrintWriter(path.toFile)
    try {
      writer.println(Seq("Event Set", "HPM Counters", "Event Number", "Event Name", "Description").map(csvEscape).mkString(","))
      setOrder.flatMap(name => eventSets.get(name).map(name -> _)).foreach { case (setName, eventSet) =>
        eventSet.events.zipWithIndex.foreach { case (event, index) =>
          val row = Seq(setName, eventSet.counterRange, index.toString, event.name, event.description)
          writer.println(row.map(csvEscape).mkString(","))
        }
      }
    } finally {
      writer.close()
    }
  }

  def register(setName: String, counterRange: String, events: Seq[PerfEventInfo]): Unit = {
    eventSets(setName) = EventSet(counterRange, events)
    writeCSV()
  }
}
