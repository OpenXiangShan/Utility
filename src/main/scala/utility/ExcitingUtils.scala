/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package chisel3

import chisel3.internal.NamedComponent
import chisel3.util.experimental.BoringUtils
import scala.collection.mutable

object ExcitingUtils {

  object ConnectionType extends Enumeration {
    val Perf = Value("perf")
    val Func = Value("func")
    val Debug = Value("debug")
  }

  type ConnectionType = ConnectionType.Value
  val Perf = ConnectionType.Perf
  val Func = ConnectionType.Func
  val Debug = ConnectionType.Debug

  private def strToErrorMsg(str: String) = "\u001b[1m\u001b[;31m"+ str +"\u001b[0m"

  private class Connection
  (
    var connType: ConnectionType,
    var sourceModule: Option[String] = None,
    var sinkModule: Option[String] = None,
    var warned: Boolean = false
  ) {

    override def toString: String =
      s"type:[$connType] source location:[${sourceModule.getOrElse(strToErrorMsg("Not Found"))}]" +
        s" sink location:[${sinkModule.getOrElse(strToErrorMsg("Not Found"))}]"

    def isLegalConnection: Boolean = sourceModule.nonEmpty && sinkModule.nonEmpty
  }

  private val map = mutable.LinkedHashMap[String, Connection]()

  private val topWire: mutable.Map[String, Data] = mutable.Map()

  private var sources: mutable.Map[String, Data] = mutable.Map()

  def register(map: Map[String, Data]): Unit = {
    require(topWire.isEmpty, "[Error] ExcitingUtils can only register once")
    map.foreach { case (name, dataType) =>
      topWire.update(name, WireDefault(0.U.asTypeOf(dataType)))
    }
  }

  def addSource
  (
    component: Data,
    name: String,
    connType: ConnectionType = Func,
    disableDedup: Boolean = false,
    uniqueName: Boolean = false
  ): Unit = {
    val conn = map.getOrElseUpdate(name, new Connection(connType))
    require(conn.sourceModule.isEmpty, s"[ERROR] Signal |$name| has multiple sources")
    require(conn.connType == connType, s"[ERROR] Signal |$name| has conflict connection type")
    require(topWire.exists(_._1 == name), s"[ERROR] Signal |$name| not registered")
    conn.sourceModule = Some(component.parentModName)
    sources.update(name, component)
  }

  def addSink
  (
    component: Data,
    name: String,
    connType: ConnectionType = Func,
    disableDedup: Boolean = false,
    forceExists: Boolean = false
  ): Unit = {
    val conn = map.getOrElseUpdate(name, new Connection(connType))
    if (!conn.sinkModule.isEmpty && !conn.warned) {
      println(s"[WARN] Signal |$name| has multiple sinks")
      conn.warned = true
    }
    require(conn.connType == connType, s"[ERROR] Signal |$name| has conflict connection type")
    require(topWire.exists(_._1 == name), s"[ERROR] Signal |$name| not registered")
    conn.sinkModule = Some(component.parentModName)
    component := BoringUtils.bore(topWire(name))
  }

  def connectAndDisplay(): Unit = {
    var legal = true
    val buf = new mutable.StringBuilder()
    for ((name, conn) <- map){
      buf ++= s"Connection:[$name] $conn\n"
      if (!conn.isLegalConnection) legal = false
      else topWire(name) := BoringUtils.bore(sources(name))
    }
    print(buf)
    // require(legal, strToErrorMsg("Error: Illegal connection found!"))
  }

}
