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

package utility

import chisel3._
import chisel3.experimental.StringParam
import chisel3.util._

// class DummyCntBundle extends Bundle {
//   val cnt = UInt(64.W)
// }

trait HasMyMapUtils extends HasTableUtils {
}

// TODO: change Record to Data?
class MyMap[T <: Data, U <: Data](
  val envInFPGA: Boolean,
  val mapName: String,
  val keyHw: Vec[T],
  val valueHw: Vec[U]
) extends HasMyMapUtils {

  val vec_size = keyHw.size
  require(keyHw.size == valueHw.size, "KeyHw should have the same size with valueHw")
  val cmap_name = s"map_$mapName"

  def get_cols[W <: Data](hw: Vec[W], port: String) = {
    val cols = get_columns(hw, port).map(_.field)
    val bundle_size = cols.size / vec_size
    val type_cols = cols.take(bundle_size)
    (cols, bundle_size, type_cols)
  }
  val (key_cols, key_bundle_size, key_type_cols) = get_cols(keyHw, "key")
  val (value_cols, value_bundle_size, value_type_cols) = get_cols(valueHw, "value")

  def get_init: String = {
    val init =
      s"""
         |// $cmap_name 's key/value type
         |struct ${cmap_name}_key_t {
         |  ${key_type_cols.map(c => s"uint64_t ${c};").mkString("\n  ")}
         |  bool operator<(const ${cmap_name}_key_t &that) const {
         |    ${key_type_cols.map(c => s"if (${c} != that.${c}) return ${c} < that.${c};").mkString("\n    ")}
         |    return false;// equal
         |  }
         |};
         |struct ${cmap_name}_value_t {
         |  ${value_type_cols.map(c => s"uint64_t ${c};").mkString("\n  ")}
         |
         |  ${cmap_name}_value_t operator+(const ${cmap_name}_value_t that) {
         |    ${cmap_name}_value_t res;
         |    ${value_type_cols.map(c => s"res.${c} = this->${c} + that.${c};").mkString("\n    ")}
         |    return res;
         |  }
         |  void operator+=(const ${cmap_name}_value_t that) {
         |    ${value_type_cols.map(c => s"this->${c} += that.${c};").mkString("\n    ")}
         |  }
         |};
         |std::map<${cmap_name}_key_t, ${cmap_name}_value_t> $cmap_name;
         |
         |""".stripMargin
    init
  }

  def get_insert: String = {
    def core_insert(i: Integer): String = {
      val key_cols_slice = key_cols.slice(i*key_bundle_size, (i+1)*key_bundle_size)
      val value_cols_slice = value_cols.slice(i*value_bundle_size, (i+1)*value_bundle_size)
      println(s"key_cols_slice")
      s"""
         |  if (en_$i) {
         |    key = {${key_cols_slice.mkString(", ")}};
         |    value = {${value_cols_slice.mkString(", ")}};
         |
         |    if ($cmap_name.find(key) == $cmap_name.end()) {
         |  //    std::cout << std::hex
         |  //              << "Inserting new key: " << ${key_type_cols.map("key." + _).mkString(" << '_' << ")}
         |  //              << std::dec
         |  //              << " value: " << ${value_type_cols.map("value." + _).mkString("  << '_' << ")}
         |  //              << std::endl;
         |      $cmap_name[key] = value;
         |    } else {
         |  //    std::cout << std::hex
         |  //              <<"Inserting old key: " << ${key_type_cols.map("key." + _).mkString(" << '_' << ")}
         |  //              << std::dec
         |  //              << " value: " << ${value_type_cols.map("value." + _).mkString("  << '_' << ")}
         |  //              << std::endl;
         |      $cmap_name[key] += value;
         |    }
         |  }
         |"""
    }
    val insert =
      s"""
         |extern "C" void ${mapName}_map_write(
         |  ${key_cols.map(c => "uint64_t " + c).mkString("", ",\n  ", ",")}
         |  ${value_cols.map(c => "uint64_t " + c).mkString("", ",\n  ", ",")}
         |  ${(0 until vec_size).map(i => s"bool en_$i").mkString("", ",\n  ", ",")}
         |  char *site
         |) {
         |  // if(!dump || !enable_dump_$mapName) return;
         |  ${cmap_name}_key_t key;
         |  ${cmap_name}_value_t value;
         |  ${(0 until vec_size).map(core_insert(_)).mkString("\n")}
         |}
         |""".stripMargin
    val insert_dummy =
      s"""
         |extern "C" void ${mapName}_map_write(
         |  ${key_cols.map(c => "uint64_t " + c).mkString("", ",\n  ", ",")}
         |  ${value_cols.map(c => "uint64_t " + c).mkString("", ",\n  ", ",")}
         |  char *site
         |) {
         |}
         |""".stripMargin

    if (envInFPGA) insert_dummy
    else insert
  }

  def get_print: String = {
    val print_map =
      s"""
         |extern "C" void print_${cmap_name}() {
         |  printf("MAP: $cmap_name\\n");
         |  for (auto it = $cmap_name.begin(); it != $cmap_name.end(); it++) {
         |    std::cout << std::hex
         |      << ${key_type_cols.map("it->first." + _).mkString(" << '_' << ")}
         |      << std::dec << " -> "
         |      << ${value_type_cols.map("it->second." + _).mkString("  << '_' << ")}
         |      << std::endl;
         |  }
         |}
         |""".stripMargin
    val print_map_dummy =
      s"""
         |extern "C" void print_${cmap_name}() {}
         |""".stripMargin

    if (envInFPGA) print_map_dummy
    else print_map
  }

  def get_save_file: String = {
    val save_to_file =
      s"""
         |extern "C" void save_${cmap_name}(const char *filename_prefix) {
         |  char name_buf[1024];
         |  strcpy(name_buf, filename_prefix);
         |  strcat(name_buf, ".${cmap_name}.csv");
         |
         |  std::ofstream file(name_buf);
         |  if (!file.is_open()) {
         |    std::cout << "Error opening file: " << name_buf << std::endl;
         |    return;
         |  }
         |
         |  file << ${key_type_cols.map('"' + _ + '"').mkString("<< ',' << ")}
         |       << "->"
         |       << ${value_type_cols.map('"' + _ + '"').mkString("<< ',' << ")}
         |       << std::endl;
         |  for (auto it = $cmap_name.begin(); it != $cmap_name.end(); it++) {
         |    file << std::hex
         |      << ${key_type_cols.map("it->first." + _).mkString(" << ',' << ")}
         |      << std::dec << "->"
         |      << ${value_type_cols.map("it->second." + _).mkString("  << ',' << ")}
         |      << std::endl;
         |  }
         |
         |  file.close();
         |}
         |""".stripMargin
    val dummy =
      s"""
         |extern "C" void save_${cmap_name}(const char *filename_prefix) {
         |
         |}
         |""".stripMargin
    if (envInFPGA) dummy
    else save_to_file
  }

  def log(key: Vec[T], value: Vec[U], en: Vec[Bool], site: String = "", clock: Clock, reset: Reset): Unit = {
    if(!envInFPGA){
      val writer = Module(new MyMapWriteHelper(mapName, keyHw, valueHw, site))
      writer.io.clock := clock
      writer.io.reset := reset
      writer.io.en := en
      writer.io.key := key
      writer.io.value := value
    } else {
      println("Warning: ChiselMap is disabled, please enable it by setting ChiselMap.init(true)")
    }
  }

  def log(key: Vec[Valid[T]], value: Vec[U], site: String, clock: Clock, reset: Reset): Unit = {
    log(VecInit(key.map(_.bits)), value, VecInit(key.map(_.valid)), site, clock, reset)
  }

  def log(key: Vec[Valid[T]], value: U, site: String, clock: Clock, reset: Reset): Unit = {
    val valueVec = Wire(Vec(key.size, chiselTypeOf(value)))
    valueVec.map(_ := value)
    log(VecInit(key.map(_.bits)), valueVec, VecInit(key.map(_.valid)), site, clock, reset)
  }
}

private class MyMapWriteHelper[T <: Data, U <: Data](
  mapName: String,
  keyHw: Vec[T],
  valueHw: Vec[U], // TODO: support normal Bundle
  site: String
)
  extends BlackBox(
    Map(
      "site" -> StringParam(site)
    )
  )
  with HasBlackBoxInline
  with HasMyMapUtils
{
  val width = keyHw.size
  require(keyHw.size == valueHw.size, "keyVec and valueVec should have the same size")
  val io = IO(new Bundle() {
    val clock = Input(Clock())
    val reset = Input(Reset())
    val key = Input(keyHw.cloneType)
    val value = Input(valueHw.cloneType)
    val en = Input(Vec(width, Bool()))
  })
  // require(io.value.head.getWidth <= 64, "valueVec width should be less than 64 bits")
  // require(io.value.head.elements.map(_._2.getWidth <= 64).reduce(_ && _), "value's elements width should be less than 64 bits")

  val moduleName = s"${mapName}MapWriter"
  val dpicFunc = s"${mapName}_map_write"

  // construct dpic api and verilog module ports
  def genDPI[W <: Data](hw: Vec[W], prefix: String, portName: String) = {
    val table = get_columns(hw, prefix = prefix)
    val dpicInputs = table.map(_.field)
    val vPorts = table.map(_.refPort).distinct.map(r => s"input [${r.width - 1}:0] ${r.ref}")
    val vIO = table.map(_.refPort).distinct.map(r => s"${r.ref}")
    (table, dpicInputs, vPorts, vIO)
  }
  val (key_table, key_dpicInputs, key_vPorts, key_vIO) = genDPI(io.key, "key", "key")
  val (value_table, value_dpicInputs, value_vPorts, value_vIO) = genDPI(io.value, "value", "value")
  val (en_table, en_dpicInputs, en_vPorts, en_vIO) = genDPI(io.en, "en", "en")

  val verilog =
    s"""
       |import "DPI-C" function void $dpicFunc
       |(
       |${key_dpicInputs.map(x => "input longint " + x).mkString("  ", ",\n  ", ",")}
       |${value_dpicInputs.map(x => "input longint " + x).mkString("  ", ",\n  ", ",")}
       |${en_dpicInputs.map(x => "input longint " + x).mkString("  ", ",\n  ", ",")}
       |  input string site
       |);
       |
       |module $moduleName(
       |${key_vPorts.mkString("  ", ",\n  ", ",")}
       |${value_vPorts.mkString("  ", ",\n  ", ",")}
       |${en_vPorts.mkString("  ", ",\n  ", ",")}
       |  input clock,
       |  input reset // put it at the end to avoid NullPort warning
       |);
       |  parameter string site = "undefined";
       |
       |  always@(posedge clock) begin
       |    if(${en_vIO.mkString("(", " | ", ")")} && !reset) begin
       |      $dpicFunc(
       |        ${key_table.map(_.vExpr).mkString("", ", ", ",\n")}
       |        ${value_table.map(_.vExpr).mkString("", ", ", ",\n")}
       |        ${en_table.map(_.vExpr).mkString("", ", ", ",\n")}
       |        site);
       |    end
       |  end
       |endmodule
       |""".stripMargin

  setInline(s"$moduleName.v", verilog)

  override def desiredName: String = moduleName
}

object ChiselMap {

  private val table_map = scala.collection.mutable.Map[String, MyMap[_, _]]()
  private var enable = false

  def init(enable: Boolean): Unit = {
    // Not needed at the moment
    this.enable = enable
  }

  def createTableBase[T <: Data, U <: Data](mapName: String, keyHw: Vec[T], valueHw: Vec[U], basicDB: Boolean = this.enable): MyMap[T, U] = {
    getTable(mapName, keyHw, valueHw)
      .getOrElse({
        val t = new MyMap[T, U](!basicDB, mapName, keyHw, valueHw)
        table_map += (mapName -> t)
        t
      })
  }

  // def createTableWidth[T <: Data, U <: Data](mapName: String, keyHw: T, valueHw: U, width: Integer, basicDB: Boolean = this.enable): MyMap[T, U] = {
  //   val key = Wire(Vec(width, new chiselTypeOf(keyHw)))
  //   val value = Wire(Vec(width, new chiselTypeOf(valueHw)))
  //   createTableBase(mapName, key, value, basicDB)
  // }

  def createTable[T <: Data](mapName: String, keyHw: Vec[T], basicDB: Boolean = this.enable): MyMap[T, UInt] = {
    createTableBase(mapName, keyHw, Vec(keyHw.size, UInt(64.W)), this.enable)
  }

  // TODO: fix syntax bug of overloaded method
  // def createTable[T <: Data](mapName: String, keyHw: T, basicDB: Boolean = this.enable) = {
  //   val valueVec = Wire(Vec(1, new DummyCntBundle))
  //   valueVec.map(_ := 1.U)
  //   keyHw match {
  //     case vec: Vec[_] =>
  //       createTable(mapName, vec, valueVec, basicDB)
  //     case _ =>
  //       val keyVec = Wire(Vec(1, chiselTypeOf(keyHw)))
  //       keyVec.head := keyHw
  //       createTable(mapName, keyVec, valueVec, basicDB)
  //   }
  // }

  def getTable(mapName: String): Option[MyMap[_, _]] = {
    table_map.get(mapName)
  }

  def getTable[T <: Data, U <: Data](mapName: String, keyHw: Vec[T], valueHw: Vec[U]): Option[MyMap[T, U]] = {
    table_map
      .get(mapName)
      .map(old => {
        // TODO: add more check of keyHw and valueHw
        require(old.keyHw.getClass.equals(valueHw.getClass), s"table name conflict: $mapName" +
          s" with different hw types of ${old.keyHw.getClass} and ${keyHw.getClass}")
        old.asInstanceOf[MyMap[T, U]]
      })
  }

  def getCHeader: String = {
    """
      |#ifndef __CHISEL_MAP_H__
      |#define __CHISEL_MAP_H__
      |
      |#include <cstdio>
      |#include <cstring>
      |#include <cstdlib>
      |#include <cassert>
      |#include <cstdint>
      |#include <cerrno>
      |#include <unistd.h>
      |#include <iostream>
      |#include <fstream>
      |
      |void init_map(bool en);
      |void save_maps(const char *filename_prefix);
      |
      |#endif
      |""".stripMargin
  }

  def getCpp: String = {

    val cmaps_name = table_map.values.map(_.cmap_name)

    // init of all map. No need.
    val init =
      s"""
         |void init_map(bool en) {}
         |""".stripMargin

    val save =
      s"""
         |void save_maps(const char *zFilename_prefix) {
         |  printf("saving map to %s.mapName.csv ...\\n", zFilename_prefix);
         |  // TODO: how to save the map
         |  ${cmaps_name.map(mn => s"print_${mn}();").mkString("", "\n", "\n")}
         |  ${cmaps_name.map(mn => s"save_${mn}(zFilename_prefix);").mkString("", "\n", "\n")}
         |}
         |""".stripMargin

    val maps_init = table_map.values.map(_.get_init)
    val maps_insert = table_map.values.map(_.get_insert)
    val maps_print = table_map.values.map(_.get_print)
    val maps_save_file = table_map.values.map(_.get_save_file)

    s"""
       |#include "chisel_map.h"
       |#include <string.h>
       |#include <stdbool.h>
       |#include <map>
       |#include <iostream>
       |
       |${maps_init.mkString("  ", "\n  ", "\n")}
       |${maps_insert.mkString("  ", "\n  ", "\n")}
       |${maps_print.mkString("  ", "\n  ", "\n")}
       |${maps_save_file.mkString("  ", "\n  ", "\n")}
       |
       |$init
       |$save
       |
       |""".stripMargin
  }
  def addToFileRegisters = {
    FileRegisters.add("chisel_map.h", getCHeader)
    FileRegisters.add("chisel_map.cpp", getCpp)
  }

}
