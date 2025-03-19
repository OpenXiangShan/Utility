/** *************************************************************************************
  * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
  * Copyright (c) 2020-2021 Peng Cheng Laboratory
  *
  * XiangShan is licensed under Mulan PSL v2.
  * You can use this software according to the terms and conditions of the Mulan PSL v2.
  * You may obtain a copy of Mulan PSL v2 at:
  * http://license.coscl.org.cn/MulanPSL2
  *
  * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
  * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
  * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
  *
  * See the Mulan PSL v2 for more details.
  * *************************************************************************************
  */

package utility

import chisel3._
import org.chipsalliance.cde.config.Field

/**
  * This object collects constants and related helper methods
  * to support different CHI issues (versions).
  */
object Issue {
  val B = "B"
  val C = "C"
  val Eb = "E.b"
}

trait HasCHIMsgConstants {
  val CONSTANTS = Map(
    "QOS_WIDTH" -> 4,
    // "TXNID_WIDTH" -> 8,
    // "LPID_WITH_PADDING_WIDTH" -> 5,
    "LPID_WIDTH" -> 5,
    // "REQ_OPCODE_WIDTH" -> 6,
    // "RSP_OPCODE_WIDTH" -> 4,
    // "SNP_OPCODE_WIDTH" -> 5,
    // "DAT_OPCODE_WIDTH" -> 3,
    "SIZE_WIDTH" -> 3,
    "PCRDTYPE_WIDTH" -> 4,
    "MEMATTR_WIDTH" -> 4,
    "ORDER_WIDTH" -> 2,
    "VMIDEXT_WIDTH" -> 8,
    "RESPERR_WIDTH" -> 2,
    "RESP_WIDTH" -> 3,
    "FWDSTATE_WIDTH" -> 3,
    "DATAPULL_WIDTH" -> 3,
    // "DATASOURCE_WIDTH" -> 3,
    "CCID_WIDTH" -> 2,
    "DATAID_WIDTH" -> 2,
  )
}

trait HasCHIMsgVariables {
  /**
    * Variables
    */
  def issue: String
  def nodeIDWidth: Int
  def reqAddrWidth: Int
  def enableMPAM: Option[Int]
  def reqRSVDCWidth: Int
  def datRSVDCWidth: Int
  def dataWidth: Int
  def enableDataCheck: Option[String]
  def enablePoison: Boolean

  /**
    * Requirement
    */
  require(nodeIDWidth >= 7 && nodeIDWidth <= 11)
  require(reqAddrWidth >= 44 && reqAddrWidth <= 52)

  if (issue.compareTo(Issue.Eb) < 0) {
    require(!enableMPAM.isDefined)
  } else {
    require(enableMPAM.isDefined)
    require(Seq(0, 11).contains(enableMPAM.get))
  }

  if (issue.compareTo(Issue.Eb) < 0) {
    require(Seq(0, 4, 12, 16, 24, 32).contains(reqRSVDCWidth))
    require(Seq(0, 4, 12, 16, 24, 32).contains(datRSVDCWidth))
  } else {
    require(Seq(0, 4, 8, 12, 16, 24, 32).contains(reqRSVDCWidth))
    require(Seq(0, 4, 8, 12, 16, 24, 32).contains(datRSVDCWidth))
  }

  require(Seq(128, 256, 512).contains(dataWidth))
  require(dataWidth == 256) // only 256 is supported for now

  val BASE_VARIABLES = Map(
    "NODEID_WIDTH" -> nodeIDWidth,
    "TXNID_WIDTH" -> 8,
    "LPID_WITH_PADDING_WIDTH" -> 5,
    "DATASOURCE_WIDTH" -> 3,
    "REQ_OPCODE_WIDTH" -> 6,
    "RSP_OPCODE_WIDTH" -> 4,
    "SNP_OPCODE_WIDTH" -> 5,
    "DAT_OPCODE_WIDTH" -> 3,
    "ADDR_WIDTH" -> reqAddrWidth,
    "DATA_WIDTH" -> dataWidth,
    "REQ_RSVDC_WIDTH" -> reqRSVDCWidth,
    "DAT_RSVDC_WIDTH" -> datRSVDCWidth,
    "DATACHECK_WIDTH" -> enableDataCheck.map(_ => dataWidth / 8).getOrElse(0),
    "POISON_WIDTH" -> (if (enablePoison) dataWidth / 64 else 0)
  )

  val B_VARIABLES = BASE_VARIABLES

  val C_VARIABLES = B_VARIABLES ++ Map(
    "DAT_OPCODE_WIDTH" -> 4
  )

  val Eb_VARIABLES = C_VARIABLES ++ Map(
    "TXNID_WIDTH" -> 12,
    "LPID_WITH_PADDING_WIDTH" -> 8,
    "DATASOURCE_WIDTH" -> 4,
    "REQ_OPCODE_WIDTH" -> 7,
    "RSP_OPCODE_WIDTH" -> 5,
    "SNP_OPCODE_WIDTH" -> 5,
    "DAT_OPCODE_WIDTH" -> 4,
    // new fields
    "CBUSY_WIDTH" -> 3,
    "MPAM_WIDTH" -> enableMPAM.getOrElse(0),
    "SLCREPHINT_WIDTH" -> 7,
    "TAGOP_WIDTH" -> 2
  )

  val VARIABLES = Map(
    Issue.B -> B_VARIABLES,
    Issue.C -> C_VARIABLES,
    Issue.Eb -> Eb_VARIABLES
  )(issue)
}

case class CHIParam(
  issue: String = Issue.Eb,
  nodeIDWidth: Int = 11,
  reqAddrWidth: Int = 48,
  enableMPAM: Option[Int] = Some(11),
  reqRSVDCWidth: Int = 4,
  datRSVDCWidth: Int = 4,
  dataWidth: Int = 256,
  enableDataCheck: Option[String] = Some("oddparity"),
  enablePoison: Boolean = true
) extends HasCHIMsgConstants with HasCHIMsgVariables {
  def CONFIG(key: String): Int = {
    val params = CONSTANTS ++ VARIABLES
    if (params.contains(key)) params(key) else 0
  }
}

case object CHIParamKey extends Field[CHIParam](CHIParam())