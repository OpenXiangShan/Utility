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

package utility.transforms

import firrtl._
import firrtl.ir._
import firrtl.options.Phase
import firrtl.stage.FirrtlCircuitAnnotation

import scala.collection.mutable

class LogPerfDedup extends Phase {

  override def invalidates(a: Phase) = false

  override def transform(annotations: AnnotationSeq): AnnotationSeq = {

    import utility.transforms.Helpers._

    val (Seq(circuitAnno: FirrtlCircuitAnnotation), otherAnnos) = annotations.partition {
      case _: FirrtlCircuitAnnotation => true
      case _ => false
    }
    val c = circuitAnno.circuit

    def onExpr(e: Expression): Expression = e match {
      case Reference(s"logPerfSignal_bore$_", tpe) => Reference("logPerfSignal_bore", tpe)
      case other => other.mapExpr(onExpr)
    }

    def onStmt(s: Statement): Statement = s match {
      case Connect(info, loc, expr) => Connect(info, onExpr(loc), onExpr(expr))
      case Conditionally(info, pred, conseq, alt) => Conditionally(info, onExpr(pred), onStmt(conseq), onStmt(alt))
      case Print(info, string, args, clk, en) => Print(info, string, args.map(onExpr), onExpr(clk), onExpr(en))
      case DefNode(info, name, value) => DefNode(info, name, onExpr(value))
      case other => other.mapStmt(onStmt)
    }

    FirrtlCircuitAnnotation(c.mapModule(m => m.mapStmt(onStmt))) +: otherAnnos
  }
}
