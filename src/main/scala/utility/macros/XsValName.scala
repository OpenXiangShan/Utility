/**************************************************************************************
 * Copyright (c) 2020 Institute of Computing Technology, CAS
 * Copyright (c) 2020 University of Chinese Academy of Sciences
 *
 * NutShell is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 *             http://license.coscl.org.cn/MulanPSL2
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND, EITHER
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT, MERCHANTABILITY OR
 * FIT FOR A PARTICULAR PURPOSE.
 *
 * See the Mulan PSL v2 for more details.
 ***************************************************************************************/

package utility.macros

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

case class XsValNameImpl(name: String)

object XsValNameImpl
{
  private val shortMap:Map[String, String] = Seq(
    "coupledL2" -> "l2"
  ).toMap

  implicit def materialize: XsValNameImpl = macro detail
  def detail(c: Context): c.Expr[XsValNameImpl] = {
    import c.universe._
    def allOwners(s: c.Symbol): Seq[c.Symbol] =
      if (s == `NoSymbol`) Nil else s +: allOwners(s.owner)
    val owners = allOwners(c.internal.enclosingOwner)
    val terms = owners.filter(_.isTerm).map(_.asTerm)
    terms.filter(t => t.isVal || t.isLazy).map(_.name.toString).find(_(0) != '$').map { s =>
      val pfx = owners.filter(t =>t.isPackage || t.isClass).init.map(_.name.toString)
        .filter(shortMap.contains).map(shortMap).reduce((a:String, b:String) => s"${a}_${b}")
      val trim = pfx + "_" + s.replaceAll("\\s", "")
      c.Expr[XsValNameImpl] { q"_root_.utility.macros.XsValNameImpl(${trim})" }
    }.getOrElse(c.abort(c.enclosingPosition, "Not a valid application."))
  }
}

case class XsValName(name: String)

object XsValName
{
  implicit def materialize(implicit x: XsValNameImpl): XsValName = XsValName(x.name)
}