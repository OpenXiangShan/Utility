package utility.statistic



import chisel3._
import chisel3.util.HasBlackBoxInline
import scala.collection.mutable.ArrayBuffer
import utility.HasDPICUtils

class Node(name: String, val isExpr: Boolean = false) {
	val descript = if (isExpr) name else ("XSstatistics_" + name)
	var express = descript
	XSStatistics.register(this)

	def +(b: Node) = {
		new NodeExpr("(" + expr() + "+" + b.expr() + ")")
	}
	def -(b: Node) = {
		new NodeExpr("(" + expr() + "-" + b.expr() + ")")
	}
	def *(b: Node) = {
		new NodeExpr("(" + expr() + "*" + b.expr() + ")")
	}
	def /(b: Node) = {
		new NodeExpr("(" + "(double)" + expr() + "/" + b.expr() + ")")
	}

	def varname() = {
		if (isExpr) {
			throw new Exception(s"Statistics: ${descript} not an variable")
		}
		descript
	} 
	def expr() = express

	/**
	  * add math function process
	  * 
	  * a = Scalar("t0") => uint64_t t0 = 0;
	  * 
	  * b = Scalar("t1") => uint64_t t1 = 0;
	  * 
	  * a.wrapping("sqrt") + b => sqrt(t0) + t1
	  * 
	  * @param mathFunc the functions of "math.h"
	  * 
	  */
	def addWrapping(mathFunc: String) {
		express = mathFunc + s"(${express})"
	}

	def getDPIC() = ""
}

class NodeExpr(expr: String) extends Node(expr, true)

class Scalar(name: String) extends Node(name) {
	val dpic_func = s"dpic_${varname()}"

	class ScalarModule(name: String) extends HasDPICUtils {
		val io = IO(new Bundle{
			val clock = Input(Clock())
			val reset = Input(Reset())
			val en = Input(Bool())
			val n = Input(UInt(64.W))
		})
		init(io, false, false, name)
	}

	def sample(n: UInt, en: UInt, clock: Clock) {
		val m = new ScalarModule(dpic_func)
		m.io.clock := clock
		m.io.reset := false.B
		m.io.en := en
		m.io.n := n
	}

	override def getDPIC() = {
		s"""
		|void ${dpic_func}(uint64_t val) {
		|  ${varname()} += val;
		|}
		"""	
	}
}


class Vector(name: String, n: Int) extends Node(name) {
	val dpic_func = s"dpic_${varname()}"
	val size = n

	class VectorModule(name: String) extends HasDPICUtils {
		val io = IO(new Bundle{
			val clock = Input(Clock())
			val reset = Input(Reset())
			val en = Input(Bool())
			val i = Input(UInt(64.W))
			val n = Input(UInt(64.W))
		})
		init(io, false, false, name)
	}

	def apply(i : Int) = {
		if (i >= size) {
			throw new Exception(s"Statistics: ${varname()} out of range")
		}
		new NodeExpr(varname() + s"[${i}]")
	}

	def len() = size

	def sample(i: UInt, n: UInt, en: UInt, clock: UInt) = {
		val m = new VectorModule(dpic_func)
		m.io.clock := clock
		m.io.reset := false.B
		m.io.en := en
		m.io.i := i
		m.io.n := n
	}
}

class Formula(name: String) extends Node(name) {
	def :=(expr: Node) {
		express = expr.expr()
	}

	def getFormula() = {
		varname() + "=" + expr()
	}
}


object XSStatistics {
	val counters: ArrayBuffer[Node] = ArrayBuffer()
	def register(n: Node) {
		val conflict = counters.exists(_.varname() == n.varname())
		if (conflict) {
			throw  new Exception(s"Statistics: ${n.varname()} has already exists")
		}
		counters.addOne(n)
	}

	def getCpp() {
		val cppvars: StringBuilder = new StringBuilder

		val formulas: StringBuilder = new StringBuilder

		val dpicfuncs: StringBuilder = new StringBuilder

		counters.foreach{c =>
			val full_name = c.varname()
			val use_float = c.expr().exists(_ == '/')
		  cppvars ++= (if (use_float) "double" else "uint64_t") + " " + full_name + "=0;\n"
			dpicfuncs ++= c.getDPIC()
			if (c.isInstanceOf[Formula]) {
				formulas ++= c.asInstanceOf[Formula].getFormula() + ";\n"
			}
		}
	}
}