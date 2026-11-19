import mill._
import scalalib._
import scalafmt._
import os.Path
import publish._
import $file.`rocket-chip`.common
import $file.`rocket-chip`.cde.common
import $file.`rocket-chip`.hardfloat.common

object v {
  val scala = "2.13.18"
  val chiselCrossVersions = Map(
    "xiangshan" -> (
      mvn"cc.xiangshan::chisel:7.16.0-xiangshan-1",
      mvn"cc.xiangshan:::chisel-plugin:7.16.0-xiangshan-1"
    ),
    "upstream" -> (
      mvn"org.chipsalliance::chisel:7.16.0",
      mvn"org.chipsalliance:::chisel-plugin:7.16.0"
    )
  )
  def chiselKeys = chiselCrossVersions.keys.toSeq
}

val pwd = os.Path(sys.env("MILL_WORKSPACE_ROOT"))

trait HasChisel extends ScalaModule with Cross.Module[String] {
  def chiselModule: Option[ScalaModule] = None

  def chiselPluginJar: T[Option[PathRef]] = None

  def chiselIvy: Option[Dep] = Some(v.chiselCrossVersions(crossValue)._1)

  def chiselPluginIvy: Option[Dep] = Some(v.chiselCrossVersions(crossValue)._2)

  def sourcecodeIvy = mvn"com.lihaoyi::sourcecode:0.4.4"

  override def scalaVersion = v.scala

  override def scalacOptions = super.scalacOptions() ++
    Agg("-language:reflectiveCalls", "-Ymacro-annotations", "-Ytasty-reader")

  override def ivyDeps = super.ivyDeps() ++ Agg(chiselIvy.get) ++ Agg(sourcecodeIvy)

  override def scalacPluginIvyDeps = super.scalacPluginIvyDeps() ++ Agg(chiselPluginIvy.get)
}

trait RocketChip extends `rocket-chip`.common.RocketChipModule with HasChisel {
  val rcPath = pwd / "rocket-chip"
  override def millSourcePath = rcPath

  def mainargsIvy = mvn"com.lihaoyi::mainargs:0.7.0"

  def json4sJacksonIvy = mvn"org.json4s::json4s-jackson:4.0.7"

  trait Macros extends `rocket-chip`.common.MacrosModule with HasChisel {
    def scalaReflectIvy = mvn"org.scala-lang:scala-reflect:${v.scala}"
  }

  object macros extends Cross[Macros](v.chiselKeys)

  def macrosModule = macros(crossValue)

  trait CDE extends `rocket-chip`.cde.common.CDEModule with HasChisel {
    override def millSourcePath = rcPath / "cde" / "cde"
  }

  object cde extends Cross[CDE](v.chiselKeys)

  def cdeModule = cde(crossValue)

  trait HardFloat extends `rocket-chip`.hardfloat.common.HardfloatModule with HasChisel {
    override def millSourcePath = rcPath / "hardfloat" / "hardfloat"
  }

  object hardfloat extends Cross[HardFloat](v.chiselKeys)

  def hardfloatModule = hardfloat(crossValue)
}

object rocketchip extends Cross[RocketChip](v.chiselKeys)

trait Utility extends SbtModule with HasChisel {
  override def millSourcePath = pwd

  override def moduleDeps = super.moduleDeps ++ Seq(rocketchip(crossValue))

  override def scalacOptions = super.scalacOptions() ++ Agg("-deprecation", "-feature")
}

object utility extends Cross[Utility](v.chiselKeys)
