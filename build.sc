import mill._
import scalalib._
import scalafmt._

val defaultVersions = Map(
  "chisel3" -> "3.6.0",
  "chisel3-plugin" -> "3.6.0",
  "chiseltest" -> "0.3.2",
  "scala" -> "2.13.10",
  "scalatest" -> "3.2.7"
)

def getVersion(dep: String, org: String = "edu.berkeley.cs", cross: Boolean = false) = {
  val version = sys.env.getOrElse(dep + "Version", defaultVersions(dep))
  if (cross)
    ivy"$org:::$dep:$version"
  else
    ivy"$org::$dep:$version"
}

trait CommonModule extends ScalaModule {
  override def scalaVersion = defaultVersions("scala")
}


object `rocket-chip` extends SbtModule with CommonModule {

  override def ivyDeps = super.ivyDeps() ++ Agg(
    ivy"${scalaOrganization()}:scala-reflect:${scalaVersion()}",
    ivy"org.json4s::json4s-jackson:3.6.1",
    getVersion("chisel3"),
  )

  object macros extends SbtModule with CommonModule

  object cde extends CommonModule {
    override def millSourcePath = super.millSourcePath / "cde" / "cde"
  }

  object hardfloat extends SbtModule with CommonModule {
    override def ivyDeps = super.ivyDeps() ++ Agg(getVersion("chisel3"))
  }

  override def moduleDeps = super.moduleDeps ++ Seq(
    cde, macros, hardfloat
  )

}

object utility extends SbtModule with ScalafmtModule with CommonModule {

  override def millSourcePath = millOuterCtx.millSourcePath


  override def ivyDeps = super.ivyDeps() ++ Agg(
    getVersion("chisel3"),
    getVersion("chiseltest"),
  )

  override def moduleDeps = super.moduleDeps ++ Seq(`rocket-chip`)

  object test extends Tests {
    override def ivyDeps = super.ivyDeps() ++ Agg(
      getVersion("scalatest","org.scalatest")
    )

    def testFrameworks = Seq("org.scalatest.tools.Framework")
  }
}
