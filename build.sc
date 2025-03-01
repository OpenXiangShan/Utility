import mill._
import mill.scalalib._
import scalafmt._

val defaultVersions = Map(
  "chisel" -> "6.6.0",
  "chisel-plugin" -> "6.6.0",
  "chiseltest" -> "6.0.0",
  "scala" -> "2.13.15",
  "scalatest" -> "3.2.7",
  "json4s-jackson" -> "4.0.7",
  "mainargs" -> "0.7.6",
  "sourcecode" -> "0.4.2",
)

def getVersion(dep: String, org: String = "org.chipsalliance", cross: Boolean = false) = {
  val version = sys.env.getOrElse(dep + "Version", defaultVersions(dep))
  dep match {
    case "chisel-plugin" => ivy"$org:::$dep:$version"
    case _ => if(cross) ivy"$org:::$dep:$version" else ivy"$org::$dep:$version"
  }
}

trait CommonModule extends ScalaModule {
  override def scalaVersion = defaultVersions("scala")
}

object `rocket-chip` extends SbtModule with CommonModule {

  override def ivyDeps = super.ivyDeps() ++ Agg(
    getVersion("json4s-jackson", "org.json4s"),
    getVersion("mainargs", "com.lihaoyi"),
    getVersion("chisel"),
  )

  object macros extends SbtModule with CommonModule {
    override def ivyDeps = super.ivyDeps() ++ Agg(
      ivy"${scalaOrganization()}:scala-reflect:${scalaVersion()}",
    )
  }

  object cde extends CommonModule {
    override def millSourcePath = super.millSourcePath / "cde"
  }

  object hardfloat extends SbtModule with CommonModule {
    override def ivyDeps = super.ivyDeps() ++ Agg(getVersion("chisel"))
    override def millSourcePath = super.millSourcePath / "hardfloat"
  }

  override def moduleDeps = super.moduleDeps ++ Seq(
    cde, macros, hardfloat
  )

}

object utility extends SbtModule with ScalafmtModule with CommonModule {

  override def millSourcePath = millOuterCtx.millSourcePath

  override def scalacOptions = super.scalacOptions() ++
    Agg("-language:reflectiveCalls", "-Ymacro-annotations", "-Ytasty-reader")

  override def scalacPluginIvyDeps = super.scalacPluginIvyDeps() ++ Agg(getVersion("chisel-plugin"))
  override def ivyDeps = super.ivyDeps() ++ Agg(
    getVersion("chisel"),
    getVersion("chiseltest", "edu.berkeley.cs"),
    getVersion("sourcecode", "com.lihaoyi"),
  )

  override def moduleDeps = super.moduleDeps ++ Seq(`rocket-chip`)

  object test extends SbtTests with TestModule.ScalaTest{
    override def ivyDeps = super.ivyDeps() ++ Agg(
      getVersion("scalatest","org.scalatest"),
    )
  }
}
