// import Mill dependency
import mill._
import mill.api.Loose
import mill.define.Target
import scalalib._
import scalafmt._
import publish._
// @todo remove build-from-source diplomacy after we have a stable version.
import $file.diplomacy.build

val defaultVersions = Map(
  "chisel3" -> "3.4.1",
  "chisel3-plugin" -> "3.4.1",
  "scala" -> "2.12.12"
)

// @todo remove build-from-source diplomacy after we have a stable version.
object diplomacyUtility extends diplomacy.build.diplomacy {
  override def scalaVersion = defaultVersions("scala")
  override def millSourcePath = os.pwd / "diplomacy" / "diplomacy"
  override def chisel3IvyDeps = Agg(getVersion("chisel3"))
}

def getVersion(dep: String, org: String = "edu.berkeley.cs", cross: Boolean = false) = {
  val version = sys.env.getOrElse(dep + "Version", defaultVersions(dep))
  if (cross)
    ivy"$org:::$dep:$version"
  else
    ivy"$org::$dep:$version"
}

trait UtilityModule extends ScalaModule with SbtModule with ScalafmtModule with PublishModule {
  def scalaVersion = defaultVersions("scala")

  def publishVersion = "0.1-SNAPSHOT"

  def chisel3Module: Option[PublishModule] = None

  def chisel3IvyDeps = if (chisel3Module.isEmpty) Agg(
    getVersion("chisel3")
  ) else Agg.empty[Dep]

  override def moduleDeps = super.moduleDeps ++ chisel3Module

  private val chisel3Plugin = getVersion("chisel3-plugin", cross = true)

  override def ivyDeps = super.ivyDeps() ++ chisel3IvyDeps

  override def scalacPluginIvyDeps = if (chisel3Module.isEmpty) Agg(chisel3Plugin) else Agg.empty[Dep]

  def pomSettings = PomSettings(
    description = artifactName(),
    organization = "me.jiuyang",
    url = "https://github.com/sequencer",
    licenses = Seq(License.`Apache-2.0`),
    versionControl = VersionControl.github("sequencer", "soc"),
    developers = Seq(
      Developer("sequencer", "Jiuyang Liu", "https://www.github.com/sequencer")
    )
  )

}

object misc extends misc
class misc extends UtilityModule

object addressing extends addressing
class addressing extends UtilityModule

object crossing extends crossing
class crossing extends UtilityModule

object decoder extends decoder
class decoder extends UtilityModule

object ecc extends ecc
class ecc extends UtilityModule

object jtag extends jtag
class jtag extends UtilityModule

object verification extends verification
class verification extends UtilityModule

object dts extends dts
class dts extends UtilityModule {
  override def moduleDeps = super.moduleDeps ++ Seq(diplomacyUtility, addressing)
}

object prci extends prci
class prci extends UtilityModule {
  override def moduleDeps = super.moduleDeps ++ Seq(diplomacyUtility, crossing, dts)
}

object regmapper extends regmapper
class regmapper extends UtilityModule {
  override def moduleDeps = super.moduleDeps ++ Seq(diplomacyUtility, crossing, dts, misc, verification, prci)
  override def ivyDeps: Target[Loose.Agg[Dep]] = super.ivyDeps() ++ Agg(
    ivy"org.json4s::json4s-jackson:3.6.1"
  )
}

object playground extends playground
class playground extends UtilityModule {
  override def moduleDeps = super.moduleDeps ++ Seq(diplomacyUtility)
}
