import mill._, scalalib._

object news extends SbtModule {
  def millSourcePath = ammonite.ops.pwd
  def scalaVersion = "0.27.0-RC1"
  def publishVersion = "0.1.0"

  //def scalacOptions = Seq("-source:3.0-migration")

  def ivyDeps = Agg(
    ivy"org.json:json:20200518"
  )

}
