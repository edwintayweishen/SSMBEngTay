name := "Sem2Scalismo"

version := "0.1"

scalaVersion := "2.12.10"

fork in run := true
connectInput in run := true

resolvers += Resolver.bintrayRepo("unibas-gravis","maven")
libraryDependencies ++=
  Seq("ch.unibas.cs.gravis" %% "scalismo-ui" % "0.13.0")
