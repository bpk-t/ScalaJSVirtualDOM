
enablePlugins(ScalaJSPlugin)

name := "ScalaJSVirtualDOM"

version := "0.1"

scalaVersion := "2.12.4"

scalaJSUseMainModuleInitializer := true

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.1"