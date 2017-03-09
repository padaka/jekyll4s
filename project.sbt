
organization in Global := "com.github.padaka"

version in Global := "0.0.1"

scalaVersion in Global := "2.11.7"

lazy val root = project.in(file(".")).aggregate(jekyll4s, http)

lazy val jekyll4s = project.in(file("jekyll4s"))
  .enablePlugins(BuildInfoPlugin)

lazy val http = project.in(file("http")).dependsOn(jekyll4s)
