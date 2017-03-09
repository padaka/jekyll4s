
name := "jekyll4s"

val Scalate = "org.scalatra.scalate" %% "scalate-core" % "1.8.0" :: Nil
val Flexmark = "com.vladsch.flexmark" % "flexmark" % "0.13.3" :: Nil
val MoultingYAML = "net.jcazevedo" %% "moultingyaml" % "0.4.0" :: Nil
val Monocle =
  "com.github.julien-truffaut" %% "monocle-core" % "1.4.0" ::
  "com.github.julien-truffaut" %% "monocle-macro" % "1.4.0" :: Nil

libraryDependencies ++=
  Scalate ++ Flexmark ++ MoultingYAML ++ Monocle ++ Seq(
    "org.scalatest" %% "scalatest" % "2.2.1" % "test" withSources() withJavadoc(),
    "org.scalacheck" %% "scalacheck" % "1.12.1" % "test" withSources() withJavadoc()
  )

buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion)

buildInfoPackage := "com.github.padaka.jekyll4s"

initialCommands := "import com.github.padaka.jekyll4s._"

