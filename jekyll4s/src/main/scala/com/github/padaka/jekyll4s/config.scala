package com.github.padaka.jekyll4s

import java.io.File
import java.nio.file.{Files, Path, Paths}

import net.jcazevedo.moultingyaml._


/**
  * Created by m41l3 on 2/20/17.
  */
object config {

  trait Protocol extends DefaultYamlProtocol {

    implicit lazy val defaultsScopeFormat = yamlFormat2(DefaultsScope.apply)

    implicit lazy val defaultsFormat = yamlFormat2(Defaults.apply)

    implicit lazy val generatorConfigFormat = yamlFormat9(GeneratorConfig.apply)


  }

  object protocol extends Protocol

  lazy val cwd: Path = Paths.get(new File("").getAbsoluteFile.toURI)

}

case class DefaultsScope(path: Option[String], `type`: Option[String])

case class Defaults(scope: DefaultsScope,
                    values: Map[String, YamlValue])

case class GeneratorConfig(title: String,
                           email: String,
                           description: String,
                           baseurl: Option[String],
                           url: String,
                           source: String,
                           destination: String,
                           template_engine: String,
                           defaults: List[Defaults])
