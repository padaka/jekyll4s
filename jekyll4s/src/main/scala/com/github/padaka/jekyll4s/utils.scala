package com.github.padaka.jekyll4s

import java.io.File
import java.nio.file.{Path, Paths}
import java.util.Calendar

import monocle.Optional
import net.jcazevedo.moultingyaml._

import scala.util.{Failure, Success, Try}
import scalaz.{Kleisli, Monad, Monoid}

/**
  * Created by m41l3 on 3/2/17.
  */
object utils {
  import Implicits._


  def fileId(f: File) = {
    val fn = f.getName
    val extStart = fn.lastIndexOf('.')
    if (extStart > 0) fn.substring(0, extStart) else fn
  }

  def destinationPath(destination: Path, path: Path): Path = {
    val sanitizedPath = ( Option(path.getRoot) match {
      case None => path
      case Some(root) => root.relativize(path)
    } ).filter { !_.toString.startsWith("_") }

    destination.resolve(sanitizedPath)
  }

  def datePartPath(date: Calendar): Path =
    "%04d/%02d/%02d".format(
      date.get(Calendar.YEAR),
      date.get(Calendar.DAY_OF_MONTH),
      date.get(Calendar.MONTH) + 1 // Java calendar months are 0-based?
    ).asPath

  def datePartPath(date: Calendar, baseurl: Option[String], name: String): Path = {
    val p1 = datePartPath(date).resolve(name)
    baseurl.fold(p1) { _.asPath.resolve(p1) }
  }

  private class ReadOnce[A, B](read: Generator.Reader[A, B]) {
    private var _cache: Option[Try[B]] = None
    def apply(a: A): Try[B] = synchronized {
      if (_cache.isEmpty) _cache = Some(read(a))
      _cache.getOrElse(Failure(GeneratorException("Synchronization failure!")))
    }
  }

  def readOnce[A, B](read: Generator.Reader[A, B]): Generator.Reader[A, B] =
    new ReadOnce(read).apply


  object Implicits {

    implicit class FileOps(val file: File) extends AnyVal {

      def listFilesRecursive: Array[File] =
        if (file.isDirectory) file.listFiles.flatMap(_.listFilesRecursive)
        else Array(file)

      def listFilesRecursiveIf(recurseIf: File => Boolean): Array[File] =
        if (file.isDirectory && recurseIf(file)) file.listFiles.flatMap(_.listFilesRecursive)
        else if(file.isFile) Array(file)
        else Array.empty

    }

    implicit class StandardKleisliOps[M[_],A,B](f: A => M[B]) {

      def scalaz: Kleisli[M, A, B] = Kleisli.kleisli(f)

    }

    implicit class ScalazKleisliOps[M[_],A,B](f: Kleisli[M, A, B]) {

      def standard: (A) => M[B] = f.run

    }

    implicit class StringOps(val str: String) extends AnyVal {

      def asPath: Path = Paths.get(str)

    }

    implicit class PathOps(val path: Path) extends AnyVal {
      import scala.collection.JavaConversions.asScalaIterator

      def filter(fn: Path => Boolean): Path = Paths.get(path.iterator.filter(fn).mkString("/"))

      def absolute: Path = Paths.get(path.toFile.getAbsoluteFile.toURI)

    }

    implicit class OptionOps[A](opt: Option[A]) {

      def toTry(fail: => Throwable): Try[A] = opt.fold[Try[A]](Failure(fail))(a => Success[A](a))

    }

    implicit val tryMonad: Monad[Try] = TypeClasses.tryMonad

    implicit val yamlValueMonoid: Monoid[YamlValue] = TypeClasses.yamlValueMonoid

  }

  object TypeClasses {

    def tryMonad = new Monad[Try] {

      def bind[A, B](fa: Try[A])(f: (A) => Try[B]): Try[B] = fa.flatMap(f)

      def point[A](a: => A): Try[A] = Success(a)

    }

    def yamlValueMonoid: Monoid[YamlValue] = new Monoid[YamlValue] {

      def zero: YamlValue = YamlNull

      def append(a1: YamlValue, a2: => YamlValue): YamlValue = (a1, a2) match {
        case (x: YamlObject, y: YamlObject) => yamlObjectMonoid.append(x, y)
        case (x: YamlArray, y: YamlArray) => yamlArrayMonoid.append(x, y)
        case (x: YamlNumber, y: YamlNumber) => yamlNumberMonoid.append(x, y)
        case (x: YamlString, y: YamlString) => yamlStringMonoid.append(x, y)
        case (x: YamlBoolean, y: YamlBoolean) => yamlBooleanMonoid.append(x, y)
        case (YamlNull, y: YamlValue) => y
        case (x: YamlValue, YamlNull) => x
        case _ => throw GeneratorException(s"Appending of YAML the following YAML values is not supported: [$a1] [$a2]")
      }

    }

    def yamlObjectMonoid =
      Monoid.instance[YamlObject]( (x, y) => YamlObject(x.fields ++ y.fields), YamlObject())

    def yamlArrayMonoid =
      Monoid.instance[YamlArray]((x, y) => YamlArray(x.elements ++ y.elements), YamlArray())

    def yamlStringMonoid =
      Monoid.instance[YamlString]((x, y) => YamlString(x.value + y.value), YamlString(new String))

    def yamlNumberMonoid =
      Monoid.instance[YamlNumber]((x, y) => YamlNumber(x.value + y.value), YamlNumber(0))

    def yamlBooleanMonoid =
      Monoid.instance[YamlBoolean]((x, y) => YamlBoolean(x.boolean && y.boolean), YamlBoolean(true))

  }

  object Markdown {
    import com.vladsch.flexmark.html.HtmlRenderer
    import com.vladsch.flexmark.parser.Parser

    // TODO: Allow static configuration changes
    lazy val parser: Parser = Parser.builder.build
    lazy val renderer: HtmlRenderer = HtmlRenderer.builder.build

    def parseString(markdown: String): String = {
      val doc = parser.parse(markdown)
      renderer.render(doc)
    }

  }

  object lens {
    import monocle.Lens

    object map {

      def value[K, V](k: K): Optional[Map[K, V], V] =
        Optional[Map[K, V], V](_.get(k))((v: V) => (m: Map[K, V]) => m + (k -> v))

    }

    object yaml {
      import DefaultYamlProtocol._

      val string: Lens[YamlValue, String] =
        Lens[YamlValue, String] (_.convertTo[String])((str: String) => _ => YamlString(str))

      def objectVal(k: YamlValue): Optional[YamlObject, YamlValue] =
        Optional[YamlObject, YamlValue](_.fields.get(k))((v: YamlValue) => (obj: YamlObject) => obj.copy(fields = obj.fields + (k -> v)))


    }

    object post {
      import yaml._

      val layout = map.value("layout") ^|-> string

      val permalink = map.value("permalink") ^|-> string

    }

    object layout {}

  }

}
