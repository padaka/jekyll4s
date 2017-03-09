package com.github.padaka.jekyll4s

import java.nio.file.{Files, Path}
import java.text.SimpleDateFormat
import java.util.Calendar

import scala.io.Source
import scala.util.{Failure, Success, Try}
import net.jcazevedo.moultingyaml._
import net.jcazevedo.moultingyaml.DefaultYamlProtocol._
import utils.Implicits._

/**
  * Features:
  * - Users can update site generator settings from a configuration file.
  * - Markdown files found in a specified asset directory i.e. _posts, are converted into HTML pages
  *   and are written to a destination directory.
  * - Templates can be provided to create layouts for the given content.
  * - A user can access site and page specific data including: site configuration attributes, page metadata,
  *   page content, etc using a supported template language to design the layout of content on a page.
  * - ...
  *
  * *** We're using moultingyaml YamlValue AST as the underlying representation for the data
  *
  */
object Generator {

  val Version = BuildInfo.version

  type Reader[A, B] = A => Try[B]

  private[jekyll4s] def documentByPath(path: Path): Try[Document] =
    Document.load(path)

  private[jekyll4s] def documentById(directory: Path, name: String): Try[Document] =
    Try {
      // TODO: store move common paths in one location
      // Document extracts the fileId and stores it as its `name` value, but there's an unnecessary
      // overhead in reading files into memory every time that we want to find a single document...
      // this can be optimized by loading all documents once and making them addressable by id. The
      // presumption here is that out sites are not going to be huge archives with 1000+ of documents.
        directory.toFile.listFilesRecursive
        .filter { utils.fileId(_) == name }
        .map { _ .toPath }
        .toList
    } .flatMap {
      case Nil      => Failure(GeneratorException(s"No documents found with the name: '$name'"))
      case x :: Nil => Document.load(x)
      case _        => Failure(GeneratorException(s"Found multiple documents with the name: '$name'"))
    }

  def layoutDocument(sourceDir: Path, id: String): Try[LayoutDocument] =
    documentById(sourceDir, id).map(new LayoutDocument(_))

  def dataDocument(sourceDir: Path, id: String): Try[DataDocument] =
    documentById(sourceDir, id).map(new DataDocument(_))

  def postDocument(sourceDir: Path, id: String, baseurl: Option[String]): Try[PostDocument] =
    documentById(sourceDir, id).map(new PostDocument(_, baseurl))

  def pageDocument(path: Path, baseurl: Option[String]): Try[PageDocument] =
    documentByPath(path).map(new PageDocument(_, baseurl))

  /**
    * Extract default data values from GeneratorConfig object. Default data values need to be addressable
    * by either type or path (a path represented by an empty string matches all documents).
    *
    * type (optional): `pages`, `posts`
    * path (required): path prefix
    *
    * @param config
    * @return
    */
  private[jekyll4s] def dataFromConfig(config: GeneratorConfig, doc: DocumentAdapter): Try[Map[String, YamlValue]] = Try {

    def filterType(defaults: Defaults): Boolean = (doc, defaults.scope.`type`) match {
      case (_, None) => true
      case (_: PageDocument, Some("pages")) => true
      case (_: PostDocument, Some("posts")) => true
      case _ => false
    }

    def filterPath(defaults: Defaults): Boolean =
      defaults.scope.path.forall { doc.document.path.startsWith }

    config.defaults
      .filter(filterType)
      .filter(filterPath)
      .foldLeft(Map.empty[String, YamlValue]) {
        case (z, x) => z ++ x.values
      }
  }

  private[jekyll4s] def dataFromDir(path: Path): Try[Map[String, YamlValue]] = {
    import scalaz.syntax.traverse._
    import scalaz.std.list._
    import scalaz.std.map._

    path.toFile.listFiles.toList
      .foldMapM {
        f =>
          Document.load(f.toPath)
            .map {
              doc => Map(utils.fileId(f) -> new DataDocument(doc).value)
            }
      }
  }

  def dataObject(config: GeneratorConfig, dataDir: Path, doc: DocumentAdapter): Try[Data] =
    for {
      m1 <- dataFromConfig(config, doc)
      m2 <- dataFromDir(dataDir)
    } yield new DataMap(m1 ++ m2)

  def renderPost(post: PostDocument, layoutReader: Reader[String, LayoutDocument], siteFactory: Site.Factory, engine: template.Engine): Try[String] =
    for {
      l <- layoutReader(post.layout)
      s <- siteFactory(post)
      o <- l.render(engine)(post, s)
    } yield o

  def renderPage(page: PageDocument, layoutReader: Reader[String, LayoutDocument], siteFactory: Site.Factory, engine: template.Engine): Try[String] =
    for {
      l <- layoutReader(page.layout)
      s <- siteFactory(page)
      o <- l.render(engine)(page, s)
    } yield o

  /**
    * Generate site documents. Load all page and post documents, render them with the selected template
    * engine, and write the files to disk.
    *
    * @param config
    */
  def generate(config: GeneratorConfig): Try[Unit] = {
    import utils.Implicits._

    val url             = config.url
    val baseurl         = config.baseurl
    val templateEngine  = config.template_engine
    val source          = config.source.asPath
    val layoutsDir      = source.resolve("_layouts")
    val postsDir        = source.resolve("_posts")
    val dataDir         = source.resolve("_data")
    val templateDir     = source.resolve("_includes")
    val destination     = config.destination.asPath

    val pagesK: Reader[Unit, List[PageDocument]] = utils.readOnce((_: Unit) => Page.loadPages(source, baseurl))
    val postsK: Reader[Unit, List[PostDocument]] = utils.readOnce((_: Unit) => Page.loadPosts(postsDir, baseurl))
    val dataK: Reader[DocumentAdapter, Map[String, YamlValue]] =
      ( for {
        dd <- utils.readOnce[DocumentAdapter, Map[String, YamlValue]](_ => dataFromDir(dataDir)).scalaz
        dc <- ((doc: DocumentAdapter) => dataFromConfig(config, doc)).scalaz
      } yield dd ++ dc ).standard

    val factory = new Site.Factory(Version, url, dataK, pagesK, postsK)
    val engine  = new template.ScalateEngine(templateEngine, layoutsDir :: templateDir :: Nil)

    val run =
      for {
        pages <- pagesK.scalaz
        posts <- postsK.scalaz
      } yield {

        val writes: List[Try[(Path, String)]] =
          pages.map {
            page =>
              val path = utils.destinationPath(destination, page.permalink.asPath)
              renderPage(page, layoutDocument(layoutsDir, _), factory, engine).map(content => (path, content))
          } ++ posts.map {
            post =>
              val path = utils.destinationPath(destination, post.permalink.asPath)
              renderPost(post, layoutDocument(layoutsDir, _), factory, engine).map(content => (path, content))
          }

        // TODO: Handle errors correctly
        writes.foreach {
          case Success((outputPath, content)) =>
            Files.createDirectories(outputPath.getParent)
            Files.write(outputPath, content.getBytes)
          case Failure(th) =>
            println(th)
        }
      }

    run({})
  }

}


//
// Document adapters
//
trait DocumentAdapter {

  val document: Document

  def raw: String = document.content.getOrElse(new String)

}


/**
  * Layout adapter for Document objects.
 *
  * @param document
  */
class LayoutDocument(val document: Document) extends DocumentAdapter {

  def render(engine: template.Engine)(page: Page, site: Site): Try[String] =
    engine.render2(this)("page" -> page, "site" -> site)

}

/**
  * Page adapter for Document objects.
  * Note: The methods match those of the Post adapter, but the two classes are separate to create a
  * distinction between the two document types.
 *
  * @param doc
  */
class PageDocument(doc: Document, baseurl: Option[String]) extends Page with DocumentAdapter {

  val document = doc.copy(ext = Some(".html"))

  lazy val layout: String =
    utils.lens.post.layout.getOption(document.data).getOrElse("default")

  lazy val permalink: String =
    utils.lens.post.permalink.getOption(document.data)
      .orElse(document.date.map { utils.datePartPath(_, baseurl, Document.filename(document)).toString })
      .getOrElse(throw GeneratorException("Could not generate a permalink"))

  lazy val content: String = ""

}

/**
  * Post adapter for Document objects.
  * Note: The methods match those of the Page adapter, but the two classes are separate to create a
  * distinction between the two document types.
 *
  * @param doc
  */
class PostDocument(doc: Document, baseurl: Option[String]) extends Page with DocumentAdapter {

  val document = doc.copy(ext = Some(".html"))

  lazy val layout: String =
    utils.lens.post.layout.getOption(document.data).getOrElse("default")

  lazy val permalink: String =
    utils.lens.post.permalink.getOption(document.data)
      .orElse(document.date.map { utils.datePartPath(_, baseurl, Document.filename(document)).toString })

      .getOrElse(throw GeneratorException("Could not generate a permalink"))

  lazy val content: String =
    utils.Markdown.parseString(this.raw)

}

/**
  * Data adapter for Document objects.
  * This seems a bit hacky, but it does sort of make sense: the underlying Document should have an empty `data` field
  * because because the "data" is the actual content of the file. Maybe the right way to go about this would be to
  * have a specific underlying type just for data files, but that seems like more trouble than what it's worth when
  * the Document type workflows will work just fine here.
 *
  * @param document
  */
class DataDocument(val document: Document) extends DocumentAdapter {
  if (document.data.nonEmpty) println(s"WARNING: Data document's front-matter will be ignored: ${document.path.toString}")

  def value: YamlValue =
    document.content.fold[YamlValue](YamlNull)(_.parseYaml)

}


/**
  * Our basic representation of site files. Any site document is allowed to have
  *
  * Note: Wanted to have Document as a trait, but it became a case class in the end. What was wrong with declaring it as a trait?
  * Because Document was a trait, there was much more boilerplate code which had to be defined in order to instantiate or
  * extend a Document definition. Because the values described in Document are fixed across the board, it just makes for
  * quicker work to define it as a case class and create adapters to fit it to any future requirements.
  *
  * @param data
  * @param path
  * @param ext
  * @param date
  * @param content
  */
case class Document(data: Map[String, YamlValue],
                    path: Path,
                    name: String,
                    ext: Option[String],
                    date: Option[Calendar],
                    content: Option[String])


object Document {

  val FrontMatter = """(---\s*\n(?:.*?\n?)*)((---|\.\.\.)\s*\n?)""".r

  val DatelessFilename = """^(?:.+/)*(.*)(\.[^.]+)$""".r

  val DatedFilename = """^(?:.+)*(\d{4}-\d{2}-\d{2})-(.*)(\.[^.]+)$""".r

  val DateFormat = new SimpleDateFormat("yyyy-dd-MM")

  case class Content(data: Map[String, YamlValue], content: Option[String])

  case class NameInfo(name: String, ext: Option[String], date: Option[Calendar])

  private[jekyll4s] def parseDate(dateStr: String): Try[Calendar] = Try {
    val c = Calendar.getInstance
    c.setTime(DateFormat.parse(dateStr))
    c
  }

  def filename(doc: Document): String = doc.ext.fold(doc.name)(doc.name + _)

  def readFile(file: Path): Try[String] = Try(Source.fromFile(file.toString).mkString)

  def readContent(path: Path): Try[Content] =
    readFile(path)
      .map {
        str =>
          FrontMatter.findFirstMatchIn(str) match {
            case Some(m) =>
              val data = m.group(1).parseYaml.convertTo[Map[String, YamlValue]]
              val content = m.after.toString
              Content(data, if (content.nonEmpty) Some(content) else None)
            case None =>
              Content(Map.empty, Some(str))
          }
      }

  def readNameInfo(path: Path): Try[NameInfo] =
    path.getFileName.toString match {
      case DatedFilename(dateString, name, ext) =>
        Success(NameInfo(name, Some(ext), parseDate(dateString).toOption))
      case DatelessFilename(name, ext) =>
        Success(NameInfo(name, Some(ext), None))
      case _ =>
        Failure(GeneratorException("Filename did not match expected patterns: " + path.toString))
    }

  /**
    * Load a document located at the provided path using the given content and name-info readers.
    *
    * @param path
    * @param contentReader
    * @param nameReader
    * @return
    */
  def load(path: Path,
           contentReader: Generator.Reader[Path, Content] = readContent,
           nameReader: Generator.Reader[Path, NameInfo]   = readNameInfo): Try[Document] =
    for {
      Content(data, content)    <- contentReader(path)
      NameInfo(name, ext, date) <- nameReader(path)
    } yield Document(data, path, name, ext, date, content)

}

object Page {

  def loadPosts(postDir: Path, baseurl: Option[String]): Try[List[PostDocument]] = Try {
    postDir.toFile.listFiles
      .toList
      .map {
        utils.fileId
      }
      .flatMap {
        Generator.postDocument(postDir, _, baseurl) match {
          case Success(doc) => doc :: Nil
          case Failure(th) =>
            th.printStackTrace()
            Nil
        }
      }
  }

  def loadPages(srcDir: Path, baseurl: Option[String]): Try[List[PageDocument]] = Try {
    srcDir
      .toFile.listFilesRecursiveIf(f => !f.getName.startsWith("_") && f.getName.endsWith(".html"))
      .toList
      .flatMap {
        f =>
          Generator.pageDocument(f.toPath, baseurl) match {
            case Success(doc) => doc :: Nil
            case Failure(th) =>
              th.printStackTrace()
              Nil
          }
      }
  }

}

trait Page {

  val layout: String

  val permalink: String

  val content: String

}


trait Data {

  def apply[A : YamlReader](key: String): A

  def get[A : YamlReader](key: String): Option[A]

}


class DataMap(m: Map[String, YamlValue]) extends Data {

  def apply[A : YamlReader](key: String): A = Try(m(key).convertTo[A]).get

  def get[A : YamlReader](key: String): Option[A] = Try(m(key).convertTo[A]).toOption

  override def toString: String = s"DataMap(${m.toString})"

}


/**
  * version: generator version
  * url: site url
  * pages: list of all pages
  * posts: list of all posts
  * data: data
  */
trait Site {

  val version: String

  val url: String

  val pages: List[Page]

  val posts: List[Page]

  val data: Data

  val page: Page

}


object Site {
  import Generator._


  /**
    * Construct a site object factory which gathers the configuration and page information relevant to
    * the generation of the site object for a particular page.
    *
    * @param version
    * @param pages
    * @param posts
    * @return
    */
  class Factory(version: String,
                url: String,
                data: Reader[DocumentAdapter, Map[String, YamlValue]],
                pages: Reader[Unit, List[PageDocument]],
                posts: Reader[Unit, List[PostDocument]]) { self =>

    def apply(doc: PageDocument): Try[Site] = {
      for {
        pa <- pages({})
        po <- posts({})
        m1 <- data(doc)
      } yield new Site {
        val version = self.version
        val url = self.url
        val pages = pa
        val posts = po
        val data = new DataMap(m1 ++ doc.document.data)
        val page = doc
      }
    }

    def apply(doc: PostDocument): Try[Site] = {
      for {
        pa <- pages({})
        po <- posts({})
        m1 <- data(doc)
      } yield new Site {
        val version = self.version
        val url = self.url
        val pages = pa
        val posts = po
        val data = new DataMap(m1 ++ doc.document.data)
        val page = doc
      }
    }

  }

}

case class GeneratorException(msg: String) extends Exception(msg)
