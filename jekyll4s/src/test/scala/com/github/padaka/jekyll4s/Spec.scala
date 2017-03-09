package com.github.padaka.jekyll4s

import java.io.File
import java.nio.file.Paths

import net.jcazevedo.moultingyaml._
import org.scalatest._
import org.fusesource.scalate._

import scala.io.Source
import scala.util.{Success, Try}


class Spec extends FlatSpec with Matchers {
  import utils.Implicits._

  val testClasspath = new File(getClass.getResource("/").toURI)
  val src = new File(testClasspath, "source")
  val _config = new File(src, "_config.yml")
  val _configObject = GeneratorConfig(
    title           = "M$!L#",
    email           = "m41l3@yahoo.com",
    description     = "This is the awesome site of Paul Kang\n",
    baseurl         = Some("/blog"),
    url             = "http://m41l3.com",
    source          = src.getAbsolutePath,
    destination     = "_site",
    template_engine = "jade",
    defaults        = List(
      Defaults(DefaultsScope(Some("path"), None), Map("layout" -> YamlString("site")))
    )
  )
  val _layouts = new File(src, "_layouts")
  val _posts = new File(src, "_posts")
  val _data = new File(src, "_data")
  val _includes = new File(src, "_includes")
  val emptyDoc: Document = Document(Map.empty, ".".asPath, "", None, None, None)

  def fileContent(f: File): String = Source.fromFile(f).mkString

  def createFactory(data: Map[String, YamlValue] = Map.empty,
                    pages: List[PageDocument] = Nil,
                    posts: List[PostDocument] = Nil) =
    new Site.Factory(
      version = "0.0",
      url = "localhost",
      data  = _ => Success(data),
      pages = _ => Success(pages),
      posts = _ => Success(posts)
    )


  behavior of "Scalate embedded templating"

  val engine = new TemplateEngine
  val helloWorldJade = new File(getClass.getResource("/helloworld.jade").toURI)
  val helloWorldIncludeJade = new File(getClass.getResource("/helloworld_include.jade").toURI)
  val helloWorldMustache = new File(getClass.getResource("/helloworld.mustache").toURI)

  it should "[impl] load layouts" in {
    val src1 = TemplateSource.fromText(helloWorldJade.getName, fileContent(helloWorldJade))
    val src2 = TemplateSource.fromText(helloWorldMustache.getName, fileContent(helloWorldMustache))
    engine.layout(src1, Map("name" -> "Paul")) should include("Hello, Paul!")
    engine.layout(src2, Map("name" -> "Paul")) should include("Hello, Paul!")
  }

  it should "[impl] render layouts" in {
    val ctx = new DefaultRenderContext("", engine)
    val jade = engine.load(helloWorldJade.getAbsolutePath, List(Binding("name", "String")))
    val mustache = engine.load(helloWorldMustache.getAbsolutePath, List(Binding("name", "String")))

    ctx.setAttribute("name", Some("Paul"))
    ctx.capture(jade) should include("Hello, Paul")
    ctx.capture(mustache) should include("Hello, Paul")
  }

  it should "render templates with includes" in {
    val rendered: Try[String] =
      Document
        .load(helloWorldIncludeJade.toPath)
        .map(new LayoutDocument(_))
        .flatMap(new template.ScalateEngine("jade", _includes.toPath :: Nil).render(_)("name" -> "Paul"))
    rendered should be(an[Success[_]])
    rendered.get should include("Hello, Paul!")
    rendered.get should include("Batteries Included!")
  }


  /**
   * Flexmark apparently has Jekyll-YAML frontmatter parsing extension which might perform
   * better then SnakeYAML which should be usable at some point, but going to stick with
   * SnakeYAML since that seems to be the more "de-facto" way to parse YAML and pluggable
   * implementation frontmatter parsing.
   */
  behavior of "YAML Front-matter parsing"

  it should "[impl] parse valid frontmatter" in {
    import DefaultYamlProtocol._
    val frontmatter =
      """|---
         |title: moultingyaml
         |author: jcazevedo
         |layout: default
         |""".stripMargin
    val yaml = frontmatter.parseYaml
    val m = yaml.convertTo[Map[String, String]]
    m("title") should be("moultingyaml")
    m("author") should be("jcazevedo")
    m("layout") should be("default")
  }

  it should "[impl] fail to parse invalid frontmatter" in {
    val frontmatter =
      """|---
         |title: moultingyaml
         |author: jcazevedo
         |layout: default
         |---
         |""".stripMargin

    an[Exception] should be thrownBy frontmatter.parseYaml
  }

  it should "[impl] load a file's frontmatter" in {
    val frontMatter =
      """---
        |title: test
        |title2: test2
        |""".stripMargin

    val delimiter =
      """---
        |""".stripMargin

    val content =
      """
        |hey der {title}
      """.stripMargin

    // w/ content
    val mOpt = Document.FrontMatter.findFirstMatchIn(frontMatter + delimiter + content)
    mOpt shouldBe an[Some[_]]

    val m = mOpt.get
    m.group(1).trim shouldEqual frontMatter.trim
    m.group(1).parseYaml shouldEqual YamlObject(YamlString("title") -> YamlString("test"), YamlString("title2") -> YamlString("test2"))
    m.group(2).trim shouldEqual delimiter.trim
    m.after.toString.trim shouldEqual content.trim
  }

//  it should "load a _data file's front-matter" in fail()


  behavior of "Markdown processor"
  import com.vladsch.flexmark.html.HtmlRenderer
  import com.vladsch.flexmark.parser.Parser

  it should "[impl] process raw markdown" in {
    val parser = Parser.builder().build()
    val renderer = HtmlRenderer.builder().build()
    val markdown = "# Hello, world!"
    val doc = parser.parse(markdown)

    renderer.render(doc) should include("<h1>")
    renderer.render(doc) should include("Hello, world!")
    renderer.render(doc) should include("</h1>")
  }


  behavior of "Layouts"

  it should "[impl] render a layout" in {
    val rendered: Try[String] =
      Document
        .load(helloWorldJade.toPath)
        .map(new LayoutDocument(_))
        .flatMap(new template.ScalateEngine("jade", Nil).render(_)("name" -> "Paul"))
    rendered should be(an[Success[_]])
    rendered.get should include("Hello, Paul!")
  }

  it should "[impl] render a layout with site and post data" in {
    val testPage = new Page {
      val layout: String = "test-layout"
      val permalink: String = "/test-permalink"
      val content: String = "test-content"
    }
    val rendered: Try[String] =
      for {
        post: PostDocument <- Document.load(_posts.toPath.resolve("2017-02-06-test-post.md")).map(new PostDocument(_, _configObject.baseurl))
        dada: Data  <- Generator.dataObject(_configObject, _data.toPath, post)
        site: Site  = new Site {
          val version: String = "test-version"
          val url: String = "/test-url"
          val posts: List[Page] = testPage :: Nil
          val pages: List[Page] = Nil
          val data: Data = dada
          val page: Page = testPage
        }
        layout      <- Document.load(_layouts.toPath.resolve("site.html")).map(new LayoutDocument(_))
        out         <- new template.ScalateEngine("jade", Nil).render2(layout)("site" -> site, "page" -> post)
      } yield out
    rendered should be(an[Success[_]])
    rendered.get should include("<div>")
    rendered.get should include("<p>test-version</p>")
    rendered.get should include("<p>/test-url</p>")
    rendered.get should include("</div>")
  }


  behavior of "YAML Lenses"

  import monocle.Lens

  it should "[impl] Monocle should work with MoutlingYAML YAML types" in {
    import net.jcazevedo.moultingyaml._
    import net.jcazevedo.moultingyaml.DefaultYamlProtocol._

    val yaml = YamlObject(YamlString("test-key") -> YamlString("test-value"))

    val yString: Lens[YamlValue, String] =
      Lens[YamlValue, String] (_.convertTo[String])((str: String) => _ => YamlString(str))

    def yObjValue(k: YamlValue): Lens[YamlObject, YamlValue] =
      Lens[YamlObject, YamlValue](_.fields(k))((v: YamlValue) => (obj: YamlObject) => obj.copy(fields = obj.fields + (k -> v)))

    val testValueLens = yObjValue(YamlString("test-key")) ^|-> yString

    testValueLens.get(yaml) shouldEqual "test-value"
    testValueLens.modify(_ => "modified-test-value")(yaml) shouldEqual YamlObject(YamlString("test-key") -> YamlString("modified-test-value"))
  }


  behavior of "Readers"

  it should "[impl] load site configuration from config file" in {
    import config.protocol._
    val cfgYaml = Source.fromFile(_config).mkString.parseYaml
    val cfg = cfgYaml.convertTo[GeneratorConfig]

    // Prepending the configured source directory with the current runtime's test classpath
    // Were assuming that the provided path is relative
    // TODO: utilities for determining if path is relative or absolute
    val normalizedCfg = cfg.copy(source = new File(testClasspath, cfg.source).getPath)
    normalizedCfg shouldEqual _configObject
  }

  it should "[impl] load layout data from <src>/_layouts/" in {
    _layouts.isDirectory should be(true)
    val layout: Try[Document] = Document.load(Paths.get(_layouts.toURI).resolve("site.html"))
    layout should be(an[Success[_]])
    val doc = layout.get
    doc.content should be(an[Some[_]])
    doc.data should be(Map("title" -> YamlString("Hi")))
    doc.ext should be(Some(".html"))
  }

  it should "load layout data from <src>/_layouts/" in {
    val layout: Try[LayoutDocument] = Generator.layoutDocument(_layouts.toPath, "site")
    layout should be(an[Success[_]])
    val doc = layout.get.document
    doc.content should be(an[Some[_]])
    doc.data should be(Map("title" -> YamlString("Hi")))
    doc.date should be(an[None.type])
  }

  it should "default values are overrideable by frontmatter" in {
    import DefaultYamlProtocol._

    val defaults = Defaults(DefaultsScope(None, None), Map("default" -> YamlString("default-value")))
    val configWithDefault = _configObject.copy(defaults = defaults :: Nil)

    val doc = emptyDoc.copy(data = Map("default" -> YamlString("override-value")))
    val page = new PageDocument(doc, configWithDefault.baseurl)
    val factory = createFactory(pages = page :: Nil)

    val site = factory(page)
    site should be(an[Success[_]])
    site.get.data.get[String]("default") should be(Some("override-value"))
  }

  it should "default values are settable in _config.yml by path" in {
    import DefaultYamlProtocol._

    val defaults = Defaults(DefaultsScope(Some("path"), None), Map("default" -> YamlString("default-value")))
    val configWithDefault = _configObject.copy(defaults = defaults :: Nil)

    val doc = emptyDoc
    val doc2 = emptyDoc.copy(path = "path".asPath)
    val page = new PageDocument(doc, configWithDefault.baseurl)
    val page2 = new PageDocument(doc2, configWithDefault.baseurl)
    val factory = createFactory(pages = page :: page2 :: Nil)

    val site = factory(page)
    site should be(an[Success[_]])
    site.get.data.get[String]("default") should be(None)

    val site2 = factory(page2)
    site2 should be(an[Success[_]])
    site2.get.data.get[String]("default") should be(Some("default-value"))
  }

  it should "frontmatter default values are settable in _config.yml by types" in {
    import DefaultYamlProtocol._

    val defaults = Defaults(DefaultsScope(None, Some("posts")), Map("default" -> YamlString("default-value")))
    val configWithDefault = _configObject.copy(defaults = defaults :: Nil)

    val doc = emptyDoc
    val doc2 = emptyDoc
    val page = new PageDocument(doc, configWithDefault.baseurl)
    val post = new PostDocument(doc2, configWithDefault.baseurl)
    val factory = createFactory(pages = page :: Nil, posts = post :: Nil)

    val site = factory(page)
    site should be(an[Success[_]])
    site.get.data.get[String]("default") should be(None)

    val site2 = factory(post)
    site2 should be(an[Success[_]])
    site2.get.data.get[String]("default") should be(Some("default-value"))

  }

//  it should "pathed frontmatter default values are applied to 'categorized' frontmatter" in fail()
//  it should "pathed frontmatter default values are applied to 'nested' frontmatter" in fail()


  behavior of "Site"

//  it should "'posts' contains a sorted list of post objects" in fail()
//  it should "'categories' groups posts by categories" in fail()
//  it should "'tags' groups posts by tags" in fail()
//  it should "'url' contains the site url" in fail()
//  it should "'version' contains the site generator version" in fail()

  behavior of "Page"

//  it should "'title' is the post title" in fail()
//  it should "'path' is the path to the local content" in fail()
//  it should "'published' determines if the page should get generated" in fail()
//  it should "'url' contains the site url" in fail()
//  it should "only 'path' is overrideable from the frontmatter" in fail()
//  it should "contain 'page.time'" in fail()
//  it should "'page.posts'" in fail()


  behavior of "Framework"

  it should "generate a static site" in {
    val cfg = GeneratorConfig(
      title           = "M$!L#",
      email           = "m41l3@yahoo.com",
      description     = "This is the awesome site of Paul Kang\n",
      baseurl         = Some("/blog"),
      url             = "http://m41l3.com",
      source          = src.getAbsolutePath,
      destination     = "_site",
      template_engine = "jade",
      defaults        = List(
        Defaults(DefaultsScope(Some("path"), None), Map("layout" -> YamlString("default")))
      )
    )

    Generator.generate(cfg)
  }

//  it should "enter pre-render hooks" in fail()
//  it should "enter post-render hooks" in fail()
//  it should "enter pre-write hooks" in fail()
//  it should "enter post-write hooks" in fail()

}
