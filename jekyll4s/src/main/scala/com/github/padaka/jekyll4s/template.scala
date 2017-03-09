package com.github.padaka.jekyll4s

import java.nio.file.Path

import org.fusesource.scalate._

import scala.reflect.ClassTag
import scala.util.Try


object template {

  type Pair[V] = (String, V)

  /**
    * The Framework trait describes a wrapper for a Templating framework.
    */
  trait Engine {

    def render[P : ClassTag](layout: LayoutDocument)(p1: Pair[P]): Try[String]

    def render2[P1 : ClassTag, P2 : ClassTag](layout: LayoutDocument)(p1: Pair[P1], p2: Pair[P2]): Try[String]

    def render3[P1 : ClassTag, P2 : ClassTag, P3 : ClassTag](layout: LayoutDocument)(p1: Pair[P1], p2: Pair[P2], p3: Pair[P3]): Try[String]

    def render4[P1 : ClassTag, P2 : ClassTag, P3 : ClassTag, P4 : ClassTag](layout: LayoutDocument)(p1: Pair[P1], p2: Pair[P2], p3: Pair[P3], p4: Pair[P4]): Try[String]

  }

  class ScalateEngine(templateType: String, templateDirs: Seq[Path]) extends Engine {

    lazy val engine = new TemplateEngine(templateDirs.map(_.toFile))

    def render[P : ClassTag](layout: LayoutDocument)(p: (String, P)): Try[String] =
      RenderSession.session(layout, engine).map {
        _ .bind(p._1, p._2)
          .output
      }

    override def render2[P1: ClassTag, P2: ClassTag](layout: LayoutDocument)(p1: (String, P1), p2: (String, P2)): Try[String] =
      RenderSession.session(layout, engine).map {
        _ .bind(p1._1, p1._2)
          .bind(p2._1, p2._2)
          .output
      }

    override def render3[P1: ClassTag, P2: ClassTag, P3: ClassTag](layout: LayoutDocument)(p1: (String, P1), p2: (String, P2), p3: (String, P3)): Try[String] =
      RenderSession.session(layout, engine).map {
        _ .bind(p1._1, p1._2)
          .bind(p2._1, p2._2)
          .bind(p3._1, p3._2)
          .output
      }

    override def render4[P1: ClassTag, P2: ClassTag, P3: ClassTag, P4: ClassTag](layout: LayoutDocument)(p1: (String, P1), p2: (String, P2), p3: (String, P3), p4: (String, P4)): Try[String] =
      RenderSession.session(layout, engine).map {
        _ .bind(p1._1, p1._2)
          .bind(p2._1, p2._2)
          .bind(p3._1, p3._2)
          .bind(p4._1, p4._2)
          .output
      }


    object RenderSession {

      def session(layout: LayoutDocument, engine: TemplateEngine = engine): Try[RenderSession] =
        Try {
          val src = TemplateSource.fromText(
            uri           = layout.document.path.getFileName.toString,
            templateText  = layout.document.content.getOrElse("")
          ).templateType(templateType)
          val ctx = new DefaultRenderContext(layout.document.path.toString, engine)

          RenderSession(src, ctx, Nil, engine)
        }

    }


    case class RenderSession(templateSource: TemplateSource, context: RenderContext, bindings: List[Binding], engine: TemplateEngine) {

      def bind[P : ClassTag](name: String, param: P) = {
        // setAttribute side affect
        context.setAttribute(name, Some(param))
        copy(bindings = Binding.of[P](name) :: bindings)
      }

      // `engine.load` and `engine.compile` have the same method signature but load caches the template
      // in the TemplateEngine instance.
      def output = context.capture(engine.load(templateSource, bindings))

    }
  }

}


