package com.jakway.term.test.run

import java.io.File

import com.jakway.term.numeric.errors.SimError
import org.apache.batik.util.XMLResourceDescriptor

import scala.util.{Failure, Success, Try}

/**
  * checks that we can read back the passed SVG without throwing an exception
  */
object CheckSVG {

  def apply(svg: File): Either[SimError, Unit] = {
    //basic file checks
    if(!svg.exists) {
      Left(SVGDoesNotExist(svg))
    } else if(!svg.canRead) {
      Left(new SVGFileError(svg, s"cannot read $svg"))
    } else if(svg.isDirectory) {
      Left(new SVGFileError(svg, s"$svg is a directory"))

    } else {
      Try {
        import org.apache.batik.anim.dom.SAXSVGDocumentFactory
        val parser: String = XMLResourceDescriptor.getXMLParserClassName();
        val factory: SAXSVGDocumentFactory = new SAXSVGDocumentFactory(parser);
        val doc = factory.createDocument(svg.toURI.toString);
        if(doc == null) {
          throw LoadException(svg, "Returned Document was null")
        }
      }.map(_ => {}) match {
        case Success(_) => Right({})
        case Failure(t) => Left(new LoadException(svg, t))
      }
    }
  }

  class CheckSVGError(val svg: File, val err: String)
    extends SimError(s"Error while checking SVG $svg: $err")

  class SVGFileError(override val svg: File, override val err: String)
    extends CheckSVGError(svg, err)

  case class SVGDoesNotExist(override val svg: File)
    extends SVGFileError(svg, "file does not exist")

  case class LoadException(override val svg: File, override val err: String)
    extends CheckSVGError(svg, err) {
    def this(svg: File, throwable: Throwable) {
      this(svg, s"caught $throwable while loading file")
    }
  }
}
