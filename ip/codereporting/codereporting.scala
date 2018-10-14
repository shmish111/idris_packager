package ip.codereporting

import language.experimental.macros
import scala.reflect.macros.whitebox.Context
import java.io.File

trait CodeReport {
  def line: Int
  def file: String
}

object CodeReport {

  implicit def CodeReportImplementation: CodeReport = macro reportImpl


  def reportImpl(c: Context): c.Expr[CodeReport] = {
    import c.universe._

    val line = Literal(Constant(c.enclosingPosition.line))

    val absolute = c.enclosingPosition.source.file.file.toURI
    val base = new File(".").toURI

    val path = Literal(Constant(base.relativize(absolute).getPath))

    c.Expr[CodeReport](
      q"""
        new _root_.ip.codereporting.CodeReport() {
          override val line = $line
          override val file = $path
        }
      """
    )
  }
}

package codereporting {
}
