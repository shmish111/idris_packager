package ip

package object stringext {

  implicit class StringExt(val a: String) extends AnyVal {
    def ` ` (b: String): String = a + " " + b
    def `\n` (b: String): String = a + "\n" + b
    def tab(count: Int): String =
      scala.io.Source.fromString(a).getLines.map(l => (" " * count) + l).mkString("\n")
    def tab: String = tab(2)
  }

}

