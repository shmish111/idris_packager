package ip

package object stringext {

  implicit class StringExt(val a: String) extends AnyVal {
    def ` ` (b: String): String = a + " " + b
    def `\n` (b: String): String = a + "\n" + b
  }

}

