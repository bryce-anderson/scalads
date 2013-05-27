package macro_readers

/**
 * @author Bryce Anderson
 *         Created on 4/7/13 at 10:04 AM
 */

class JsonStringCursor(txt: String) extends JsonTextCursor { self =>

  private var current = 0
  private val maxLength = txt.length
  trim()


  def empty = {trim(); (current >= maxLength)}

  def remainder = txt.substring(current)

  def nextChar() = { current += 1; txt.charAt(current-1)}

  def findNextString(): JsonString = {
    trim()
    if(txt.charAt(current) != '"') failStructure(s"Next token is not a string: '$remainder'")
    var begin = current + 1
    var end = begin

    var builder: StringBuilder = null
    var chr = txt.charAt(end)
    while(chr != '"') {
      if(txt.charAt(end) == '\\') {
        if (builder == null) builder = new StringBuilder(txt.length)

        builder.append(txt.substring(begin, end))
        end +=1
        txt.charAt(end) match {
          case c if(c == '"' || c == '\\' || c == '/') => builder.append(c)
          case c if (c == 'b') => builder.append('\b')
          case c if (c == 'f') => builder.append('\f')
          case c if (c == 'n') => builder.append('\n')
          case c if (c == 'r') => builder.append('\r')
          case c if (c == 't') => builder.append('\t')
          case c if (c == 'u') => { builder.append(Integer.parseInt(txt.substring(end+1, end+5), 16).toChar); end +=4 }
          case c => failParse(s"Bad escaped character: '$c'.")
        }
        begin = end + 1
      }

      end += 1
      require(end < txt.length)
      chr = txt.charAt(end)
    }

    if (begin == current + 1) {
      current = end + 1
      JsonString(txt.substring(begin, end))
    }
    else {
      current = end + 1
      if (begin < end) builder.append(txt.substring(begin, end))
      JsonString(builder.result())
    }
  }

  @inline
  final private def trim() { while(current < maxLength && isWhitespace(txt.charAt(current))) current += 1 }

  // returns true if finished, otherwise false
  def zoomPastSeparator(sep: Char, end: Char): Boolean = {
    trim()
    val chr = txt.charAt(current)
    if (chr == end) true
    else if (chr != sep) failParse(s"Separator '${txt.charAt(current)}' is not '$sep' or '$end'")
    else {
      current += 1
      trim()      // TODO: Can I elimenate the trim from here?
      false
    }
  }

  def findNextNumber(): JsonNumber = {
    trim()
    var end = current
    while(end < maxLength && isNumberChar(txt.charAt(end))) end += 1

    if(end == current) failStructure(s"Next token is not number: ${txt.charAt(end)}")
    else {
      val str = txt.substring(current, end)
      current = end
      JsonNumber(str)
    }
  }

  def findNextBoolean(): JsonBool = {
    trim()
    if (txt.charAt(current)     == 't' &&
      txt.charAt(current + 1) == 'r' &&
      txt.charAt(current + 2) == 'u' &&
      txt.charAt(current + 3) == 'e') {
      current = current + 4
      JsonBool(true)
    }
    else if ( txt.charAt(current)     == 'f' &&
      txt.charAt(current + 1) == 'a' &&
      txt.charAt(current + 2) == 'l' &&
      txt.charAt(current + 3) == 's' &&
      txt.charAt(current + 4) == 'e') {
      current = current + 5
      JsonBool(false)
    }
    else failStructure(s"Next token is not of type boolean: ${txt.substring(current, 5)}")
  }

  def extractField(): JsonField = {
    trim()
    if(maxLength == current) {
      failStructure(s"Tried to extract field that doesn't exist!")
    } else txt.charAt(current) match {
      case '"'                    => findNextString()
      case '{'                    => JsonObject(new TextObjectReader(self))
      case '['                    => JsonArray(new TextArrayIterator(self))
      case c if (isNumberChar(c)) => findNextNumber()
      case c if(c == 't' || c == 'f') => findNextBoolean()
      case 'n' if (txt.charAt(current + 1) == 'u' && txt.charAt(current + 2) == 'l' && txt.charAt(current + 3) == 'l') =>
        current += 4
        Null

      case _ => failParse(s"Cursor not at valid field.")
    }
  }

  override def failParse(msg: String) = sys.error(msg)
}