package macro_readers

import scala.annotation.tailrec

/**
 * @author Bryce Anderson
 *         Created on 4/7/13 at 10:03 AM
 */

class JsonReaderCursor(reader: java.io.Reader, bufferSize: Int = 150) extends JsonTextCursor { self =>

  private val readBuffer = new Array[Char](bufferSize)
  private var current = 0
  private var max = 0

  private var isEmpty = false

  def nextChar(): Char = {
    if (isEmpty) failParse("Cannot get nextChar from empty TextCursor")

    if (current < max) {
      val c = readBuffer(current)
      current += 1
      c
    }
    else {
      max = reader.read(readBuffer)
      if( -1 == max) isEmpty = true
      nextChar()
    }
  }

  private def peak(): Char = {
    val c = nextChar
    current -= 1
    c
  }

  @inline @tailrec
  private final def trim() {
    if(isWhitespace(nextChar())) trim()
    else current -= 1           // Found it, so need to rewind one char
  }

  def empty: Boolean = isEmpty

  def findNextString() = {
    trim()
    var chr = nextChar()
    if(chr != '"') failStructure(s"Next token is not a string: $chr")

    val builder = new StringBuilder(15)
    chr = nextChar()
    while(chr != '"') {
      if(chr == '\\') {
        nextChar() match {
          case c if(c == '"' || c == '\\' || c == '/') => builder.append(c)
          case c if (c == 'b') => builder.append('\b')
          case c if (c == 'f') => builder.append('\f')
          case c if (c == 'n') => builder.append('\n')
          case c if (c == 'r') => builder.append('\r')
          case c if (c == 't') => builder.append('\t')
          case c if (c == 'u') => builder.append(
            Integer.parseInt(new String(Array(nextChar, nextChar, nextChar, nextChar)), 16).toChar
          )
          case c => failParse(s"Bad escaped character: '$c'.")
        }
      } else builder.append(chr)
      chr = nextChar()
    }
    JsonString(builder.result())
  }

  // returns true if finished, otherwise false
  def zoomPastSeparator(sep: Char, end: Char): Boolean = {
    trim()
    nextChar match {
      case c if (c == sep) => false
      case c if (c == end) => current -= 1; true
      case c => failParse(s"Separator '$c' is not '$sep' or '$end'")
    }
  }

  def findNextNumber(): JsonNumber = {
    trim()
    val builder = new StringBuilder(15)
    var chr = nextChar
    while(isNumberChar(chr)) {
      builder.append(chr)
      chr = nextChar
    }
    current -= 1
    JsonNumber(builder.result)
  }

  def findNextBoolean(): JsonBool = {
    val start = nextChar
    if (start == 'f' && nextChar == 'a' && nextChar == 'l' && nextChar == 's' && nextChar == 'e') JsonBool(false)
    else if (start == 't' && nextChar == 'r' && nextChar == 'u' && nextChar == 'e') JsonBool(true)
    else failStructure(s"Next token is not a boolean: $start")
  }

  def extractField() = {
    trim()
    peak() match {
      case '"'                        => findNextString()
      case '{'                        => JsonObject(new TextObjectReader(self))
      case '['                        => JsonArray(new TextArrayIterator(self))
      case c if (isNumberChar(c))     => findNextNumber()
      case 'f' if (nextChar == 'f' && nextChar == 'a' && nextChar == 'l' && nextChar == 's' && nextChar == 'e') => JsonBool(false)
      case 't' if (nextChar == 't' && nextChar == 'r' && nextChar == 'u' && nextChar == 'e') => JsonBool(true)
      case 'n' if (nextChar == 'u' && nextChar == 'l' && nextChar == 'l') => Null

      case e => failParse(s"Cursor not at valid field. Char: $e")
    }
  }
}

