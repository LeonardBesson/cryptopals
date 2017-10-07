object Set1 {

  /* Hex */

  def decodeHex(hex: String): Array[Byte] = {

    if ((hex.length % 2) != 0) {
      throw new IllegalArgumentException("Cannot convert odd number of characters")
    }

    hex
      .toCharArray
      .grouped(2)
      .map { case Array(c1, c2) => hexPairToByte (c1, c2) }
      .toArray
  }

  def hexPairToByte(chars: (Char, Char)): Byte = {
    (charToHex(chars._1) << 4 | charToHex(chars._2) & 0xFF).toByte
  }

  def hexByteToString(byte: Byte): String = {
    s"${hexToChar(byte >>> 4)}${hexToChar(byte & 0x0F)}"
  }

  def encodeHex(bytes: Array[Byte]): String = bytes.flatMap(hexByteToString).mkString

  def charToHex(char: Char): Int = {
    char match {
      case c if c >= '0' && c <= '9' => c - '0'
      case c if c >= 'A' && c <= 'Z' => c - 'A' - '0'
      case c                         => c - 'a' + 10
    }
  }

  def hexToChar(hex: Int): Char = {
    (hex match {
      case b if b < 10 => b + '0'
      case b => b + 'a' + 10
    })
    .toChar
  }

  /* Base64 */

  private val Base64Table = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  private val Base64CharMask = (1 << 6) - 1

  def hexToBase64(hex: Array[Byte]): String = {
    val padLength = hex.length % 3

    val padded = padLength match {
      case 0 => hex
      case 1 => hex ++ Array(0x00).map(_.toByte)
      case 2 => hex ++ Array(0x00, 0x00).map(_.toByte)
    }

    padded
      .grouped(3)
      .flatMap {
        case Array(b1, b2, b3) =>
          val pattern = (b1 << 16) | (b2 << 8) | b3

          Array(
            (pattern >>> 18) & Base64CharMask,
            (pattern >>> 12) & Base64CharMask,
            (pattern >>>  6) & Base64CharMask,
             pattern         & Base64CharMask
          )
          .map(Base64Table.charAt)

        case _ => throw new IllegalStateException("Input not properly padded")
      }
      .zipWithIndex
      .foldLeft(StringBuilder.newBuilder) {
        case (b, (_, i)) if padLength > 0 && i >= padded.length - padLength => b append "="
        case (b, (c, _))                                                    => b append c
      }
      .mkString
  }

  /* XOR */

  def xor(bytes: Array[Byte], key: Array[Byte]): Array[Byte] = {
    bytes
      .zip(key)
      .map { case (char, keyChar) => (char ^ keyChar).toByte }
  }
}
