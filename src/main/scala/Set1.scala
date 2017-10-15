import javax.crypto.Cipher
import javax.crypto.spec.SecretKeySpec

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

  def encodeHex(bytes: Array[Byte]): String = {
    bytes.flatMap(hexByteToString).mkString
  }

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
      case b => b + 'a' - 10
    })
    .toChar
  }

  /* Base64 */

  private val Base64Table = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  private val Base64ByteTable = Base64Table.getBytes
  private val Base64CharMask = (1 << 6) - 1

  def hexToBase64(hex: Array[Byte]): String = {
    val padLength = (3 - (hex.length % 3)) % 3

    val padded = padLength match {
      case 0 => hex
      case 1 => hex ++ Array(0x00).map(_.toByte)
      case 2 => hex ++ Array(0x00, 0x00).map(_.toByte)
    }

    val base64 = padded
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
      .mkString

      base64.slice(0, base64.length - padLength) ++ "=" * padLength
  }

  def base64ToHex(base64: String): Array[Byte] = {
    val padIndex = base64.indexOf("=")

    val (unpadded, padLength) = padIndex match {
      case -1 => (base64, 0)
      case _  => (base64.slice(0, padIndex) ++ "A" * (base64.length - padIndex), base64.length - padIndex)
    }

    val hex = unpadded
      .getBytes
      .grouped(4)
      .map(_.map(Base64ByteTable.indexOf))
      .flatMap {
        case Array(w1, w2, w3, w4) =>
          val pattern = (w1 << 18) | (w2 << 12) | (w3 << 6) | w4

          Array(
            (pattern >>> 16) & 0xFF,
            (pattern >>>  8) & 0xFF,
             pattern         & 0xFF,
          )
          .map(_.toByte)
      }
      .toArray

      hex.slice(0, hex.length - padLength)
  }

  /* XOR */

  def xor(bytes: Array[Byte], key: Array[Byte]): Array[Byte] = {
    bytes
      .zip(key)
      .map { case (char, keyChar) => (char ^ keyChar).toByte }
  }

  def xor(bytes: Array[Byte], key: Byte): Array[Byte] = bytes.map(b => (b ^ key).toByte)


  /* Challenge 3 */

  def findSingleByteKey(hex: Array[Byte]): (Byte, Float) = {
    SingleByteXorDecryptor.findKey(hex)
  }

  /* Challenge 4 */

  def detectSingleByteXOR(sample: Array[Array[Byte]]): (Byte, Float, Array[Byte]) = {
    val (key, score, in) = sample
      .map { in =>
        val (key, score) = findSingleByteKey(in)

        (key, score, in)
      }
      .maxBy(_._2)

    (key, score, in)
  }

  object SingleByteXorDecryptor {
    /* https://www.math.cornell.edu/~mec/2003-2004/cryptography/subs/frequencies.html */
    val CharFreqTable: Map[Char, Float] = Map(
      'E' -> 12.02f,
      'T' ->  9.10f,
      'A' ->  8.12f,
      'O' ->  7.68f,
      'I' ->  7.31f,
      'N' ->  6.95f,
      ' ' ->  6.55f,
      'S' ->  6.28f,
      'R' ->  6.02f,
      'H' ->  5.92f,
      'D' ->  4.32f,
      'L' ->  3.98f,
      'U' ->  2.88f,
    )

    def getCharFrequencyScore(bytes: Array[Byte]): Float = {
      new String(bytes)
        .toUpperCase
        .map(SingleByteXorDecryptor.CharFreqTable.getOrElse(_, 0.00f))
        .sum
    }

    def findKey(hex: Array[Byte]): (Byte, Float) = {
      val (bestKey, score) =
        (0x00 to 0xFF)
          .map(key => (key, xor(hex, key.toByte)))
          .map { case (key, reversed) => (key.toByte, SingleByteXorDecryptor.getCharFrequencyScore(reversed)) }
          .maxBy(_._2)

      (bestKey, score)
    }
  }

  /* Challenge 5 */

  def repeatingXOR(bytes: Array[Byte], key: Array[Byte]): Array[Byte] = {
    bytes
      .grouped(key.length)
      .flatMap(xor(_, key))
      .toArray
  }

  /* Challenge 6 */

  def hammingDistance(in1: Array[Byte], in2: Array[Byte]): Int = {
    if (in1.length != in2.length) {
      throw new IllegalArgumentException("hammingDistance; length mismatch")
    }

    in1
      .zip(in2)
      .withFilter { case (b1, b2) => b1 != b2 }
      .map { case (b1, b2) => Integer.bitCount(b1 ^ b2) }
      .sum
  }

  def breakRepeatingXOR(bytes: Array[Byte]): (Array[Byte], Float, Array[Byte]) = {
    val keySizes =
      (2 to 40)
      .map { keySize =>
        val blocks =
          bytes
          .grouped(keySize)
          .toList
          .dropRight(1)

        val normalizedDistance =
          blocks
          .zip(blocks.tail)
          .map { case (block1, block2) =>
              hammingDistance(block1, block2).toDouble / keySize
          }
          .sum / (blocks.length - 1)

        (keySize, normalizedDistance)
      }
      .sortBy(_._2)
      .take(3)
      .map(_._1)

    val keys =
      keySizes
      .map { keySize =>
        val grouped =
          bytes
          .grouped(keySize)
          .toList

        val transposed =
          if (grouped.last.length == keySize) {
            grouped.transpose
          } else {
            val lastBlock = grouped.last

            grouped
              .dropRight(1)
              .transpose
              .zipWithIndex
              .map {
                case (block, i) =>
                  block ++ (lastBlock.lift(i) match {
                    case Some(b) => List(b)
                    case None => List()
                  })
              }
          }

        transposed
          .map(block => findSingleByteKey(block.toArray)._1)
          .toArray
      }

      keys
        .map { key =>
          val message = repeatingXOR(bytes, key)

          val score = SingleByteXorDecryptor.getCharFrequencyScore(message)

          (key, score, message)
        }
        .maxBy(_._2)
  }

  /* Challenge 7 */

  def decryptAESECB(bytes: Array[Byte], key: Array[Byte]): Array[Byte] = {
    val cipher = Cipher.getInstance("AES/ECB/PKCS5Padding")
    val secretKeySpec = new SecretKeySpec(key, "AES")

    cipher.init(Cipher.DECRYPT_MODE, secretKeySpec)
    cipher.doFinal(bytes)
  }

  /* Challenge 8 */

  def detectAESECB(cipherTexts: Array[Array[Byte]]): Array[Byte] = {
    cipherTexts
      .map(cipherText => (cipherText, cipherText.grouped(16)))
      .map { case (cipherText, blocks) =>

        val score = blocks.map(block => blocks.count(block sameElements _)).sum

        (cipherText, score)
      }
      .maxBy(_._2)
      ._1
  }
}
