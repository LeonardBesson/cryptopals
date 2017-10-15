import org.scalatest.{FunSuite, MustMatchers}

import scala.io.Source

class TestSet1
  extends FunSuite
    with MustMatchers
{

  test("Challenge 1") {
    val in = Set1.decodeHex("49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d")

    Set1.hexToBase64(in) mustEqual "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
  }

  test("Challenge 2") {
    val in = Set1.decodeHex("1c0111001f010100061a024b53535009181c")
    val key = Set1.decodeHex("686974207468652062756c6c277320657965")

    Set1.xor(in, key) mustEqual Set1.decodeHex("746865206b696420646f6e277420706c6179")
  }

  test("Challenge 3") {
    val in = Set1.decodeHex("1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736")

    val (key, _) = Set1.findSingleByteKey(in)

    key.toChar mustEqual 'X'
    new String(Set1.xor(in, key)) mustEqual "Cooking MC's like a pound of bacon"
  }

  test("Challenge 4") {
    val strings = Source.fromURL("https://cryptopals.com/static/challenge-data/4.txt")
      .getLines()
      .map(Set1.decodeHex)
      .toArray

    val (key, score, bytes) = Set1.detectSingleByteXOR(strings)

    key.toChar mustEqual '5'
    new String(Set1.xor(bytes, key)) mustEqual "Now that the party is jumping\n"
  }

  test("Challenge 5") {
    val in = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"

    val out = Set1.repeatingXOR(in.getBytes, "ICE".getBytes)

    Set1.encodeHex(out) mustEqual "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"
  }

  test("Challenge 6") {
    val in = Source.fromURL("https://cryptopals.com/static/challenge-data/6.txt")
      .getLines()
      .mkString

    val (key, _, _) = Set1.breakRepeatingXOR(Set1.base64ToHex(in))

    new String(key) mustEqual "Terminator X: Bring the noise"
  }

  test("Challenge 7") {
    val in = Source.fromURL("https://cryptopals.com/static/challenge-data/7.txt")
      .getLines()
      .mkString

    val out = Set1.decryptAESECB(Set1.base64ToHex(in), "YELLOW SUBMARINE".getBytes)
  }

  test("Challenge 8") {
    val in = Source.fromURL("https://cryptopals.com/static/challenge-data/8.txt")
      .getLines()
      .map(Set1.decodeHex)
      .toArray

    val ECBEncrypted = Set1.detectAESECB(in)
  }
}
