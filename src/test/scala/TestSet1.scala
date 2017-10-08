import org.scalatest.{FunSuite, MustMatchers}

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

    val key = Set1.findSingleByteKey(in)

    key.toChar mustEqual 'X'
    new String(Set1.xor(in, key)) mustEqual "Cooking MC's like a pound of bacon"
  }
}
