package FIRST

object Tut_06 {
    def main(args: Array[String]): Unit = {
    val plaintext = scala.io.StdIn.readLine("Enter the text:: ")
    val shift = scala.io.StdIn.readLine("Enter the number of shifts ::").toInt

    val encryptedText = caesarCipher(plaintext, shift, caesarEncrypt)
    println("Encrypted :: " + encryptedText)

    val decryptedText = caesarCipher(encryptedText, shift, caesarDecrypt)
    println("Decrypted :: " + decryptedText)
  }


  def caesarEncrypt(plainText: String, shift: Int): String = {
    val alphabetSize = 26

    plainText.map { char_Input =>
      if (char_Input.isLetter) {
        val isUpperCase = char_Input.isUpper
        val base = if (isUpperCase) 'A' else 'a'
        val shiftedChar = ((char_Input - base + shift + alphabetSize) % alphabetSize + base).toChar

        if (isUpperCase) shiftedChar
        else shiftedChar.toLower
      }
      else {
        char_Input
      }
    }
  }


  def caesarDecrypt(cipherText: String, shift: Int): String = {
    caesarEncrypt(cipherText, -shift)
  }


  def caesarCipher(text: String, shift: Int, func: (String, Int) => String): String = {
    func(text, shift)
  }

}