import  scala.io.StdIn

object labsheet_6 {

  def main(args:Array[String])={

    println("Enter text: ")
    val plaintext = StdIn.readLine()
    println("Enter shift: ")
    val shift = StdIn.readInt()
//Q1
    // Encryption
    val encryptedText = caesarEncrypt(plaintext, shift)
    println("Encrypted: " + encryptedText)

    // Decryption
    val decryptedText = caesarDecrypt(encryptedText, shift)
    println("Decrypted: " + decryptedText)

    //Q2
    // Using the cipher function with the Caesar encryption and decryption functions
    val decryptedTextbycipher = cipher(plaintext, shift, caesarEncrypt, caesarDecrypt)


  }
//Q1
  def caesarEncrypt(plaintext: String, shift: Int): String = {
    val encryptedText = plaintext.map { char =>
      if (char.isLetter) {
        val asciiOffset = if (char.isUpper) 'A' else 'a'
        val encryptedChar = ((char - asciiOffset + shift) % 26 + asciiOffset).toChar
        encryptedChar
      } else {
        char
      }
    }
    encryptedText.mkString
  }
  def caesarDecrypt(ciphertext: String, shift: Int): String = {
    val decryptedText = ciphertext.map { char =>
      if (char.isLetter) {
        val asciiOffset = if (char.isUpper) 'A' else 'a'
        val decryptedChar = ((char - asciiOffset - shift + 26) % 26 + asciiOffset).toChar
        decryptedChar
      } else {
        char
      }
    }
    decryptedText.mkString
  }

  //Q2
  // Cipher Function
  def cipher(data: String, shift: Int, encryptionFunc: (String, Int) => String, decryptionFunc: (String, Int) => String): String = {
    val encryptedData = encryptionFunc(data, shift)
    val decryptedData = decryptionFunc(encryptedData, shift)

    println("Original Data: " + data)
    println("Encrypted Data: " + encryptedData)
    println("Decrypted Data: " + decryptedData)

    decryptedData
  }

}
