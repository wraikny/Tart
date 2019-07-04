module wraikny.Tart.Sockets.Crypt

open System.Security.Cryptography

// https://qiita.com/ak2ie/items/f97ee5265527507f5308


//let internal createEncryptor (aes : AesManaged) encoder =
//    let t = aes.CreateEncryptor()
//    (fun msg ->
//        let bytes = encoder msg
        
//        let encryptedBytes = t.TransformFinalBlock(bytes, 0, bytes.Length)
        
//        encryptedBytes
//    )

//let internal createDecrypter (aes : AesManaged) decoder =
//    let t = aes.CreateDecryptor()
//    (fun bytes ->
//        let decryptedBytes = t.TransformFinalBlock(bytes, 0, bytes.Length)
        
//        decoder decryptedBytes
//    )


open System.Text


// https://qiita.com/kz-rv04/items/62a56bd4cd149e36ca70#rsa%E6%9A%97%E5%8F%B7%E3%81%AE%E5%AE%9F%E8%A3%85
type RSACryptoServiceProvider with
    member rsa.PublicKey = rsa.ToXmlString(false) |> Encoding.UTF8.GetBytes
    member rsa.PrivateKey = rsa.ToXmlString(true) |> Encoding.UTF8.GetBytes