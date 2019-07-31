module wraikny.Tart.Socket.Crypt

open System.Security.Cryptography
open System.Text


// https://qiita.com/kz-rv04/items/62a56bd4cd149e36ca70#rsa%E6%9A%97%E5%8F%B7%E3%81%AE%E5%AE%9F%E8%A3%85
type RSACryptoServiceProvider with
    member inline rsa.PublicKey = rsa.ToXmlString(false) |> Encoding.UTF8.GetBytes
    member inline rsa.PrivateKey = rsa.ToXmlString(true) |> Encoding.UTF8.GetBytes


type AesCryptoServiceProvider with
    member inline aes.Encrypt(bytes : byte [], ?offset, ?count) =
        let offset = defaultArg offset 0
        let count = defaultArg count bytes.Length
        aes.CreateEncryptor().TransformFinalBlock(bytes, offset, count)

    member inline aes.Decrypt(bytes : byte [], ?offset, ?count) =
        let offset = defaultArg offset 0
        let count = defaultArg count bytes.Length
        aes.CreateDecryptor().TransformFinalBlock(bytes, offset, count)