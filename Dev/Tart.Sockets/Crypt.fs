module wraikny.Tart.Sockets.Crypt

open System.Security.Cryptography
open System.Text


// https://qiita.com/kz-rv04/items/62a56bd4cd149e36ca70#rsa%E6%9A%97%E5%8F%B7%E3%81%AE%E5%AE%9F%E8%A3%85
type RSACryptoServiceProvider with
    member inline rsa.PublicKey = rsa.ToXmlString(false) |> Encoding.UTF8.GetBytes
    member inline rsa.PrivateKey = rsa.ToXmlString(true) |> Encoding.UTF8.GetBytes