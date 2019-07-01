module wraikny.Tart.Sockets.Crypt

open System.Security.Cryptography

// https://qiita.com/ak2ie/items/f97ee5265527507f5308

let internal encryptor (crypto : ICryptoTransform) encoder msg =
    let bytes = encoder msg

    let encryptedBytes = crypto.TransformFinalBlock(bytes, 0, bytes.Length)

    encryptedBytes

let internal decryptor (crypto : ICryptoTransform) decoder bytes =
    let decryptedBytes = crypto.TransformFinalBlock(bytes, 0, bytes.Length)

    decoder decryptedBytes