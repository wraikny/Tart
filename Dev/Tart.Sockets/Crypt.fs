﻿module wraikny.Tart.Sockets.Crypt

open System.Security.Cryptography

// https://qiita.com/ak2ie/items/f97ee5265527507f5308


let internal createEncryptor (aes : AesManaged) encoder =
    let t = aes.CreateEncryptor()
    (fun msg ->
        let bytes = encoder msg
        
        let encryptedBytes = t.TransformFinalBlock(bytes, 0, bytes.Length)
        
        encryptedBytes
    )

let internal createDecrypter (aes : AesManaged) decoder =
    let t = aes.CreateDecryptor()
    (fun bytes ->
        let decryptedBytes = t.TransformFinalBlock(bytes, 0, bytes.Length)
        
        decoder decryptedBytes
    )