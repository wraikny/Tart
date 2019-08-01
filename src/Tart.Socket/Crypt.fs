module wraikny.Tart.Socket.Crypt

open System
open System.Security.Cryptography
open System.Text
open System.Xml


// https://qiita.com/kz-rv04/items/62a56bd4cd149e36ca70#rsa%E6%9A%97%E5%8F%B7%E3%81%AE%E5%AE%9F%E8%A3%85
// https://qiita.com/mxProject/items/7531e93386907cdff2d0
// https://gist.github.com/Jargon64/5b172c452827e15b21882f1d76a94be4/
type RSACryptoServiceProvider with
    member rsa.FromXmlStringForDotNetCore(xmlString : string) =
        let mutable parameters = RSAParameters()
        let xmlDoc = XmlDocument()
        xmlDoc.LoadXml(xmlString)
        if xmlDoc.DocumentElement.Name.Equals("RSAKeyValue") then
            for node in xmlDoc.DocumentElement.ChildNodes do
                let x = Convert.FromBase64String node.InnerText

                node.Name |> function
                | "Modulus" -> parameters.Modulus <- x
                | "Exponent" -> parameters.Exponent <- x
                | "P" -> parameters.P <- x
                | "Q" -> parameters.Q <- x
                | "DP" -> parameters.DP <- x
                | "DQ" -> parameters.DQ <- x
                | "InverseQ" -> parameters.InverseQ <- x
                | "D" -> parameters.D <- x
                | _ -> invalidArg "xmlString" "Invalid XML RSA key."
        else
            invalidArg "xmlString" "Invalid XML RSA key."

        rsa.ImportParameters(parameters)

    member rsa.ToXmlStringForDotNetCore(includePrivateParameters : bool) =
        let parameters = rsa.ExportParameters(includePrivateParameters)


        [
            "Modulus", parameters.Modulus
            "Exponent", parameters.Exponent
        ]
        |> List.toSeq
        |> (includePrivateParameters |> function
            | true ->
                Seq.append
                    [
                        "P", parameters.P
                        "Q", parameters.Q
                        "DP", parameters.DP
                        "DQ", parameters.DQ
                        "InverseQ", parameters.InverseQ
                        "D", parameters.D
                    ]
            | _ -> id    
        )
        |> Seq.map(fun (key, value) ->
            sprintf "<%s>%s</%s>" key (Convert.ToBase64String value) key
        )
        |> String.concat ""
        |>  sprintf "<RSAKeyValue>%s</RSAKeyValue>"

    member inline rsa.PublicKey = rsa.ToXmlStringForDotNetCore(false) |> Encoding.UTF8.GetBytes
    member inline rsa.PrivateKey = rsa.ToXmlStringForDotNetCore(true) |> Encoding.UTF8.GetBytes



type AesCryptoServiceProvider with
    member inline aes.Encrypt(bytes : byte [], ?offset, ?count) =
        let offset = defaultArg offset 0
        let count = defaultArg count bytes.Length
        aes.CreateEncryptor().TransformFinalBlock(bytes, offset, count)

    member inline aes.Decrypt(bytes : byte [], ?offset, ?count) =
        let offset = defaultArg offset 0
        let count = defaultArg count bytes.Length
        aes.CreateDecryptor().TransformFinalBlock(bytes, offset, count)