let MD5HashBeginsWitZeros len (input : string) =
    (use md5 = System.Security.Cryptography.MD5.Create()
    input
    |> System.Text.Encoding.ASCII.GetBytes
    |> md5.ComputeHash
    |> Seq.take len
    |> Seq.map (fun c -> c.ToString("X2"))
    |> Seq.reduce (+)).Substring(0, len) = "".PadRight(len, '0')

MD5HashBeginsWitZeros 5 "abcdef609043"
MD5HashBeginsWitZeros 5 "pqrstuv1048970"

Seq.initInfinite (id)
|> Seq.map (string)
|> Seq.map (fun n -> "yzbqklnj" + n)
|> Seq.find (fun s -> MD5HashBeginsWitZeros 5 s)

MD5HashBeginsWitZeros 5 "yzbqklnj282749"

Seq.initInfinite (id)
|> Seq.map (string)
|> Seq.map (fun n -> "yzbqklnj" + n)
|> Seq.find (fun s -> MD5HashBeginsWitZeros 6 s)

MD5HashBeginsWitZeros 6 "yzbqklnj9962624"
