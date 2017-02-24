

let showWordCount text =

    let splitAtSpaces (text: string) =  
        text.Split ' '
        |> List.ofArray
    
    let wordCount text =
        let words = splitAtSpaces text
        let wc = words.Length
        let distinctWords = List.distinct words
        let dups = wc - distinctWords.Length
        (wc, dups)

    let showResults (wc, dups) =
        printfn "--> %d words in the text" wc
        printfn "--> %d duplicate words" dups

    text
    |> wordCount
    |> showResults

showWordCount "a aa c cc cc aa dd aa cc"
