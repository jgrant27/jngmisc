__precompile__()

module ngrams

using TextAnalysis

ngramsForFile(textfile, cnt) =
    open(textfile) do f
        @info("Loading file $(textfile) ...")
        sd = StringDocument(read(f, String))
        @info("Removing case ...")
        remove_case!(sd)
        @info("Removing punctuation ...")
        prepare!(sd, strip_punctuation)
        @info("Counting $(cnt)-grams ...")
        ngd = NGramDocument(sd.text, cnt)
        @info("Sorting results ...")
        sort(collect(ngd.ngrams), by = tuple -> last(tuple), rev=true)
    end

end
