using ngrams, Test

const PROJECT_ROOT = pkgdir(ngrams)

@time trigrams = ngrams.ngramsForFile("$(PROJECT_ROOT)/text/100-0.txt", 3)
@testset "Basic Tri-Grams" begin
    @test length(trigrams) == 798456
    @test trigrams[1].first == "i pray you"
    @test trigrams[1].second == 251
    @test trigrams[2].first == "i will not"
    @test trigrams[2].second == 228
    @test trigrams[3].first == "i know not"
    @test trigrams[3].second == 171
end
