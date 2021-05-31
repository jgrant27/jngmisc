using news, Test

const PROJECT_ROOT = pkgdir(news)
NUM_RUNS = 10

@testset "Using only Tasks" begin
    avg = sum([@elapsed news.tasks() for _ in 1:NUM_RUNS]) / NUM_RUNS
    @test avg > 0
end

@testset "Using Tasks / Channels" begin
    avg = sum([@elapsed news.channels() for _ in 1:NUM_RUNS]) / NUM_RUNS
    @test avg > 0
end
