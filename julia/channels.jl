const N = 100000
const NUM_TASKS = N / 10
const JOBS = Channel{Int}(NUM_TASKS);
const RESULTS = Channel{Tuple{Int, Float32}}(NUM_TASKS);

function start()
    function make_jobs(n)
        for i in 1:n
            put!(JOBS, i)
        end
    end;

    function do_work()
        for job_id in JOBS
            exec_time = rand()
            # simulates elapsed time doing actual work
            sleep(exec_time)
            # typically performed externally.
            put!(RESULTS, (job_id, exec_time))
        end
    end;

    # feed the JOBS channel with "n" JOBS
    @async make_jobs(N);

    # start m tasks to process requests in parallel
    for i in 1:NUM_TASKS
        @async do_work()
    end
end

function print_results()
    @elapsed for _ in 1:N
        job_id, exec_time = take!(RESULTS)
        println("$job_id finished in $(round(exec_time; digits=2)) seconds")
    end
end

start()
print_results()
