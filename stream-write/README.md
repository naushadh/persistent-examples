# stream-write

Back-end agnostic functionality to demo stream writes in `persistent`.

Specifically, the `library` component itself is back-end agonistic, but each `executable` chooses to demo streaming using single and batched inserts with a specific back-end.

## Benchmarks

### BatchingVsLoop

Comparing performance between performing multiple looped database calls and a batched one. Persistent ~~does not support batched inserts yet~~ supports batching. This sample project was put together to demo performance gains of batching for [#770](https://github.com/yesodweb/persistent/pull/770). Note: Benchmarks done using a local database.

#### Running benchmarks

```bash
$ stack build --pedantic --bench
... # should build and run benchmarks. run command below if it didn't run
$ stream-write/.stack-work/dist/x86_64-osx/Cabal-2.0.1.0/build/batching-vs-loop/batching-vs-loop
... # benchmark results
```

#### PostgreSQL 10

Results
```diff
benchmarking MyRecord/10
- time                 232.4 μs   (201.2 μs .. 281.1 μs)
+ time                 69.22 μs   (62.76 μs .. 80.15 μs)

benchmarking MyRecord/100
- time                 4.187 ms   (3.453 ms .. 6.545 ms)
+ time                 518.7 μs   (435.0 μs .. 654.4 μs)

benchmarking MyRecord/1000
- time                 47.51 ms   (35.15 ms .. NaN s)
+ time                 12.98 ms   (11.22 ms .. 20.11 ms)

benchmarking MyUniqueRecord/10
- time                 311.8 μs   (274.1 μs .. 372.5 μs)
+ time                 42.35 μs   (38.29 μs .. 49.32 μs)

benchmarking MyUniqueRecord/100
- time                 6.699 ms   (5.152 ms .. NaN s)
+ time                 210.2 μs   (184.5 μs .. 251.0 μs)

benchmarking MyUniqueRecord/1000
- time                 66.15 ms   (49.38 ms .. NaN s)
+ time                 3.981 ms   (3.169 ms .. 6.536 ms)
```

Conclusion
- Batching wins without any network latencies.
- Network latencies will only make batching even more appealing, i.e. fewer net round-trips to the database.