# stream

Back-end agnostic functionality to demo stream processing in `persistent`.

Most of the code here has been lifted from [kubkon/conduit-persistent-example](https://github.com/kubkon/conduit-persistent-example). [See blog post here](http://www.jakubkonka.com/2014/01/23/conduit-haskell.html).

Specifically, the `library` component itself is back-end agonistic, but each `executable`
chooses to demo streaming using a specific sql backend.

### Executable projects/apps

- `doList`: perform a naive full memory load of a 1.1M row table and perform some tasks.
- `doStream`: perform the same task as `doList` instead using conduit streaming.

### bench-results

Date/back-end wise profiling results for each of the above executables.
Steps to (re)create results:

  - Build with profiling enabled
    ```bash
    $ stack build --profile
    ```

  - Execute with profiling enabled. (`$target` is one of executables from the previous section)
    ```
    $ stack exec -- $target +RTS -s -p
    $ stack exec -- $target +RTS -s -p
    ```

  - Manually move the resulting `$target.prof` file into `bench-results/<today>/`

  - Manually create a `$target.profS` file to paste in the run time statistics.

### bench-results findings

#### `persistent-mysql-haskell-0.3.5`

This version of `persistent-mysql-haskell` adds streaming support. i.e., `selectSource` streams data instead of loading it all into memory. Note: there were 1M+ rows in the table being used.

doStream

- 212x lower memory usage
  ```diff
  -   15,183,787,896 bytes allocated in the heap
  -    1,614,074,640 bytes copied during GC
  -      272,859,448 bytes maximum residency (13 sample(s))
  -       11,361,024 bytes maximum slop
  -              636 MB total memory in use (0 MB lost due to fragmentation)
  ---
  +   18,246,210,072 bytes allocated in the heap
  +       47,275,128 bytes copied during GC
  +          120,528 bytes maximum residency (29 sample(s))
  +          208,232 bytes maximum slop
  +                3 MB total memory in use (0 MB lost due to fragmentation)
  ```

- about 50% slower runtime though
  ```diff
  -   Total   time   17.402s  ( 27.573s elapsed)
  +   Total   time   31.654s  ( 47.127s elapsed)
  ```

doList

- 1.5x lower memory usage
  ```diff
  -   12,455,765,888 bytes allocated in the heap
  -    2,090,260,056 bytes copied during GC
  -      272,857,512 bytes maximum residency (13 sample(s))
  -       13,175,736 bytes maximum slop
  -              780 MB total memory in use (0 MB lost due to fragmentation)
  +  14,347,766,480 bytes allocated in the heap
  +    1,243,056,656 bytes copied during GC
  +      207,739,880 bytes maximum residency (12 sample(s))
  +       12,013,736 bytes maximum slop
  +              525 MB total memory in use (0 MB lost due to fragmentation)
  ```

- about 10% slower runtime
  ```diff
  -   Total   time    8.138s  ( 15.229s elapsed)
  +   Total   time    9.131s  ( 15.429s elapsed)
  ```

It appears that for an exponential gain in memory efficiency there is some small linear increase of runtime.