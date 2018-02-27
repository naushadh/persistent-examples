# Model Organization

Comparing various organization styles and compile times.

Notes:
- Benchmarks done on a Retina MacBookPro 13" 2017.
- Compilation done using `stack`, timing done using `time`.
  ```bash
  $ time stack build --pedantic
  ```
- bare/initial compile is simulated by manually purging the project specific `.stack-work` dir.
- "change" done between re-compilation was just a simple whitespace addition. This is intentionally contrived since we are trying to simulate the effects of code change but we're keeping the effective types the same for simpler test set.

## Organization styles

### monolith

```bash
$ tree model-organization/monolith/
model-organization/monolith/
├── Database
│   └── PersistExample
│       ├── Models
│       └── Schema.hs
├── LICENSE
├── README.md
└── model-org-monolith.cabal

2 directories, 5 files
```

The classic persistent dir structure suggested by various guides, scaffolds, templates etc.

Pros:
- single source of truth for all entities
- compact single module for all definitions, no duplicate boiler plate (extensions and imports) for defining entities
- probably the only straight forward way to teach persistent about your entities using `persistent-template` APIs.
- plays well with the simple organization structure of having a central `Types.hs` module.

Cons:
- changes to single entity will require re-compiling the whole entity definition.
- a project structure that puts "types" into a single module more often than not probably incurs a full re-compilation for _any_ change to the "types"/entities module.

### component

```bash
$ tree model-organization/component/
model-organization/component/
├── Database
│   └── PersistExample
│       ├── Migration.hs
│       ├── MyRecord1
│       │   ├── Models
│       │   └── Schema.hs
│       ├── MyRecord1And2
│       │   ├── Models
│       │   └── Schema.hs
│       ├── MyRecord2
│       │   ├── Models
│       │   └── Schema.hs
│       ├── MyRecord3
│       │   ├── Models
│       │   └── Schema.hs
│       ├── Schema.hs
│       └── Util.hs
├── LICENSE
├── README.md
└── model-org-component.cabal

6 directories, 14 files
```

Collect entities into "groups" by relationship.

Sometimes a single source of truth is too much. Some enterprise-y projects may have hundreds of entities, but have clusters of relations inter-related with little/none external relations.

Pros:
- single responsibility principle. each "component" is responsible for one local collection of entities and all business logic for said component. Usage across components is still possible when needed.
- quicker re-compile and bare-compile times by exploiting concurrent building and not centralizing all definitions in one place
- can be retro-fitted into applications that have a single mega entity module by having a re-export module that pieces all component entity definitions back. See `component` dir. This still offers compile time boost as we save the time spent on the compiler re-building all of the persistent TH code.

Cons:
- More boiler plate with each entity component. But this could be addressed with a component "prelude" of sorts.
- `mkMigrate` _requires_ you to have all entities in one place.
  > Creates a single function to perform all migrations for the entities defined here. One thing to be aware of is dependencies: if you have entities with foreign references, make sure to place those definitions after the entities they reference.
  - What isn't obvious is, running multiple component migrations isn't the same as running a single combined component migration (you loose FK integrity).
  - but! _we have the technology_ thanks to the opening up of persistent-template `parseReferences`. all we need to do to address migrations is to have a `persistManyFileWith` function that consumes all of the component definitions (without creating entities).

## Timings

### monolith

- no .stack-work
  ```bash
  $ stack build model-org-monolith --pedantic --ghc-options -O2 +RTS -N4
  model-org-monolith-0.1.0.0: unregistering (local file changes: Database/PersistExample/Schema.hs README.md model-org-monolith.cabal)
  model-org-monolith-0.1.0.0: configure (lib)
  Configuring model-org-monolith-0.1.0.0...
  model-org-monolith-0.1.0.0: build (lib)
  Preprocessing library for model-org-monolith-0.1.0.0..
  Building library for model-org-monolith-0.1.0.0..
  [1 of 1] Compiling Database.PersistExample.Schema ( Database/PersistExample/Schema.hs, .stack-work/dist/x86_64-osx/Cabal-2.0.1.0/build/Database/PersistExample/Schema.o )
  model-org-monolith-0.1.0.0: copy/register
  Installing library in .stack-work/install/x86_64-osx/lts-10.4/8.2.2/lib/x86_64-osx-ghc-8.2.2/model-org-monolith-0.1.0.0-4J9AfazLsAoFuNrx7FJKH8
  Registering library for model-org-monolith-0.1.0.0..
      424,572,704 bytes allocated in the heap
      597,573,584 bytes copied during GC
        69,995,296 bytes maximum residency (14 sample(s))
        3,337,440 bytes maximum slop
              167 MB total memory in use (0 MB lost due to fragmentation)

                                      Tot time (elapsed)  Avg pause  Max pause
    Gen  0       742 colls,   742 par    0.465s   0.162s     0.0002s    0.0009s
    Gen  1        14 colls,    13 par    0.509s   0.219s     0.0157s    0.0418s

    Parallel GC work balance: 35.82% (serial 0%, perfect 100%)

    TASKS: 16 (1 bound, 15 peak workers (15 total), using -N4)

    SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

    INIT    time    0.000s  (  0.002s elapsed)
    MUT     time    0.158s  ( 29.959s elapsed)
    GC      time    0.973s  (  0.381s elapsed)
    EXIT    time    0.001s  (  0.012s elapsed)
    Total   time    1.140s  ( 30.355s elapsed)

    Alloc rate    2,692,059,018 bytes per MUT second

    Productivity  14.6% of total user, 98.7% of total elapsed

  gc_alloc_block_sync: 47012
  whitehole_spin: 0
  gen[0].sync: 4
  gen[1].sync: 23611
  ```

- change to Models
  ```bash
  $ stack build model-org-monolith --pedantic --ghc-options -O2 +RTS -N4
  model-org-monolith-0.1.0.0: unregistering (local file changes: Database/PersistExample/Models)
  model-org-monolith-0.1.0.0: build (lib)
  Preprocessing library for model-org-monolith-0.1.0.0..
  Building library for model-org-monolith-0.1.0.0..
  [1 of 1] Compiling Database.PersistExample.Schema ( Database/PersistExample/Schema.hs, .stack-work/dist/x86_64-osx/Cabal-2.0.1.0/build/Database/PersistExample/Schema.o ) [Database/PersistExample/Models changed]
  model-org-monolith-0.1.0.0: copy/register
  Installing library in .stack-work/install/x86_64-osx/lts-10.4/8.2.2/lib/x86_64-osx-ghc-8.2.2/model-org-monolith-0.1.0.0-4J9AfazLsAoFuNrx7FJKH8
  Registering library for model-org-monolith-0.1.0.0..
      435,217,736 bytes allocated in the heap
      343,965,232 bytes copied during GC
        69,993,824 bytes maximum residency (10 sample(s))
        4,690,664 bytes maximum slop
              175 MB total memory in use (0 MB lost due to fragmentation)

                                      Tot time (elapsed)  Avg pause  Max pause
    Gen  0       759 colls,   759 par    0.483s   0.161s     0.0002s    0.0008s
    Gen  1        10 colls,     9 par    0.260s   0.133s     0.0133s    0.0524s

    Parallel GC work balance: 21.33% (serial 0%, perfect 100%)

    TASKS: 16 (1 bound, 15 peak workers (15 total), using -N4)

    SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

    INIT    time    0.000s  (  0.002s elapsed)
    MUT     time    0.154s  ( 28.635s elapsed)
    GC      time    0.743s  (  0.294s elapsed)
    EXIT    time    0.001s  (  0.014s elapsed)
    Total   time    0.907s  ( 28.944s elapsed)

    Alloc rate    2,831,347,411 bytes per MUT second

    Productivity  18.0% of total user, 99.0% of total elapsed

  gc_alloc_block_sync: 39563
  whitehole_spin: 0
  gen[0].sync: 2
  gen[1].sync: 8943

  ```

- prognosis: near full re-compilation for _any_ change.

### component

- no .stack-work
  ```
  $ stack build model-org-component --pedantic --ghc-options -O2 +RTS -N4
  model-org-component-0.1.0.0: unregistering (local file changes: Database/PersistExample/Migration.hs Database/PersistExample/MyRecord1/Schema.hs Database/Persist...)
  model-org-component-0.1.0.0: configure (lib)
  Configuring model-org-component-0.1.0.0...
  model-org-component-0.1.0.0: build (lib)
  Preprocessing library for model-org-component-0.1.0.0..
  Building library for model-org-component-0.1.0.0..
  [ 1 of 24] Compiling Database.PersistExample.MyRecord1.Schema ( Database/PersistExample/MyRecord1/Schema.hs, .stack-work/dist/x86_64-osx/Cabal-2.0.1.0/build/Database/PersistExample/MyRecord1/Schema.o )
  [ 2 of 24] Compiling Database.PersistExample.MyRecord10.Schema ( Database/PersistExample/MyRecord10/Schema.hs, .stack-work/dist/x86_64-osx/Cabal-2.0.1.0/build/Database/PersistExample/MyRecord10/Schema.o )
  [ 3 of 24] Compiling Database.PersistExample.MyRecord11.Schema ( Database/PersistExample/MyRecord11/Schema.hs, .stack-work/dist/x86_64-osx/Cabal-2.0.1.0/build/Database/PersistExample/MyRecord11/Schema.o )
  [ 4 of 24] Compiling Database.PersistExample.MyRecord12.Schema ( Database/PersistExample/MyRecord12/Schema.hs, .stack-work/dist/x86_64-osx/Cabal-2.0.1.0/build/Database/PersistExample/MyRecord12/Schema.o )
  [ 5 of 24] Compiling Database.PersistExample.MyRecord13.Schema ( Database/PersistExample/MyRecord13/Schema.hs, .stack-work/dist/x86_64-osx/Cabal-2.0.1.0/build/Database/PersistExample/MyRecord13/Schema.o )
  [ 6 of 24] Compiling Database.PersistExample.MyRecord14.Schema ( Database/PersistExample/MyRecord14/Schema.hs, .stack-work/dist/x86_64-osx/Cabal-2.0.1.0/build/Database/PersistExample/MyRecord14/Schema.o )
  [ 7 of 24] Compiling Database.PersistExample.MyRecord15.Schema ( Database/PersistExample/MyRecord15/Schema.hs, .stack-work/dist/x86_64-osx/Cabal-2.0.1.0/build/Database/PersistExample/MyRecord15/Schema.o )
  [ 8 of 24] Compiling Database.PersistExample.MyRecord16.Schema ( Database/PersistExample/MyRecord16/Schema.hs, .stack-work/dist/x86_64-osx/Cabal-2.0.1.0/build/Database/PersistExample/MyRecord16/Schema.o )
  [ 9 of 24] Compiling Database.PersistExample.MyRecord17.Schema ( Database/PersistExample/MyRecord17/Schema.hs, .stack-work/dist/x86_64-osx/Cabal-2.0.1.0/build/Database/PersistExample/MyRecord17/Schema.o )
  [10 of 24] Compiling Database.PersistExample.MyRecord18.Schema ( Database/PersistExample/MyRecord18/Schema.hs, .stack-work/dist/x86_64-osx/Cabal-2.0.1.0/build/Database/PersistExample/MyRecord18/Schema.o )
  [11 of 24] Compiling Database.PersistExample.MyRecord19.Schema ( Database/PersistExample/MyRecord19/Schema.hs, .stack-work/dist/x86_64-osx/Cabal-2.0.1.0/build/Database/PersistExample/MyRecord19/Schema.o )
  [12 of 24] Compiling Database.PersistExample.MyRecord2.Schema ( Database/PersistExample/MyRecord2/Schema.hs, .stack-work/dist/x86_64-osx/Cabal-2.0.1.0/build/Database/PersistExample/MyRecord2/Schema.o )
  [13 of 24] Compiling Database.PersistExample.MyRecord1And2.Schema ( Database/PersistExample/MyRecord1And2/Schema.hs, .stack-work/dist/x86_64-osx/Cabal-2.0.1.0/build/Database/PersistExample/MyRecord1And2/Schema.o )
  [14 of 24] Compiling Database.PersistExample.MyRecord20.Schema ( Database/PersistExample/MyRecord20/Schema.hs, .stack-work/dist/x86_64-osx/Cabal-2.0.1.0/build/Database/PersistExample/MyRecord20/Schema.o )
  [15 of 24] Compiling Database.PersistExample.MyRecord3.Schema ( Database/PersistExample/MyRecord3/Schema.hs, .stack-work/dist/x86_64-osx/Cabal-2.0.1.0/build/Database/PersistExample/MyRecord3/Schema.o )
  [16 of 24] Compiling Database.PersistExample.MyRecord4.Schema ( Database/PersistExample/MyRecord4/Schema.hs, .stack-work/dist/x86_64-osx/Cabal-2.0.1.0/build/Database/PersistExample/MyRecord4/Schema.o )
  [17 of 24] Compiling Database.PersistExample.MyRecord5.Schema ( Database/PersistExample/MyRecord5/Schema.hs, .stack-work/dist/x86_64-osx/Cabal-2.0.1.0/build/Database/PersistExample/MyRecord5/Schema.o )
  [18 of 24] Compiling Database.PersistExample.MyRecord6.Schema ( Database/PersistExample/MyRecord6/Schema.hs, .stack-work/dist/x86_64-osx/Cabal-2.0.1.0/build/Database/PersistExample/MyRecord6/Schema.o )
  [19 of 24] Compiling Database.PersistExample.MyRecord7.Schema ( Database/PersistExample/MyRecord7/Schema.hs, .stack-work/dist/x86_64-osx/Cabal-2.0.1.0/build/Database/PersistExample/MyRecord7/Schema.o )
  [20 of 24] Compiling Database.PersistExample.MyRecord8.Schema ( Database/PersistExample/MyRecord8/Schema.hs, .stack-work/dist/x86_64-osx/Cabal-2.0.1.0/build/Database/PersistExample/MyRecord8/Schema.o )
  [21 of 24] Compiling Database.PersistExample.MyRecord9.Schema ( Database/PersistExample/MyRecord9/Schema.hs, .stack-work/dist/x86_64-osx/Cabal-2.0.1.0/build/Database/PersistExample/MyRecord9/Schema.o )
  [22 of 24] Compiling Database.PersistExample.Util ( Database/PersistExample/Util.hs, .stack-work/dist/x86_64-osx/Cabal-2.0.1.0/build/Database/PersistExample/Util.o )
  [23 of 24] Compiling Database.PersistExample.Migration ( Database/PersistExample/Migration.hs, .stack-work/dist/x86_64-osx/Cabal-2.0.1.0/build/Database/PersistExample/Migration.o )
  [24 of 24] Compiling Database.PersistExample.Schema ( Database/PersistExample/Schema.hs, .stack-work/dist/x86_64-osx/Cabal-2.0.1.0/build/Database/PersistExample/Schema.o )
  model-org-component-0.1.0.0: copy/register
  Installing library in .stack-work/install/x86_64-osx/lts-10.4/8.2.2/lib/x86_64-osx-ghc-8.2.2/model-org-component-0.1.0.0-A6IxkrMbNiXCnaZwUhbNxM
  Registering library for model-org-component-0.1.0.0..
      442,072,424 bytes allocated in the heap
      915,672,008 bytes copied during GC
        69,876,824 bytes maximum residency (19 sample(s))
        1,902,720 bytes maximum slop
              157 MB total memory in use (0 MB lost due to fragmentation)

                                      Tot time (elapsed)  Avg pause  Max pause
    Gen  0       771 colls,   771 par    0.455s   0.155s     0.0002s    0.0011s
    Gen  1        19 colls,    18 par    0.914s   0.392s     0.0206s    0.0587s

    Parallel GC work balance: 45.35% (serial 0%, perfect 100%)

    TASKS: 17 (1 bound, 16 peak workers (16 total), using -N4)

    SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

    INIT    time    0.000s  (  0.002s elapsed)
    MUT     time    0.172s  ( 19.700s elapsed)
    GC      time    1.369s  (  0.546s elapsed)
    EXIT    time    0.002s  (  0.013s elapsed)
    Total   time    1.551s  ( 20.260s elapsed)

    Alloc rate    2,571,279,810 bytes per MUT second

    Productivity  11.7% of total user, 97.3% of total elapsed

  gc_alloc_block_sync: 83000
  whitehole_spin: 0
  gen[0].sync: 2
  gen[1].sync: 50498
  ```

- change to MyRecordN/Models
  ```
  $ stack build model-org-component --pedantic --ghc-options -O2 +RTS -N4
  model-org-component-0.1.0.0: unregistering (local file changes: Database/PersistExample/MyRecord20/Models)
  model-org-component-0.1.0.0: build (lib)
  Preprocessing library for model-org-component-0.1.0.0..
  Building library for model-org-component-0.1.0.0..
  [14 of 24] Compiling Database.PersistExample.MyRecord20.Schema ( Database/PersistExample/MyRecord20/Schema.hs, .stack-work/dist/x86_64-osx/Cabal-2.0.1.0/build/Database/PersistExample/MyRecord20/Schema.o ) [Database/PersistExample/MyRecord20/Models changed]
  model-org-component-0.1.0.0: copy/register
  Installing library in .stack-work/install/x86_64-osx/lts-10.4/8.2.2/lib/x86_64-osx-ghc-8.2.2/model-org-component-0.1.0.0-A6IxkrMbNiXCnaZwUhbNxM
  Registering library for model-org-component-0.1.0.0..
      471,110,672 bytes allocated in the heap
      476,087,040 bytes copied during GC
        69,958,192 bytes maximum residency (12 sample(s))
        4,685,760 bytes maximum slop
              175 MB total memory in use (0 MB lost due to fragmentation)

                                      Tot time (elapsed)  Avg pause  Max pause
    Gen  0       822 colls,   822 par    0.496s   0.170s     0.0002s    0.0012s
    Gen  1        12 colls,    11 par    0.376s   0.170s     0.0142s    0.0380s

    Parallel GC work balance: 28.71% (serial 0%, perfect 100%)

    TASKS: 16 (1 bound, 15 peak workers (15 total), using -N4)

    SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

    INIT    time    0.000s  (  0.002s elapsed)
    MUT     time    0.173s  (  3.946s elapsed)
    GC      time    0.872s  (  0.340s elapsed)
    EXIT    time    0.002s  (  0.013s elapsed)
    Total   time    1.055s  (  4.300s elapsed)

    Alloc rate    2,724,647,770 bytes per MUT second

    Productivity  17.3% of total user, 92.1% of total elapsed

  gc_alloc_block_sync: 39037
  whitehole_spin: 0
  gen[0].sync: 1
  gen[1].sync: 19078

  ```

- prognosis:
  - full re-compile is using concurrent building efficiency
  - component changes only re-compile the component and it's dependencies