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
  ```
  real    0m10.012s
  user    0m11.796s
  sys     0m3.216s
  ```

- change to Models
  ```
  real    0m9.196s
  user    0m11.120s
  sys     0m2.918s
  ```

- prognosis: near full re-compilation for _any_ change.

### component

- no .stack-work
  ```
  real    0m7.715s
  user    0m14.299s
  sys     0m2.292s
  ```

- change to MyRecord1/Models
  ```
  real    0m4.494s
  user    0m5.380s
  sys     0m1.499s
  ```

- change to MyRecord1And2/Models
  ```
  real    0m3.457s
  user    0m3.941s
  sys     0m1.149s
  ```

- change to MyRecord2/Models
  ```
  real    0m4.540s
  user    0m5.496s
  sys     0m1.469s
  ```

- change to MyRecord2/Models
  ```
  real    0m3.604s
  user    0m4.188s
  sys     0m1.172s
  ```

- change to */Models
  ```
  real    0m6.337s
  user    0m12.162s
  sys     0m1.892s
  ```

- prognosis:
  - full re-compile is using concurrent building efficiency
  - component changes only re-compile the component and it's dependencies