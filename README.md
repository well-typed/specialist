# Spacialist Plugin

Detects and instruments overloaded function applications compilation. At
runtime, the instrumentation will emit information about the overloaded calls
that get evaluated.

## Usage

> Any suggestions to improve the following workflow would be welcome.

Build the plugin:

```
cabal build
```

Register the package (only need to do this once):

```
ghc-pkg register dist-newstyle/packagedb/ghc-$(ghc --numeric-version)/specialist-*-inplace.conf
```

Compile, e.g., the `T1` example with the plugin enabled:

```
ghc test/T1.hs -finfo-table-map -fdistinct-constructor-tables -package specialist -fplugin=GHC.Specialist
```

Run the resulting `T1` executable and check the `test/specialist-notes.txt`
file to see what the instrumentation output.

To make the plugin will print information during compilation, pass `v` as a
plugin option like so:

```
ghc test/T1.hs -finfo-table-map -fdistinct-constructor-tables -package specialist -fplugin=GHC.Specialist -fplugin-opt=GHC.Specialist:v
```
