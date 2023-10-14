# Spacialist Plugin

Detects and instruments overloaded function applications compilation. At
runtime, the instrumentation will emit information about the overloaded calls
that get evaluated.

## Usage

> Any suggestions to improve the following workflow would be welcome.

### Installation

The `Makefile` includes some rules for common workflows. To install the plugin
with the default configuration, just `make install`. To install the plugin with
late cost center profiling, `make prof-install`.

To uninstall, `make uninstall`. Unfortunately, the only reliable way I have
found to properly uninstall the plugin is to delete the whole cabal store for
the current GHC version.

To enable the plugin on a program, use the following GHC options:

```
    -package-db=CABAL_STORE/ghc-GHC_VERSION/package.db
    -plugin-package=specialist
    -package=specialist
    -fplugin=GHC.Specialist
```

Where `CABAL_STORE` is the path to your cabal store, usually
`$HOME/.cabal/store`, and `GHC_VERSION` is the output of `ghc
--numeric-version`. Those options can be given directly to `program-options` to
enable the plugin on local packages like so (on cabal versions >= 3.8):

```cabal
program-options
  ghc-options:
    -package-db=CABAL_STORE/ghc-GHC_VERSION/package.db
    -plugin-package=specialist
    -package=specialist
    -fplugin=GHC.Specialist
```

Note that we need to give `-package=specialist` because the plugin injects code
into the AST.

You could also enable the plugin on upstream dependencies by specifying those
flags under a `package *` stanza.

It is recommended to enable late cost centre profiling and the info table map to
ensure the plugin output is as detailed as possible. The info table map should
be enabled on upstream dependencies, to make source locations for dictionaries
more available. Cost centre profiling should be enabled on any package the
plugin is enabled for.

The precise workflow that I have found works best for me is:

1. Remove all of cabal store:
```
make uninstall
```

2. Install the plugin as desired
```
make install
```

3. Build the package with the plugin enabled using something like:
```
program-options
  ghc-options:
    -package-db=CABAL_STORE/ghc-GHC_VERSION/package.db
    -plugin-package=specialist
    -package=specialist
    -fplugin=GHC.Specialist
```