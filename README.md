# Spacialist Plugin

Detects and instruments overloaded function applications during compilation. At
runtime, the instrumentation will emit information about the overloaded calls
that get evaluated.

## Usage

Add this plugin to `build-depends` and use `-fplugin=GHC.Specialist` on whatever
modules you wish to instrument. To set a dynamic sample rate for plugin output,
use `-fplugin-opt=GHC.Specialist:f:N` where `N` is the decimal probability of
output being emitted anytime instrumentation is executed. To set the plugin
verbosity use `-fplugin-opt=GHC.Specialist:v` for verbose and
`-fplugin-opt=GHC.Specialist:vv` for more verbose.

## Tests

The test suite includes some basic unit tests. Each unit test consists of an
instrumented executable (see the `test-TX` executables in the cabal file). These
are included as `build-tool-depends` for the test suite, which executes the
executables and retrieves the specialist notes from the event log output.

Various conditions are then checked for each of the tests to ensure that the
overloaded calls are what we expect and that the data included in the notes is
accurate.

The test suite could be extended to check the function of the `specialyze` tool
more comprehensively or test the compile-time behavior of the plugin. At the
moment, it really only tests the generation and retrieval of the plugin output.
