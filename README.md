# Spacialist Plugin

Detects and instruments overloaded function applications during compilation. At
runtime, the instrumentation will emit information about the overloaded calls
that get evaluated.

## Usage

Add this plugin to `build-depends` and use `-fplugin=GHC.Specialist` on whatever
modules you wish to instrument. To set a dynamic sample rate for plugin output,
use `-fplugin-opt=GHC.Specialist:f:N` where `N` is the decimal probability of
output being emitted anytime instrumentation is executed.

## Tests

Currently, the test suite is implemented as golden tests. This could be better,
since we could easily get our hands on the event log output of the test
executables and work directly with the `SpecialistNotes` held in the event log.
