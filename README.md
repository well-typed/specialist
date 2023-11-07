# Spacialist Plugin

Detects and instruments overloaded function applications compilation. At
runtime, the instrumentation will emit information about the overloaded calls
that get evaluated.

## Usage

Add this plugin to `build-depends` and use `-fplugin=GHC.Specialist` on whatever
modules you wish to instrument. To set a dynamic sampling rate for plugin
output, use `-fplugin-opt=GHC.Specialist:f:N` where `N` is the decimal
probability of output being emitted anytime instrumentation is executed.
