# Diplomatictester
This is a Chisel library specifically focused on test generation for diplomacy-based designs.
For integration tests, you can replace arbitrary Chisel hardware modules with a verification IP (VIP) implemented in Scala.
You can generate specific TileLink sequences based on the `BaseNode` of a `LazyModule`.
You can also peek/poke `AutoBundle`s for `LazyModule`s.

## Requirements
Requires Chisel 3.3+ and associated dependencies.

## Unit Tests
`diplomatictester.dutIO` can instantiate a top module only with a DUT explicitly marked with `dutIO`.

## Integration Tests
`diplomatictester.mockIO` can replace an arbitrary Chisel hardware module with a VIP implemented in Scala.
