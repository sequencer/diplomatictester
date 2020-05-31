# Diplomatictester
This is a chisel plugin specifically focused on test generation of diplomacy-based design.
For integration test, you can replace arbitrary chisel hardware module with VIP implemented with Scala. 
You can generate specific TileLink sequence based on a `BaseNode` of an `LazyModule`.
You can peek/poke `AutoBundle` for `LazyModule`. 

## Unit Test
`diplomatictester.dutIO` can instantiate a top module only with a dut which you marked with `dutIO`.
  
## Integration Test
`diplomatictester.mockIO` can replace arbitrary chisel hardware module with VIP implemented with Scala.
