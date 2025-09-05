(* A simple example of parsing `.debug_info`.
   This example demonstrates how to parse the `.debug_info` section of a
   DWARF object file and iterate over the compilation units and their DIEs.
   It also demonstrates how to find the DWO unit for each CU in a DWP file.
   Most of the complexity is due to loading the sections from the object
   file and DWP file, which is not something that is provided by durin itself.
 *)
