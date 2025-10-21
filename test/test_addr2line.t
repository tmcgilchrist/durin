Test addr2line basic functionality

Test with function names (-f flag):
  $ addr2line -e hello_world.dSYM/Contents/Resources/DWARF/hello_world -f 0x100000478
  main
  /Users/tsmc/code/ocaml/durin/_build/default/test/hello_world.c:3

Test without function names:
  $ addr2line -e hello_world.dSYM/Contents/Resources/DWARF/hello_world 0x100000478
  /Users/tsmc/code/ocaml/durin/_build/default/test/hello_world.c:3

Test multiple addresses:
  $ addr2line -e hello_world.dSYM/Contents/Resources/DWARF/hello_world -f 0x100000478 0x100000488
  main
  /Users/tsmc/code/ocaml/durin/_build/default/test/hello_world.c:3
  main
  /Users/tsmc/code/ocaml/durin/_build/default/test/hello_world.c:4

Test invalid address (should return ??:0):
  $ addr2line -e hello_world.dSYM/Contents/Resources/DWARF/hello_world 0xFFFFFFFF
  ??:0
