# DWARF 5 Terminology


## Compile Unit vs DIEs
In DWARF 5 terminology, a Compile Unit and a DIE (Debugging Information Entry) are related but distinct concepts:

###  Compile Unit

  - Definition: A top-level organizational unit that represents all the debugging information for a single compilation unit
   (typically one source file and its included headers)
  - Structure: Contains a header with metadata and a tree of DIEs
  - Header includes:
    - unit_length: Size of the compilation unit data
    - version: DWARF version (5 in this case)
    - unit_type: Type of unit (e.g., DW_UT_compile for regular compilation)
    - address_size: Size of addresses on target machine
    - debug_abbrev_offset: Offset into abbreviation table

###  DIE (Debugging Information Entry)

  - Definition: Individual entries within a compile unit that describe specific program entities
  - Examples: Variables, functions, types, lexical blocks, etc.
  - Structure: Each DIE has:
    - An abbreviation code (references the abbreviation table)
    - A set of attributes with values
    - Optional children DIEs (forming a tree structure)

### Relationship

```

  Compile Unit
  ├── Header (metadata)
  └── DIE Tree
      ├── Root DIE (DW_TAG_compile_unit)
      │   ├── Child DIE (e.g., DW_TAG_subprogram for a function)
      │   │   └── Child DIE (e.g., DW_TAG_variable for a local variable)
      │   └── Child DIE (e.g., DW_TAG_base_type for int)
      └── ...

```
  Key distinction: A Compile Unit is the container that holds the header and organizational structure, while DIEs are the content that describe the actual debugging information about your program's entities. The compile unit header tells you how to parse the DIEs, and the DIEs tell you about your program's structure.

  In our dwarfdump implementation, we parse the Compile Unit header first to understand the format, then traverse the DIE tree to extract debugging information about the program.