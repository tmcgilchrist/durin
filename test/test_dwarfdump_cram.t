Test dwarfdump.ml basic functionality

Set up the test environment:
  $ cd $TESTCASE_ROOT/..

Test error handling with non-existent file:
  $ dwarfdump --debug-line nonexistent_file.txt
  dwarfdump: FILE argument: no 'nonexistent_file.txt' file or directory
  Usage: dwarfdump [OPTION]… FILE
  Try 'dwarfdump --help' for more information.
  [124]

Test help flag:
  $ dwarfdump --help=plain
  NAME
         dwarfdump - A DWARF debugging information dumper
  
  SYNOPSIS
         dwarfdump [OPTION]… FILE
  
  ARGUMENTS
         FILE (required)
             Binary file to analyze for DWARF debug information
  
  OPTIONS
         -a, --all
             Dump all available debug information
  
         --debug-abbrev
             Dump debug abbreviation information (__debug_abbrev section)
  
         --debug-addr
             Dump debug address information (__debug_addr section)
  
         --debug-aranges
             Dump debug address ranges information (__debug_aranges section)
  
         --debug-info
             Dump debug info information (__debug_info section)
  
         --debug-line
             Dump debug line information (__debug_line section)
  
         --debug-line-str
             Dump debug line string information (__debug_line_str section)
  
         --debug-loclists
             Dump debug location lists information (__debug_loclists section)
  
         --debug-macro
             Dump debug macro information (__debug_macro section)
  
         --debug-names
             Dump debug names information (__debug_names section)
  
         --debug-str
             Dump debug string information (__debug_str section)
  
         --debug-str-offsets
             Dump debug string offsets information (__debug_str_offs section)
  
  COMMON OPTIONS
         --help[=FMT] (default=auto)
             Show this help in format FMT. The value FMT must be one of auto,
             pager, groff or plain. With auto, the format is pager or plain
             whenever the TERM env var is dumb or undefined.
  
  EXIT STATUS
         dwarfdump exits with:
  
         0   on success.
  
         123 on indiscriminate errors reported on standard error.
  
         124 on command line parsing errors.
  
         125 on unexpected internal errors (bugs).
  
