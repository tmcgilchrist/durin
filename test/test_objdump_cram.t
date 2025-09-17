Test objdump.ml basic functionality

Set up the test environment:
  $ cd $TESTCASE_ROOT/..

Test error handling with non-existent file:
  $ objdump --unwind-info nonexistent_file.txt
  objdump: FILE argument: no 'nonexistent_file.txt' file or directory
  Usage: objdump [--unwind-info] [OPTION]… FILE
  Try 'objdump --help' for more information.
  [124]

Test help flag:
  $ objdump --help=plain
  NAME
         objdump - Display information from object files
  
  SYNOPSIS
         objdump [--unwind-info] [OPTION]… FILE
  
  ARGUMENTS
         FILE (required)
             Binary file to analyze for object information
  
  OPTIONS
         -u, --unwind-info
             Display unwind information
  
  COMMON OPTIONS
         --help[=FMT] (default=auto)
             Show this help in format FMT. The value FMT must be one of auto,
             pager, groff or plain. With auto, the format is pager or plain
             whenever the TERM env var is dumb or undefined.
  
  EXIT STATUS
         objdump exits with:
  
         0   on success.
  
         123 on indiscriminate errors reported on standard error.
  
         124 on command line parsing errors.
  
         125 on unexpected internal errors (bugs).
  


Test no option specified with non-existent file:
  $ objdump hello_world
  objdump: FILE argument: no 'hello_world' file or directory
  Usage: objdump [--unwind-info] [OPTION]… FILE
  Try 'objdump --help' for more information.
  [124]
