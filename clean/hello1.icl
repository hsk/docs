module hello1

import StdEnv
// test
/* comment */
Start :: *World -> *World
Start world
	# (console,world) = stdio world
	  console          = fwrites "Hello World\n" console
	  (ok,world)       = fclose console world
	| not ok           = abort "Cannot close console"
	= world
