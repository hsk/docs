/**
 * init.c
 * @copyright (c) 2007-2009, Tohoku University.
 * @author UENO Katsuhiro
 * @version $Id: $
 */

#include <stdlib.h>
#include "smlsharp.h"
#include "objspace.h"
#include "control.h"

void
sml_finish()
{
	sml_objspace_free();
}
