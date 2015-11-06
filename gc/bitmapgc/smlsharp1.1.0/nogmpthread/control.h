/*
 * control.h
 * @copyright (c) 2010, Tohoku University.
 * @author UENO Katsuhiro
 */
#ifndef SMLSHARP__CONTROL_H__
#define SMLSHARP__CONTROL_H__

#include "objspace.h"

void sml_control_enum_ptr(void (*callback)(void **), enum sml_gc_mode mode);

#endif /* SMLSHARP__CONTROL_H__ */
