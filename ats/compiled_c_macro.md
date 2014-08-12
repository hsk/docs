# Index of ATS0.1.0 compiled results macros 

ATS0.1.0で以下のコードをコンパイルしました。

		implement main0() = {}

この結果のヘッダファイルを見やすく書き出しました。

# [pats_ccomp_config.h](https://github.com/githwxi/ATS-Postiats/blob/master/ccomp/runtime/pats_ccomp_config.h)

empty


# [pats_ccomp_basics.h](https://github.com/githwxi/ATS-Postiats/blob/master/ccomp/runtime/pats_ccomp_basics.h)

## primitives

bool_true bool_false ptr_null extval(name)

* atsbool_true

		1
* atsbool_false

		0
* atsptr_null

		((void*)0)

* ATSextval(name)

		name

## inline extern static

inline() extern() static()

* ATSinline()

		static inline

* ATSextern()

		extern
* ATSstatic()

		static

## dyn

	load0(flag) load1(flag) loadset(flag) loadfcall(dynloadfun)

* ATSdynload0(flag)

		int flag = 0
* ATSdynload1(flag)

		ATSextern() int flag
* ATSdynloadset(flag)

		flag = 1
* ATSdynloadfcall(dynloadfun)

		dynloadfun()

## dynexn

		#ifndef _ATS_CCOMP_EXCEPTION_NONE

dec(d2c) initize(d2c, exnmsg) extdec(d2c)

* ATSdynexn_dec(d2c)

		atstype_exncon d2c = { 0, "ats-exncon-name" }
* ATSdynexn_initize(d2c, exnmsg)

		the_atsexncon_initize(&(d2c), exnmsg)
* ATSdynexn_extdec(d2c)

		ATSextern() atstype_exncon d2c


		#endif // end of [_ATS_CCOMP_EXCEPTION_NONE]

## assume

assume(flag)

* ATSassume(flag)

		void *flag = (void*)0

## dyncst

mac(d2c) castfn(d2c)

extfun(d2c, targs, tres)

stafun(d2c, targs, tres)

* ATSdyncst_mac(d2c)

		　

* ATSdyncst_castfn(d2c)

		　

* ATSdyncst_extfun(d2c, targs, tres)

		ATSextern() tres d2c targs

* ATSdyncst_stafun(d2c, targs, tres)

		ATSstatic() tres d2c targs

## dyncst_val

imp(d2c, type) dec(d2c, type) bind(d2c, pmv)

* ATSdyncst_valimp(d2c, type)

		type d2c
* ATSdyncst_valdec(d2c, type)

		ATSextern() type d2c
* ATSdyncst_valbind(d2c, pmv)

		d2c = (pmv)

## mainats

void_0(err) argc_argv_0(argc, argv, err) argc_argv_envp_0(argc, argv, envp, err)

void_int(err) argc_argv_int(argc, argv, err) argc_argv_envp_int(argc, argv, envp, err)

* ATSmainats_void_0(err)

		mainats_void_0()
* ATSmainats_argc_argv_0(argc, argv, err)

		mainats_argc_argv_0(argc, argv)
* ATSmainats_argc_argv_envp_0(argc, argv, envp, err)

		mainats_argc_argv_envp_0(argc, argv, envp)
* ATSmainats_void_int(err)

		err = mainats_void_int()
* ATSmainats_argc_argv_int(argc, argv, err)

		err = mainats_argc_argv_int(argc, argv)
* ATSmainats_argc_argv_envp_int(argc, argv, envp, err)

		err = mainats_argc_argv_envp_int(argc, argv, envp)

## error functions

* extern void atsruntime_raise (void *exn) ;
* extern void atsruntime_handle_uncaughtexn (void *exn0) ;
* extern void atsruntime_handle_unmatchedval (char *msg0) ;
* extern void atsruntime_handle_unmatchedarg (char *msg0) ;

# [pats_ccomp_typedefs.h](https://github.com/githwxi/ATS-Postiats/blob/master/ccomp/runtime/pats_ccomp_typedefs.h)


## struct types

* struct atstype_struct ;

of indefinite size

## void types

* typedef void atstype_void ;
* typedef void atsvoid_t0ype ;

## primitive types

* typedef int atstype_int ;
* typedef unsigned int atstype_uint ;

* typedef long int atstype_lint ;
* typedef unsigned long int atstype_ulint ;

* typedef long long int atstype_llint ;
* typedef unsigned long long int atstype_ullint ;

* typedef short int atstype_sint ;
* typedef unsigned short int atstype_usint ;

## size types

* typedef atstype_lint atstype_ssize ;
* typedef atstype_ulint atstype_size ;

## bool type

HX: true/false: 1/0

* typedef int atstype_bool ;

## byte type

* typedef unsigned char atstype_byte ;

## char types

* typedef char atstype_char ;
* typedef signed char atstype_schar ;
* typedef unsigned char atstype_uchar ;

## string types

* typedef char *atstype_string ;
* typedef char *atstype_stropt ;
* typedef char *atstype_strptr ;

## floating point types

* typedef float atstype_float ;
* typedef double atstype_double ;
* typedef long double atstype_ldouble ;

## pointer types

HX: for pointers

* typedef void *atstype_ptr ;
* typedef void *atstype_ptrk ;

HX: for references

* typedef void *atstype_ref ;

HX: for boxed values

* typedef void* atstype_boxed ;

HX: for [datconptr]

* typedef void* atstype_datconptr ;

HX: for [datcontyp]

* typedef void* atstype_datcontyp ;

## exncon types

	#ifndef _ATS_CCOMP_EXCEPTION_NONE

* atstype_exncon

	typedef
	struct
	{
	  int exntag ; char *exnmsg ;
	} atstype_exncon ;

* typedef atstype_exncon *atstype_exnconptr ;

	#endif // end of [_ATS_CCOMP_EXCEPTION_NONE]

## array pointer types

HX: for pointers to arrays

* typedef void* atstype_arrptr ;

HX: for arrays plus size info

* atstype_arrpsz

		typedef
		struct {
		  atstype_arrptr ptr ; atstype_size size ;
		} atstype_arrpsz ;

## function types

* typedef void* atstype_funptr ;
* typedef void* atstype_cloptr ;

## kind types

* atstkind_type(tk)

		tk
* atstkind_t0ype(tk)

		tk

##

HX-2014-05: making it not usable!!!

	#ifndef _ATSTYPE_VAR_SIZE
	#define _ATSTYPE_VAR_SIZE 0X10000
	#endif // end of [#ifndef]
	//
	// HX-2014-05:
	// for 8-bit or 16-bit march,
	// _ATSTYPE_VAR_SIZE can be set to 0x100
	//

* asttype_var

		typedef
		struct{char _[_ATSTYPE_VAR_SIZE];} atstype_var[0] ;

##

* atstyvar_type(a)

		atstype_var

##

* atstybox_type(hit)

		atstype_boxed

##

* atstyclo_top

		struct{ void *cfun; }
* atstyclo_type(flab)

		flab##__closure_t0ype

##

* atsrefarg0_type(hit)

		hit

* atsrefarg1_type(hit)

		atstype_ref

# [pats_ccomp_instrset.h](https://github.com/githwxi/ATS-Postiats/blob/master/ccomp/runtime/pats_ccomp_instrset.h)

## bool

bool_true bool_false

define of bool value

* atsbool_true

		1

* atsbool_false

		0

## Type

tysum tyexn tylist tyclo tylazy

* ATStysum()

		struct{ int contag; }

	struct of sum
* ATStyexn()

		struct{ int exntag; char *exnmsg; }
	struct of exn
* ATStylist(tyelt)

		struct{ tyelt head; void *tail; }
	struct of list
* ATStyclo()

		struct{ void *cfun; }

	struct of closure
* ATStylazy(tyval)

		struct{ int flag; union{ void* thunk; tyval saved; } lazy; }

	struct of lazy evalution

## empty

* ATSempty()

		　
	empty

## If

if(x) ifnot(x) then() else()

Cのif elseと条件を判定するifnotとthenがあってthen elseに括弧がつく

* ATSif(x)

		if (x)
* ATSifnot(x)

		if (!(x))
* ATSthen()

		　
* ATSelse()

		else

## Statements

do() while(x) break() continue() goto(lab)

Cのdo while break continue gotoと変わらないけど、括弧がつく

* ATSdo()

		do
* ATSwhile(x)

		while (x)
* ATSbreak()

		break
* ATScontinue()

		continue
* ATSgoto(lab)

		goto lab

## Return

return(x) return_void(x)

リターン用

* ATSreturn(x)

		return (x)
* ATSreturn_void(x)

		return

## FC Return

return(x) return_void(x)

リターン用の別バージョン

* ATSFCreturn(x)

		return (x)
* ATSFCreturn_void(x)

		(x); return

## caseof branch

caseofbeg() caseofend()
branchbeg() branchend()

caseofのブロックは１回のみ実行するdo whileブロック
branchのブロックはbreakするだけ

* ATScaseofbeg()

		do {
* ATScaseofend()

		} while (0) ;
* ATSbranchbeg()

		　
* ATSbranchend()

		break

## tail call

tailcalbeg() tailcalend()

tailcalのブロックは１回のみ実行するdo whileブロック

* ATStailcalbeg()

		do {
* ATStailcalend()

		} while (0) ;

## PMV primitives

int(i) intrep(str)
bool_true() bool_false()
char(c) float(rep) string(str)
i0int(x) f0loat(x)

boolは最終的に0か1になり、以外は値をそのまま使用する

* ATSPMVint(i)

		i
* ATSPMVintrep(str)

		str

* ATSPMVbool_true()

		atsbool_true

	ats_bool_true == 1

* ATSPMVbool_false()

		atsbool_false

	ats_bool_false == 0

* ATSPMVchar(c)

		c
* ATSPMVfloat(rep)

		rep
* ATSPMVstring(str)

		str
* ATSPMVi0nt(x)

		x
* ATSPMVf0loat(x)

		x

## CSTSP my

fil(info) loc(info)

これらもそのままの値を使用する

* ATSCSTSPmyfil(info)

		info
* ATSCSTSPmyloc(info)

		info

## PMV top empty

top() empty()

asterror_top asterror_emptyに置き換わる

* ATSPMVtop()

		atserror_top
* ATSPMVempty()

		atserror_empty

## PMV extval

extval(id)

括弧がついた値になる

* ATSPMVextval(id)

		(id)

## PMV funlab

funlab(flab) cfunlab(knd, flab, env)

funlabは括弧がついた値になる
cfunlabはenvをflabに__closurerizeを繋げたトークンでキャストした値になる

* ATSPMVfunlab(flab)

		(flab)

* ATSPMVcfunlab(knd, flab, env)

		(flab##__closurerize)env

## PMV prtof

ptrof(lval) ptrof_void(lval)

* ATSPMVptrof(lval)

		(&(lval))
* ATSPMVptrof_void(lval)

		((void*)0)

## PMV refarg

refarg0 refarg1

括弧がついた値になる

* ATSPMVrefarg0(val)

		(val)
* ATSPMVrefarg1(ref)

		(ref)

## PMV sizeof castfn 

sizeof(hit) castfn(d2c, hit, arg)

sizeofを求める値と
キャストした値になるd2cは無視される

* ATSPMVsizeof(hit)

		(sizeof(hit))

HX: castfn application

* ATSPMVcastfn(d2c, hit, arg)

		((hit)arg)

## Functions

fcall funclo_fun funclo_clo

* ATSfcall(fun, args)

		(fun)args
* ATSfunclo_fun(pmv, targs, tres)

		((tres(*)targs)(pmv))
* ATSfunclo_clo(pmv, targs, tres)

		((tres(*)targs)(((ATStyclo()*)pmv)->cfun))

## tmpdec statmpdec

tmpdec tmpdec_void
statmpdec statmpdec_void

* ATStmpdec(tmp, hit)

		hit tmp
* ATStmpdec_void(tmp, hit)

		　
* ATSstatmpdec(tmp, hit)

		static hit tmp
* ATSstatmpdec_void(tmp, hit)

		　

## deref

* ATSderef(pmv, hit)

		(*(hit*)pmv)

## SEL

con recsin fltrec boxrec arrind arrptrind

[ATSSELcon] is the same as [ATSSELboxrec]

* ATSSELcon(pmv, tysum, lab)

		(((tysum*)pmv)->lab)

* ATSSELrecsin(pmv, tyrec, lab)

		(pmv)
* ATSSELfltrec(pmv, tyrec, lab)

		((pmv).lab)
* ATSSELboxrec(pmv, tyrec, lab)

		(((tyrec*)pmv)->lab)
* ATSSELarrind(pmv, tyarr, lab)

		(((tyarr)pmv).lab)
* ATSSELarrptrind(pmv, tyelt, lab)

		(((tyelt*)pmv)lab)

## CK

not iseqz isneqz ptriscons ptrisnull

* ATSCKnot(x)

		((x)==0)
* ATSCKiseqz(x)

		((x)==0)
* ATSCKisneqz(x)

		((x)!=0)
* ATSCKptriscons(x)

		(0 != (void*)(x))
* ATSCKptrisnull(x)

		(0 == (void*)(x))

## handling for/while loops

break2 continue2


* ATSbreak2(fini)

		goto fini
* ATScontinue2(cont)

		goto cont

## LOOP open close

open close

* ATSLOOPopen(init, fini, cont)

		do { init:
* ATSLOOPclose(init, fini, cont)

		goto init ; fini: break ; } while(0)

## PATCK primitives

int bool char float string

* ATSPATCKint(pmv, pat)

		((pmv)==pat)
* ATSPATCKbool(pmv, pat)

		((pmv)==pat)
* ATSPATCKchar(pmv, pat)

		((pmv)==pat)
* ATSPATCKfloat(pmv, pat)

		((pmv)==pat)
* ATSPATCKstring(pmv, pat)

		(atspre_string_equal(pmv, pat))

## DATA

a datatype should not contain more than 1024 constructors!

* ATS_DATACONMAX

		1024

## PATCK con0 con1 exn0 exn1

con0 con1
exn0 exn1

* ATSPATCKcon0(pmv, tag)

		((pmv)==(void*)tag)
* ATSPATCKcon1(pmv, tag)

		((pmv)>=(void*)ATS_DATACONMAX && ((ATStysum()*)(pmv))->contag==tag)

* ATSPATCKexn0(pmv, d2c)

		((pmv)==(void*)(&(d2c)))
* ATSPATCKexn1(pmv, d2c)

		(((ATStyexn()*)(pmv))->exntag==(&(d2c))->exntag)

## INS lab freeclo freecon 

lab freeclo freecon 

* ATSINSlab(lab)

		lab

* ATSINSfreeclo(cloptr)

		ATS_MFREE(cloptr)
* ATSINSfreecon(datconptr)

		ATS_MFREE(datconptr)

## INS move

move pmove
move_void pmove_void
move_ptralloc

* ATSINSmove(tmp, val)

		(tmp = val)
* ATSINSpmove(tmp, hit, val)

		(*(hit*)tmp = val)

Do not have parentheses around [command]

* ATSINSmove_void(tmp, command)

		command
* ATSINSpmove_void(tmp, hit, command)

		command


* ATSINSmove_ptralloc(tmp, hit)

		(tmp = ATS_MALLOC(sizeof(hit)))

## INS con

move_con0 move_con1
store_con_tag
store_con_ofs

* ATSINSmove_con0(tmp, tag)

		(tmp = ((void*)tag))
* ATSINSmove_con1(tmp, tysum)

		(tmp = ATS_MALLOC(sizeof(tysum)))
* ATSINSstore_con_tag(tmp, val)

		(((ATStysum()*)(tmp))->contag = val)
* ATSINSstore_con_ofs(tmp, tysum, lab, val)

		(((tysum*)(tmp))->lab = val)

## INS exn

move_exn0 move_exn1
store_exntag
store_exnmsg

* ATSINSmove_exn0(tmp, d2c)

		(tmp = &(d2c))
* ATSINSmove_exn1(tmp, tyexn)

		(tmp = ATS_MALLOC(sizeof(tyexn)))
* ATSINSstore_exntag(tmp, d2c)

		(((ATStyexn()*)tmp)->exntag = (&(d2c))->exntag)
* ATSINSstore_exnmsg(tmp, d2c)

		(((ATStyexn()*)tmp)->exnmsg = (&(d2c))->exnmsg)

## INS tlcal

move_tlcal argmove_tlcal store_fltrec_ofs

* ATSINSmove_tlcal(argx, tmp)

		(argx = tmp)
* ATSINSargmove_tlcal(arg, argx)

		(arg = argx)


* ATSINSstore_fltrec_ofs(tmp, tyrec, lab, val)

		((tmp).lab = val)

## INS boxrec

move_boxrec store_boxrec_ofs

* ATSINSmove_boxrec(tmp, tyrec)

		(tmp = ATS_MALLOC(sizeof(tyrec)))
* ATSINSstore_boxrec_ofs(tmp, tyrec, lab, val)

		(((tyrec*)(tmp))->lab = val)

## INS load store

load store xstore

* ATSINSload(tmp, pmv)

		(tmp = pmv)
* ATSINSstore(pmv1, pmv2)

		(pmv1 = pmv2)
* ATSINSxstore(tmp, pmv1, pmv2)

		(tmp = pmv1, pmv1 = pmv2, pmv2 = tmp)

## INS move_list

move_list_nil move_list_phead move_list_ptail
pmove_list_nil pmove_list_cons

* ATSINSmove_list_nil(tmp)

		(tmp = (void*)0)
* ATSINSmove_list_phead(tmp1, tmp2, tyelt)

		(tmp1 = &(((ATStylist(tyelt)*)(*(void**)tmp2))->head))
* ATSINSmove_list_ptail(tmp1, tmp2, tyelt)

		(tmp1 = &(((ATStylist(tyelt)*)(*(void**)tmp2))->tail))
* ATSINSpmove_list_nil(tmp)

		(*(void**)tmp = (void*)0)
* ATSINSpmove_list_cons(tmp, tyelt)

		(*(void**)tmp = ATS_MALLOC(sizeof(ATStylist(tyelt))))

## INS arrpsz update

store_arrpsz_asz
store_arrpsz_ptr move_arrpsz_ptr
update_ptrinc
update_ptrdec

* ATSINSstore_arrpsz_asz(tmp, asz)

		((tmp).size = asz)
* ATSINSstore_arrpsz_ptr(tmp, tyelt, asz)

		((tmp).ptr = ATS_MALLOC(asz*sizeof(tyelt)))

* ATSINSmove_arrpsz_ptr(tmp, psz)

		(tmp = (psz).ptr)

* ATSINSupdate_ptrinc(tmp, tyelt)

		(tmp = (tyelt*)(tmp) + 1)
* ATSINSupdate_ptrdec(tmp, tyelt)

		(tmp = (tyelt*)(tmp) - 1)

## INS closure

closure_initize

* ATSINSclosure_initize(flab, tmpenv)

		(flab##__closureinit)tmpenv

## INS error

raise_exn caseof_fail funarg_fail

* ATSINSraise_exn(tmp, pmv)

		atsruntime_raise(pmv)

* ATSINScaseof_fail(msg)

		atsruntime_handle_unmatchedval(msg)

* ATSINSfunarg_fail(msg)

		atsruntime_handle_unmatchedarg(msg)

## INS lazy

move_delay move_lazyeval
move_ldelay move_llazyeval
atspre_lazy_vt_free

遅延評価用の命令群である

* ATSINSmove_delay(tmpret, tyval, pmv_thk)

        do {
          tmpret =
            ATS_MALLOC(sizeof(ATStylazy(tyval))) ;
          (*(ATStylazy(tyval)*)tmpret).flag = 0 ;
          (*(ATStylazy(tyval)*)tmpret).lazy.thunk = pmv_thk ;
        } while (0) ; /* end of [do ... while ...] */

* ATSINSmove_lazyeval(tmpret, tyval, pmv_lazy)

        do {
          if (
            (*(ATStylazy(tyval)*)pmv_lazy).flag==0
          ) {
            (*(ATStylazy(tyval)*)pmv_lazy).flag += 1 ;
            atstype_cloptr __thunk = (*(ATStylazy(tyval)*)pmv_lazy).lazy.thunk ;
            tmpret = ATSfcall(ATSfunclo_clo(__thunk, (atstype_cloptr), tyval), (__thunk)) ;
            (*(ATStylazy(tyval)*)pmv_lazy).lazy.saved = tmpret ;
          } else {
            tmpret = (*(ATStylazy(tyval)*)pmv_lazy).lazy.saved ;
          } /* end of [if] */
        } while (0) /* end of [do ... while ...] */

* ATSINSmove_ldelay(tmpret, tyval, pmv_thk)

		ATSINSmove(tmpret, pmv_thk)

* ATSINSmove_llazyeval(tmpret, tyval, __thunk)

        do {
         tmpret =
         ATSfcall(ATSfunclo_clo(__thunk, (atstype_cloptr, atstype_bool), tyval), (__thunk, atsbool_true)) ;
         ATS_MFREE(__thunk) ;
        } while (0) /* end of [do ... while ...] */

* atspre_lazy_vt_free(__thunk)

        do {
          ATSfcall(ATSfunclo_clo(__thunk, (atstype_cloptr, atstype_bool), void), (__thunk, atsbool_false)) ;
          ATS_MFREE(__thunk) ;
        } while (0) /* atspre_lazy_vt_free */

# [pats_ccomp_memalloc.h](https://github.com/githwxi/ATS-Postiats/blob/master/ccomp/runtime/pats_ccomp_memalloc.h)

## malloc functions

* extern void atsruntime_mfree_undef (void *ptr) ;
* extern void *atsruntime_malloc_undef (size_t bsz) ;
* extern void *atsruntime_calloc_undef (size_t asz, size_t tsz) ;
* extern void *atsruntime_realloc_undef (void *ptr, size_t bsz) ;

## undef

* undef ATS_MEMALLOC_FLAG

## if defined ATS_MEMALLOC_LIBC

* ATS_MEMALLOC_FLAG

	#include "pats_ccomp_memalloc_libc.h"

* ATS_MINIT atsruntime_minit_libc
* ATS_MFREE atsruntime_mfree_libc
* ATS_MALLOC atsruntime_malloc_libc_exn
* ATS_CALLOC atsruntime_calloc_libc_exn
* ATS_REALLOC atsruntime_realloc_libc_exn


## if defined ATS_MEMALLOC_GCBDW

* ATS_MEMALLOC_FLAG

	#include "pats_ccomp_memalloc_gcbdw.h"

* ATS_MINIT atsruntime_minit_gcbdw
* ATS_MFREE atsruntime_mfree_gcbdw
* ATS_MALLOC atsruntime_malloc_gcbdw_exn
* ATS_CALLOC atsruntime_calloc_gcbdw_exn
* ATS_REALLOC atsruntime_realloc_gcbdw_exn

## if defined ATS_MEMALLOC_USER

* ATS_MEMALLOC_FLAG

	#include "pats_ccomp_memalloc_user.h"

* ATS_MINIT atsruntime_minit_user
* ATS_MFREE atsruntime_mfree_user
* ATS_MALLOC atsruntime_malloc_user
* ATS_CALLOC atsruntime_calloc_user
* ATS_REALLOC atsruntime_realloc_user

## not if defined ATS_MEMALLOC_FLAG

* ATS_MINIT atsruntime_minit_undef
* ATS_MFREE atsruntime_mfree_undef
* ATS_MALLOC atsruntime_malloc_undef
* ATS_CALLOC atsruntime_calloc_undef
* ATS_REALLOC atsruntime_realloc_undef

# [pats_ccomp_memalloca.h](https://github.com/githwxi/ATS-Postiats/blob/master/ccomp/runtime/pats_ccomp_memalloca.h)

* extern void *alloca (size_t bsz) ;

HX: [afree] matches [alloca]

* atsruntime_afree_libc

		ATSinline()
		atsvoid_t0ype
		atsruntime_afree_libc
		  (atstype_ptr ptr) { return ; }

* atsruntime_alloca_libc

		ATSinline()
		atstype_ptr
		atsruntime_alloca_libc
		  (atstype_size bsz) { return alloca(bsz) ; }


# [pats_ccomp_exception.h](https://github.com/githwxi/ATS-Postiats/blob/master/ccomp/runtime/pats_ccomp_exception.h)


	#include <setjmp.h>

## jump functions

* atstype_jmp_buf jmp_buf
* atspre_setjmp(env, mask) setjmp(env)
* atspre_longjmp(env, ret) longjmp(env, ret)

## alloca

* extern void *alloca (size_t bsz) ;

## atsexnframe types

* atsexnframe_t

		typedef
		struct atsexnframe
		{
		  atstype_jmp_buf env ;
		  atstype_exnconptr exn ;
		  struct atsexnframe *prev ;
		} atsexnframe_t ;

* atsexnframe_ptr

		typedef
		atsexnframe_t *atsexnframe_ptr ;

## atsexnframe alloc mfree

* atsexnframe_alloc()

		alloca(sizeof(atsexnframe_t))

* atsexnframe_mfree(frame)

		/* there-is-nothing-to-do */

## my_atsexnframe_getref

* extern atsexnframe_ptr *my_atsexnframe_getref () ;

## my_atsexnframe inline functions

* my_atsexnframe_enter

		static
		inline
		void my_atsexnframe_enter
		(
		  atsexnframe_ptr frame
		, atsexnframe_ptr *framep
		) {
		  frame->prev = *framep ; *framep = frame ; return ;
		} // end of [my_atsexnframe_enter]

* my_atsexnframe_leave

		static
		inline
		void my_atsexnframe_leave
		(
		  atsexnframe_ptr *framep
		) {
		  atsexnframe_mfree(*framep) ; *framep = (*framep)->prev ; return ;
		} // end of [my_atsexnframe_leave]

## trywith

try with end

	HX:
	beg-of-WARNING:
	DO NOT USE THE FOLLOWING MACROS:

* ATStrywith_try(tmpexn)

		do { \
		  int flag ; \
		  atsexnframe_ptr frame ; \
		  atsexnframe_ptr *framep ; \
		  frame = atsexnframe_alloc() ; \
		  framep = my_atsexnframe_getref() ; \
		  my_atsexnframe_enter(frame, framep) ; \
		  flag = atspre_setjmp(frame->env, 1) ; \
		  if (flag==0) { /* normal */

* ATStrywith_with(tmpexn)

		    my_atsexnframe_leave(framep) ; \
		  } else { /* flag<>0 : exceptional */ \
		    tmpexn = (*framep)->exn ; \
		    my_atsexnframe_leave(framep) ;

* ATStrywith_end(tmpexn)

		  } \
		} while(0) ; /* end of [do] */
