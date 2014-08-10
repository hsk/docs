ATSのコンパイラ結果マクロ

## bool

bool_true bool_false

boolの値の定義

* atsbool_true

		1

* atsbool_false

		0

## Type

tysum tyexn tylist tyclo tylazy

* ATStysum()

		struct{ int contag; }

	sum用の構造体
* ATStyexn()

		struct{ int exntag; char *exnmsg; }
	exn用の構造体
* ATStylist(tyelt)

		struct{ tyelt head; void *tail; }
	リスト用の構造体
* ATStyclo()

		struct{ void *cfun; }

	クロージャ用の構造体
* ATStylazy(tyval)

		struct{ int flag; union{ void* thunk; tyval saved; } lazy; }

	遅延評価用の構造体

## empty

* ATSempty()

		　
	なにもしない

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

