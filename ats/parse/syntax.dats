#include "syntax.hats"
#define ATS_DYNLOADFLAG 0

overload print with show_e
implement show_e(e) =
  begin case e of
  | Int(i) => print!("Int(",i,")")
  | Bin(e1,op1,e2) => print!("Bin(",e1,", \"",op1,"\", ", e2,")")
  end

implement read_charlst(filename:string):list0(char) =
  let
    val-~Some_vt(filr) = fileref_open_opt (filename, file_mode_r)
    val charss = fileref_get_lines_charlstlst (filr)
    val () = fileref_close(filr)
  in
    list0_foldleft(charss, list0_nil(), lam (a,b) =>
      list0_append(a,list0_cons('\n',b))
    )
  end

staload "libats/ML/SATS/string.sats"
staload "libats/ML/DATS/string.dats"
implement read_all(filename:string):string =
  let
    val-~Some_vt(filr) = fileref_open_opt (filename, file_mode_r)
    val ss:list0(string) = fileref_get_lines_stringlst (filr)
    val () = fileref_close(filr)
  in
    stringlst_concat(ss)
  end
