#include "share/atspre_staload.hats"
#define ATS_DYNLOADFLAG 0
#include "./syntax.hats"

overload print with show_e
implement show_e(e) =
  begin case e of
  | Int(i) => print!("Int(",i,")")
  | Bin(e1,op1,e2) => print!("Bin(",e1,", ",op1,",", e2,")")
  end

implement read_all(filename) =
  let
    val-~Some_vt(filr) = fileref_open_opt (filename, file_mode_r)
    val charss = fileref_get_lines_charlstlst (filr)
  in
    list0_foldleft(charss, list0_nil(), lam (a,b) =>
      list0_append(a,list0_cons('\n',b))
    )
  end
