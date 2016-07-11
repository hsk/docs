open Lambda

let () =
  assert (subst Tm_true 1 (Tm_var 1) = Tm_true);
  assert (subst Tm_true 1 (Tm_if(Tm_var 1, Tm_var 2, Tm_var 1)) =
          Tm_if(Tm_true, Tm_var 2, Tm_true));
  assert (eval Tm_true  = Tm_true);
  assert (eval Tm_false = Tm_false);
  assert (eval (Tm_if(Tm_true,Tm_false,Tm_true)) = Tm_false);
