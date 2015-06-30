staload "./peg.sats"
staload "libats/ML/SATS/basis.sats"
staload "libats/ML/SATS/string.sats"
staload "libats/ML/DATS/string.dats"
staload "libats/ML/DATS/array0.dats"
staload "libats/ML/SATS/strarr.sats"
staload "libats/ML/DATS/strarr.dats"
staload "libats/ML/SATS/list0.sats"
staload "libats/ML/DATS/list0.dats"

infixr <||>
macdef <||>(a1,b1) = a(,(a1), (,(b1)))
infixr <|>
macdef <|>(this,that) = peg_or(,(this), (,(that)))
infixr <~>
macdef <~>(this,that) = peg_seq(,(this), (,(that)))
infixr ~>
macdef ~> (this,that) = peg_seqr(,(this), ,(that))
infixr <~
macdef <~ (this,that) = peg_seql(,(this), ,(that))
infixr ^^
macdef ^^ (a,b) = peg_action(,(a), ,(b))
infixr ^?^
macdef ^?^ (a,b) = peg_action1(,(a), ,(b))

