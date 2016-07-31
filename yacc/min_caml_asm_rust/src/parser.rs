#![allow(unused_imports)]
#![allow(unused_variables)]
use std::str::FromStr;
use ast::*;
extern crate lalrpop_util as __lalrpop_util;
use self::__lalrpop_util::ParseError as __ParseError;

mod __parse__program {
    #![allow(non_snake_case, non_camel_case_types, unused_mut, unused_variables, unused_imports)]

    use std::str::FromStr;
    use ast::*;
    extern crate lalrpop_util as __lalrpop_util;
    use self::__lalrpop_util::ParseError as __ParseError;
    pub fn parse_program<
        'input,
    >(
        input: &'input str,
    ) -> Result<Prog, __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __tokens = super::__intern_token::__Matcher::new(input);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match try!(__state0(input, &mut __tokens, __lookahead)) {
            (Some(__lookahead), _) => {
                Err(__ParseError::ExtraToken { token: __lookahead })
            }
            (None, __Nonterminal::____program((_, __nt, _))) => {
                Ok(__nt)
            }
            _ => unreachable!(),
        }
    }

    #[allow(dead_code)]
    pub enum __Nonterminal<> {
        _28_3cfundef_3e_29((usize, Fundef, usize)),
        _28_3cfundef_3e_29_2a((usize, ::std::vec::Vec<Fundef>, usize)),
        _28_3cfundef_3e_29_2b((usize, ::std::vec::Vec<Fundef>, usize)),
        _28_3cid__ty_3e_20_22_2c_22_29((usize, String, usize)),
        _28_3cid__ty_3e_20_22_2c_22_29_2a((usize, ::std::vec::Vec<String>, usize)),
        _28_3cid__ty_3e_20_22_2c_22_29_2b((usize, ::std::vec::Vec<String>, usize)),
        _28_3cty_3e_20_22_2c_22_29((usize, T, usize)),
        _28_3cty_3e_20_22_2c_22_29_2a((usize, ::std::vec::Vec<T>, usize)),
        _28_3cty_3e_20_22_2c_22_29_2b((usize, ::std::vec::Vec<T>, usize)),
        ____program((usize, Prog, usize)),
        ____ty((usize, T, usize)),
        args((usize, Vec<String>, usize)),
        exp((usize, Exp, usize)),
        fundef((usize, Fundef, usize)),
        id__or__imm((usize, IdOrImm, usize)),
        id__ty((usize, String, usize)),
        id__ty_3f((usize, ::std::option::Option<String>, usize)),
        ident((usize, String, usize)),
        int((usize, i32, usize)),
        program((usize, Prog, usize)),
        term((usize, E, usize)),
        ty((usize, T, usize)),
        ty_3f((usize, ::std::option::Option<T>, usize)),
        tys((usize, Vec<T>, usize)),
    }

    pub fn __state0<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (6, __tok0), __loc2)) => {
                let mut __sym0 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state7(input, __tokens, __sym0));
            }
            Some((__loc1, (8, __tok0), __loc2)) => {
                let mut __sym0 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state8(input, __tokens, __sym0));
            }
            Some((__loc1, (9, __tok0), __loc2)) => {
                let mut __sym0 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state9(input, __tokens, __sym0));
            }
            Some((__loc1, (11, __tok0), __loc2)) => {
                let mut __sym0 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state10(input, __tokens, __sym0));
            }
            Some((__loc1, (12, __tok0), __loc2)) => {
                let mut __sym0 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state11(input, __tokens, __sym0));
            }
            Some((__loc1, (13, __tok0), __loc2)) => {
                let mut __sym0 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state12(input, __tokens, __sym0));
            }
            Some((__loc1, (15, __tok0), __loc2)) => {
                let mut __sym0 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state13(input, __tokens, __sym0));
            }
            Some((__loc1, (16, __tok0), __loc2)) => {
                let mut __sym0 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state14(input, __tokens, __sym0));
            }
            Some((__loc1, (17, __tok0), __loc2)) => {
                let mut __sym0 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state15(input, __tokens, __sym0));
            }
            Some((__loc1, (18, __tok0), __loc2)) => {
                let mut __sym0 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state16(input, __tokens, __sym0));
            }
            Some((__loc1, (19, __tok0), __loc2)) => {
                let mut __sym0 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state17(input, __tokens, __sym0));
            }
            Some((__loc1, (20, __tok0), __loc2)) => {
                let mut __sym0 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state18(input, __tokens, __sym0));
            }
            Some((__loc1, (21, __tok0), __loc2)) => {
                let mut __sym0 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state19(input, __tokens, __sym0));
            }
            Some((__loc1, (22, __tok0), __loc2)) => {
                let mut __sym0 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state20(input, __tokens, __sym0));
            }
            Some((__loc1, (23, __tok0), __loc2)) => {
                let mut __sym0 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state21(input, __tokens, __sym0));
            }
            Some((__loc1, (24, __tok0), __loc2)) => {
                let mut __sym0 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state22(input, __tokens, __sym0));
            }
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym0 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state23(input, __tokens, __sym0));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        loop {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::_28_3cfundef_3e_29_2b(__nt) => {
                    let __sym0 = &mut Some(__nt);
                    __result = try!(__state1(input, __tokens, __lookahead, __sym0));
                }
                __Nonterminal::exp(__nt) => {
                    let __sym0 = &mut Some(__nt);
                    __result = try!(__state2(input, __tokens, __lookahead, __sym0));
                }
                __Nonterminal::fundef(__nt) => {
                    let __sym0 = &mut Some(__nt);
                    __result = try!(__state3(input, __tokens, __lookahead, __sym0));
                }
                __Nonterminal::ident(__nt) => {
                    let __sym0 = &mut Some(__nt);
                    __result = try!(__state4(input, __tokens, __lookahead, __sym0));
                }
                __Nonterminal::program(__nt) => {
                    let __sym0 = &mut Some(__nt);
                    __result = try!(__state5(input, __tokens, __lookahead, __sym0));
                }
                __Nonterminal::term(__nt) => {
                    let __sym0 = &mut Some(__nt);
                    __result = try!(__state6(input, __tokens, __lookahead, __sym0));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
    }

    pub fn __state1<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, ::std::vec::Vec<Fundef>, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (6, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state7(input, __tokens, __sym1));
            }
            Some((__loc1, (8, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state8(input, __tokens, __sym1));
            }
            Some((__loc1, (9, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state9(input, __tokens, __sym1));
            }
            Some((__loc1, (11, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state10(input, __tokens, __sym1));
            }
            Some((__loc1, (12, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state11(input, __tokens, __sym1));
            }
            Some((__loc1, (13, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state12(input, __tokens, __sym1));
            }
            Some((__loc1, (15, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state13(input, __tokens, __sym1));
            }
            Some((__loc1, (16, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state14(input, __tokens, __sym1));
            }
            Some((__loc1, (17, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state15(input, __tokens, __sym1));
            }
            Some((__loc1, (18, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state16(input, __tokens, __sym1));
            }
            Some((__loc1, (19, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state17(input, __tokens, __sym1));
            }
            Some((__loc1, (20, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state18(input, __tokens, __sym1));
            }
            Some((__loc1, (21, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state19(input, __tokens, __sym1));
            }
            Some((__loc1, (22, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state20(input, __tokens, __sym1));
            }
            Some((__loc1, (23, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state21(input, __tokens, __sym1));
            }
            Some((__loc1, (24, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state22(input, __tokens, __sym1));
            }
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state23(input, __tokens, __sym1));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::exp(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state2(input, __tokens, __lookahead, __sym1));
                }
                __Nonterminal::fundef(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state24(input, __tokens, __lookahead, __sym0, __sym1));
                }
                __Nonterminal::ident(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state4(input, __tokens, __lookahead, __sym1));
                }
                __Nonterminal::term(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state25(input, __tokens, __lookahead, __sym0, __sym1));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state2<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, Exp, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (6, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state7(input, __tokens, __sym1));
            }
            Some((__loc1, (8, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state8(input, __tokens, __sym1));
            }
            Some((__loc1, (11, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state10(input, __tokens, __sym1));
            }
            Some((__loc1, (12, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state11(input, __tokens, __sym1));
            }
            Some((__loc1, (13, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state12(input, __tokens, __sym1));
            }
            Some((__loc1, (15, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state13(input, __tokens, __sym1));
            }
            Some((__loc1, (16, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state14(input, __tokens, __sym1));
            }
            Some((__loc1, (17, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state15(input, __tokens, __sym1));
            }
            Some((__loc1, (18, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state16(input, __tokens, __sym1));
            }
            Some((__loc1, (19, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state17(input, __tokens, __sym1));
            }
            Some((__loc1, (20, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state18(input, __tokens, __sym1));
            }
            Some((__loc1, (21, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state19(input, __tokens, __sym1));
            }
            Some((__loc1, (22, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state20(input, __tokens, __sym1));
            }
            Some((__loc1, (23, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state21(input, __tokens, __sym1));
            }
            Some((__loc1, (24, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state22(input, __tokens, __sym1));
            }
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state23(input, __tokens, __sym1));
            }
            None => {
                let __sym0 = __sym0.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action16(input, __sym0);
                let __nt = __Nonterminal::term((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::exp(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state2(input, __tokens, __lookahead, __sym1));
                }
                __Nonterminal::ident(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state4(input, __tokens, __lookahead, __sym1));
                }
                __Nonterminal::term(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state26(input, __tokens, __lookahead, __sym0, __sym1));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state3<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, Fundef, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((_, (6, _), _)) |
            Some((_, (8, _), _)) |
            Some((_, (9, _), _)) |
            Some((_, (11, _), _)) |
            Some((_, (12, _), _)) |
            Some((_, (13, _), _)) |
            Some((_, (15, _), _)) |
            Some((_, (16, _), _)) |
            Some((_, (17, _), _)) |
            Some((_, (18, _), _)) |
            Some((_, (19, _), _)) |
            Some((_, (20, _), _)) |
            Some((_, (21, _), _)) |
            Some((_, (22, _), _)) |
            Some((_, (23, _), _)) |
            Some((_, (24, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action52(input, __sym0);
                let __nt = __Nonterminal::_28_3cfundef_3e_29_2b((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state4<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, String, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (4, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state27(input, __tokens, __sym0, __sym1));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state5<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, Prog, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            None => {
                let __sym0 = __sym0.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action0(input, __sym0);
                let __nt = __Nonterminal::____program((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state6<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, E, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            None => {
                let __sym0 = __sym0.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action54(input, __sym0);
                let __nt = __Nonterminal::program((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state7<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state29(input, __tokens, __sym1));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::ident(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state28(input, __tokens, __lookahead, __sym0, __sym1));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state8<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state31(input, __tokens, __sym1));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::ident(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state30(input, __tokens, __lookahead, __sym0, __sym1));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state9<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state31(input, __tokens, __sym1));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::ident(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state32(input, __tokens, __lookahead, __sym0, __sym1));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state10<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state29(input, __tokens, __sym1));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::ident(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state33(input, __tokens, __lookahead, __sym0, __sym1));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state11<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state29(input, __tokens, __sym1));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::ident(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state34(input, __tokens, __lookahead, __sym0, __sym1));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state12<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state29(input, __tokens, __sym1));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::ident(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state35(input, __tokens, __lookahead, __sym0, __sym1));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state13<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state29(input, __tokens, __sym1));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::ident(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state36(input, __tokens, __lookahead, __sym0, __sym1));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state14<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state38(input, __tokens, __sym1));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::ident(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state37(input, __tokens, __lookahead, __sym0, __sym1));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state15<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state38(input, __tokens, __sym1));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::ident(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state39(input, __tokens, __lookahead, __sym0, __sym1));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state16<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            None |
            Some((_, (6, _), _)) |
            Some((_, (8, _), _)) |
            Some((_, (11, _), _)) |
            Some((_, (12, _), _)) |
            Some((_, (13, _), _)) |
            Some((_, (15, _), _)) |
            Some((_, (16, _), _)) |
            Some((_, (17, _), _)) |
            Some((_, (18, _), _)) |
            Some((_, (19, _), _)) |
            Some((_, (20, _), _)) |
            Some((_, (21, _), _)) |
            Some((_, (22, _), _)) |
            Some((_, (23, _), _)) |
            Some((_, (24, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action18(input, __sym0);
                let __nt = __Nonterminal::exp((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state17<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state38(input, __tokens, __sym1));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::ident(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state40(input, __tokens, __lookahead, __sym0, __sym1));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state18<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state42(input, __tokens, __sym1));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::ident(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state41(input, __tokens, __lookahead, __sym0, __sym1));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state19<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (28, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state44(input, __tokens, __sym1));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::int(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state43(input, __tokens, __lookahead, __sym0, __sym1));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state20<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state38(input, __tokens, __sym1));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::ident(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state45(input, __tokens, __lookahead, __sym0, __sym1));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state21<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state42(input, __tokens, __sym1));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::ident(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state46(input, __tokens, __lookahead, __sym0, __sym1));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state22<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state29(input, __tokens, __sym1));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::ident(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state47(input, __tokens, __lookahead, __sym0, __sym1));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state23<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((_, (4, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action3(input, __sym0);
                let __nt = __Nonterminal::ident((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state24<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, ::std::vec::Vec<Fundef>, usize)>,
        __sym1: &mut Option<(usize, Fundef, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((_, (6, _), _)) |
            Some((_, (8, _), _)) |
            Some((_, (9, _), _)) |
            Some((_, (11, _), _)) |
            Some((_, (12, _), _)) |
            Some((_, (13, _), _)) |
            Some((_, (15, _), _)) |
            Some((_, (16, _), _)) |
            Some((_, (17, _), _)) |
            Some((_, (18, _), _)) |
            Some((_, (19, _), _)) |
            Some((_, (20, _), _)) |
            Some((_, (21, _), _)) |
            Some((_, (22, _), _)) |
            Some((_, (23, _), _)) |
            Some((_, (24, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action53(input, __sym0, __sym1);
                let __nt = __Nonterminal::_28_3cfundef_3e_29_2b((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state25<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, ::std::vec::Vec<Fundef>, usize)>,
        __sym1: &mut Option<(usize, E, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            None => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action55(input, __sym0, __sym1);
                let __nt = __Nonterminal::program((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state26<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, Exp, usize)>,
        __sym1: &mut Option<(usize, E, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            None => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action17(input, __sym0, __sym1);
                let __nt = __Nonterminal::term((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state27<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, String, usize)>,
        __sym1: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (0, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state49(input, __tokens, __sym2));
            }
            Some((__loc1, (7, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state50(input, __tokens, __sym2));
            }
            Some((__loc1, (14, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state51(input, __tokens, __sym2));
            }
            Some((__loc1, (25, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state52(input, __tokens, __sym2));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym1.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::ty(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state48(input, __tokens, __lookahead, __sym0, __sym1, __sym2));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state28<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (28, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state44(input, __tokens, __sym2));
            }
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state38(input, __tokens, __sym2));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym1.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::id__or__imm(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state53(input, __tokens, __lookahead, __sym0, __sym1, __sym2));
                }
                __Nonterminal::ident(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state54(input, __tokens, __lookahead, __sym2));
                }
                __Nonterminal::int(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state55(input, __tokens, __lookahead, __sym2));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state29<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((_, (28, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action3(input, __sym0);
                let __nt = __Nonterminal::ident((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state30<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (0, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state56(input, __tokens, __sym0, __sym1, __sym2));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state31<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((_, (0, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action3(input, __sym0);
                let __nt = __Nonterminal::ident((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state32<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (0, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state57(input, __tokens, __sym0, __sym1, __sym2));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state33<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (28, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state61(input, __tokens, __sym2));
            }
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state62(input, __tokens, __sym2));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym1.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::id__or__imm(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state58(input, __tokens, __lookahead, __sym0, __sym1, __sym2));
                }
                __Nonterminal::ident(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state59(input, __tokens, __lookahead, __sym2));
                }
                __Nonterminal::int(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state60(input, __tokens, __lookahead, __sym2));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state34<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (28, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state61(input, __tokens, __sym2));
            }
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state62(input, __tokens, __sym2));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym1.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::id__or__imm(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state63(input, __tokens, __lookahead, __sym0, __sym1, __sym2));
                }
                __Nonterminal::ident(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state59(input, __tokens, __lookahead, __sym2));
                }
                __Nonterminal::int(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state60(input, __tokens, __lookahead, __sym2));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state35<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (28, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state61(input, __tokens, __sym2));
            }
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state62(input, __tokens, __sym2));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym1.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::id__or__imm(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state64(input, __tokens, __lookahead, __sym0, __sym1, __sym2));
                }
                __Nonterminal::ident(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state59(input, __tokens, __lookahead, __sym2));
                }
                __Nonterminal::int(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state60(input, __tokens, __lookahead, __sym2));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state36<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (28, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state68(input, __tokens, __sym2));
            }
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state69(input, __tokens, __sym2));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym1.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::id__or__imm(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state65(input, __tokens, __lookahead, __sym0, __sym1, __sym2));
                }
                __Nonterminal::ident(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state66(input, __tokens, __lookahead, __sym2));
                }
                __Nonterminal::int(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state67(input, __tokens, __lookahead, __sym2));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state37<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            None |
            Some((_, (6, _), _)) |
            Some((_, (8, _), _)) |
            Some((_, (11, _), _)) |
            Some((_, (12, _), _)) |
            Some((_, (13, _), _)) |
            Some((_, (15, _), _)) |
            Some((_, (16, _), _)) |
            Some((_, (17, _), _)) |
            Some((_, (18, _), _)) |
            Some((_, (19, _), _)) |
            Some((_, (20, _), _)) |
            Some((_, (21, _), _)) |
            Some((_, (22, _), _)) |
            Some((_, (23, _), _)) |
            Some((_, (24, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action21(input, __sym0, __sym1);
                let __nt = __Nonterminal::exp((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state38<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            None |
            Some((_, (6, _), _)) |
            Some((_, (8, _), _)) |
            Some((_, (11, _), _)) |
            Some((_, (12, _), _)) |
            Some((_, (13, _), _)) |
            Some((_, (15, _), _)) |
            Some((_, (16, _), _)) |
            Some((_, (17, _), _)) |
            Some((_, (18, _), _)) |
            Some((_, (19, _), _)) |
            Some((_, (20, _), _)) |
            Some((_, (21, _), _)) |
            Some((_, (22, _), _)) |
            Some((_, (23, _), _)) |
            Some((_, (24, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action3(input, __sym0);
                let __nt = __Nonterminal::ident((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state39<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            None |
            Some((_, (6, _), _)) |
            Some((_, (8, _), _)) |
            Some((_, (11, _), _)) |
            Some((_, (12, _), _)) |
            Some((_, (13, _), _)) |
            Some((_, (15, _), _)) |
            Some((_, (16, _), _)) |
            Some((_, (17, _), _)) |
            Some((_, (18, _), _)) |
            Some((_, (19, _), _)) |
            Some((_, (20, _), _)) |
            Some((_, (21, _), _)) |
            Some((_, (22, _), _)) |
            Some((_, (23, _), _)) |
            Some((_, (24, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action22(input, __sym0, __sym1);
                let __nt = __Nonterminal::exp((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state40<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            None |
            Some((_, (6, _), _)) |
            Some((_, (8, _), _)) |
            Some((_, (11, _), _)) |
            Some((_, (12, _), _)) |
            Some((_, (13, _), _)) |
            Some((_, (15, _), _)) |
            Some((_, (16, _), _)) |
            Some((_, (17, _), _)) |
            Some((_, (18, _), _)) |
            Some((_, (19, _), _)) |
            Some((_, (20, _), _)) |
            Some((_, (21, _), _)) |
            Some((_, (22, _), _)) |
            Some((_, (23, _), _)) |
            Some((_, (24, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action32(input, __sym0, __sym1);
                let __nt = __Nonterminal::exp((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state41<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state38(input, __tokens, __sym2));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym1.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::ident(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state70(input, __tokens, __lookahead, __sym0, __sym1, __sym2));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state42<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action3(input, __sym0);
                let __nt = __Nonterminal::ident((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state43<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, i32, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            None |
            Some((_, (6, _), _)) |
            Some((_, (8, _), _)) |
            Some((_, (11, _), _)) |
            Some((_, (12, _), _)) |
            Some((_, (13, _), _)) |
            Some((_, (15, _), _)) |
            Some((_, (16, _), _)) |
            Some((_, (17, _), _)) |
            Some((_, (18, _), _)) |
            Some((_, (19, _), _)) |
            Some((_, (20, _), _)) |
            Some((_, (21, _), _)) |
            Some((_, (22, _), _)) |
            Some((_, (23, _), _)) |
            Some((_, (24, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action19(input, __sym0, __sym1);
                let __nt = __Nonterminal::exp((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state44<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            None |
            Some((_, (6, _), _)) |
            Some((_, (8, _), _)) |
            Some((_, (11, _), _)) |
            Some((_, (12, _), _)) |
            Some((_, (13, _), _)) |
            Some((_, (15, _), _)) |
            Some((_, (16, _), _)) |
            Some((_, (17, _), _)) |
            Some((_, (18, _), _)) |
            Some((_, (19, _), _)) |
            Some((_, (20, _), _)) |
            Some((_, (21, _), _)) |
            Some((_, (22, _), _)) |
            Some((_, (23, _), _)) |
            Some((_, (24, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action2(input, __sym0);
                let __nt = __Nonterminal::int((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state45<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            None |
            Some((_, (6, _), _)) |
            Some((_, (8, _), _)) |
            Some((_, (11, _), _)) |
            Some((_, (12, _), _)) |
            Some((_, (13, _), _)) |
            Some((_, (15, _), _)) |
            Some((_, (16, _), _)) |
            Some((_, (17, _), _)) |
            Some((_, (18, _), _)) |
            Some((_, (19, _), _)) |
            Some((_, (20, _), _)) |
            Some((_, (21, _), _)) |
            Some((_, (22, _), _)) |
            Some((_, (23, _), _)) |
            Some((_, (24, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action20(input, __sym0, __sym1);
                let __nt = __Nonterminal::exp((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state46<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state29(input, __tokens, __sym2));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym1.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::ident(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state71(input, __tokens, __lookahead, __sym0, __sym1, __sym2));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state47<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (28, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state44(input, __tokens, __sym2));
            }
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state38(input, __tokens, __sym2));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym1.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::id__or__imm(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state72(input, __tokens, __lookahead, __sym0, __sym1, __sym2));
                }
                __Nonterminal::ident(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state54(input, __tokens, __lookahead, __sym2));
                }
                __Nonterminal::int(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state55(input, __tokens, __lookahead, __sym2));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state48<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, String, usize)>,
        __sym1: &mut Option<(usize, &'input str, usize)>,
        __sym2: &mut Option<(usize, T, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (5, __tok0), __loc2)) => {
                let mut __sym3 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state73(input, __tokens, __sym0, __sym1, __sym2, __sym3));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state49<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (0, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state77(input, __tokens, __sym1));
            }
            Some((__loc1, (7, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state78(input, __tokens, __sym1));
            }
            Some((__loc1, (14, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state79(input, __tokens, __sym1));
            }
            Some((__loc1, (25, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state80(input, __tokens, __sym1));
            }
            Some((_, (1, _), _)) => {
                let __start = __sym0.as_ref().unwrap().2.clone();
                let __end = __lookahead.as_ref().map(|o| o.0.clone()).unwrap_or_else(|| __start.clone());
                let __nt = super::__action69(input, &__start, &__end);
                let __nt = __Nonterminal::tys((
                    __start,
                    __nt,
                    __end,
                ));
                __result = (__lookahead, __nt);
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::_28_3cty_3e_20_22_2c_22_29_2b(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state74(input, __tokens, __lookahead, __sym1));
                }
                __Nonterminal::ty(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state75(input, __tokens, __lookahead, __sym1));
                }
                __Nonterminal::tys(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state76(input, __tokens, __lookahead, __sym0, __sym1));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state50<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((_, (5, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action9(input, __sym0);
                let __nt = __Nonterminal::ty((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state51<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((_, (5, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action10(input, __sym0);
                let __nt = __Nonterminal::ty((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state52<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((_, (5, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action8(input, __sym0);
                let __nt = __Nonterminal::ty((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state53<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            None |
            Some((_, (6, _), _)) |
            Some((_, (8, _), _)) |
            Some((_, (11, _), _)) |
            Some((_, (12, _), _)) |
            Some((_, (13, _), _)) |
            Some((_, (15, _), _)) |
            Some((_, (16, _), _)) |
            Some((_, (17, _), _)) |
            Some((_, (18, _), _)) |
            Some((_, (19, _), _)) |
            Some((_, (20, _), _)) |
            Some((_, (21, _), _)) |
            Some((_, (22, _), _)) |
            Some((_, (23, _), _)) |
            Some((_, (24, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __sym2 = __sym2.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym2.2.clone();
                let __nt = super::__action23(input, __sym0, __sym1, __sym2);
                let __nt = __Nonterminal::exp((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state54<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, String, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            None |
            Some((_, (6, _), _)) |
            Some((_, (8, _), _)) |
            Some((_, (11, _), _)) |
            Some((_, (12, _), _)) |
            Some((_, (13, _), _)) |
            Some((_, (15, _), _)) |
            Some((_, (16, _), _)) |
            Some((_, (17, _), _)) |
            Some((_, (18, _), _)) |
            Some((_, (19, _), _)) |
            Some((_, (20, _), _)) |
            Some((_, (21, _), _)) |
            Some((_, (22, _), _)) |
            Some((_, (23, _), _)) |
            Some((_, (24, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action13(input, __sym0);
                let __nt = __Nonterminal::id__or__imm((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state55<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, i32, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            None |
            Some((_, (6, _), _)) |
            Some((_, (8, _), _)) |
            Some((_, (11, _), _)) |
            Some((_, (12, _), _)) |
            Some((_, (13, _), _)) |
            Some((_, (15, _), _)) |
            Some((_, (16, _), _)) |
            Some((_, (17, _), _)) |
            Some((_, (18, _), _)) |
            Some((_, (19, _), _)) |
            Some((_, (20, _), _)) |
            Some((_, (21, _), _)) |
            Some((_, (22, _), _)) |
            Some((_, (23, _), _)) |
            Some((_, (24, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action14(input, __sym0);
                let __nt = __Nonterminal::id__or__imm((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state56<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym3 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state23(input, __tokens, __sym3));
            }
            Some((_, (1, _), _)) => {
                let __start = __sym2.as_ref().unwrap().2.clone();
                let __end = __lookahead.as_ref().map(|o| o.0.clone()).unwrap_or_else(|| __start.clone());
                let __nt = super::__action65(input, &__start, &__end);
                let __nt = __Nonterminal::args((
                    __start,
                    __nt,
                    __end,
                ));
                __result = (__lookahead, __nt);
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym2.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::_28_3cid__ty_3e_20_22_2c_22_29_2b(__nt) => {
                    let __sym3 = &mut Some(__nt);
                    __result = try!(__state81(input, __tokens, __lookahead, __sym3));
                }
                __Nonterminal::args(__nt) => {
                    let __sym3 = &mut Some(__nt);
                    __result = try!(__state82(input, __tokens, __lookahead, __sym0, __sym1, __sym2, __sym3));
                }
                __Nonterminal::id__ty(__nt) => {
                    let __sym3 = &mut Some(__nt);
                    __result = try!(__state83(input, __tokens, __lookahead, __sym3));
                }
                __Nonterminal::ident(__nt) => {
                    let __sym3 = &mut Some(__nt);
                    __result = try!(__state84(input, __tokens, __lookahead, __sym3));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state57<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym3 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state23(input, __tokens, __sym3));
            }
            Some((_, (1, _), _)) => {
                let __start = __sym2.as_ref().unwrap().2.clone();
                let __end = __lookahead.as_ref().map(|o| o.0.clone()).unwrap_or_else(|| __start.clone());
                let __nt = super::__action65(input, &__start, &__end);
                let __nt = __Nonterminal::args((
                    __start,
                    __nt,
                    __end,
                ));
                __result = (__lookahead, __nt);
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym2.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::_28_3cid__ty_3e_20_22_2c_22_29_2b(__nt) => {
                    let __sym3 = &mut Some(__nt);
                    __result = try!(__state81(input, __tokens, __lookahead, __sym3));
                }
                __Nonterminal::args(__nt) => {
                    let __sym3 = &mut Some(__nt);
                    __result = try!(__state85(input, __tokens, __lookahead, __sym0, __sym1, __sym2, __sym3));
                }
                __Nonterminal::id__ty(__nt) => {
                    let __sym3 = &mut Some(__nt);
                    __result = try!(__state83(input, __tokens, __lookahead, __sym3));
                }
                __Nonterminal::ident(__nt) => {
                    let __sym3 = &mut Some(__nt);
                    __result = try!(__state84(input, __tokens, __lookahead, __sym3));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state58<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (26, __tok0), __loc2)) => {
                let mut __sym3 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state86(input, __tokens, __sym0, __sym1, __sym2, __sym3));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state59<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, String, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((_, (26, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action13(input, __sym0);
                let __nt = __Nonterminal::id__or__imm((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state60<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, i32, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((_, (26, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action14(input, __sym0);
                let __nt = __Nonterminal::id__or__imm((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state61<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((_, (26, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action2(input, __sym0);
                let __nt = __Nonterminal::int((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state62<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((_, (26, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action3(input, __sym0);
                let __nt = __Nonterminal::ident((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state63<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (26, __tok0), __loc2)) => {
                let mut __sym3 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state87(input, __tokens, __sym0, __sym1, __sym2, __sym3));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state64<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (26, __tok0), __loc2)) => {
                let mut __sym3 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state88(input, __tokens, __sym0, __sym1, __sym2, __sym3));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state65<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (28, __tok0), __loc2)) => {
                let mut __sym3 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state44(input, __tokens, __sym3));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym2.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::int(__nt) => {
                    let __sym3 = &mut Some(__nt);
                    __result = try!(__state89(input, __tokens, __lookahead, __sym0, __sym1, __sym2, __sym3));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state66<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, String, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((_, (28, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action13(input, __sym0);
                let __nt = __Nonterminal::id__or__imm((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state67<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, i32, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((_, (28, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action14(input, __sym0);
                let __nt = __Nonterminal::id__or__imm((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state68<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((_, (28, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action2(input, __sym0);
                let __nt = __Nonterminal::int((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state69<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((_, (28, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action3(input, __sym0);
                let __nt = __Nonterminal::ident((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state70<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, String, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            None |
            Some((_, (6, _), _)) |
            Some((_, (8, _), _)) |
            Some((_, (11, _), _)) |
            Some((_, (12, _), _)) |
            Some((_, (13, _), _)) |
            Some((_, (15, _), _)) |
            Some((_, (16, _), _)) |
            Some((_, (17, _), _)) |
            Some((_, (18, _), _)) |
            Some((_, (19, _), _)) |
            Some((_, (20, _), _)) |
            Some((_, (21, _), _)) |
            Some((_, (22, _), _)) |
            Some((_, (23, _), _)) |
            Some((_, (24, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __sym2 = __sym2.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym2.2.clone();
                let __nt = super::__action31(input, __sym0, __sym1, __sym2);
                let __nt = __Nonterminal::exp((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state71<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, String, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (28, __tok0), __loc2)) => {
                let mut __sym3 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state68(input, __tokens, __sym3));
            }
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym3 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state69(input, __tokens, __sym3));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym2.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::id__or__imm(__nt) => {
                    let __sym3 = &mut Some(__nt);
                    __result = try!(__state90(input, __tokens, __lookahead, __sym0, __sym1, __sym2, __sym3));
                }
                __Nonterminal::ident(__nt) => {
                    let __sym3 = &mut Some(__nt);
                    __result = try!(__state66(input, __tokens, __lookahead, __sym3));
                }
                __Nonterminal::int(__nt) => {
                    let __sym3 = &mut Some(__nt);
                    __result = try!(__state67(input, __tokens, __lookahead, __sym3));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state72<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            None |
            Some((_, (6, _), _)) |
            Some((_, (8, _), _)) |
            Some((_, (11, _), _)) |
            Some((_, (12, _), _)) |
            Some((_, (13, _), _)) |
            Some((_, (15, _), _)) |
            Some((_, (16, _), _)) |
            Some((_, (17, _), _)) |
            Some((_, (18, _), _)) |
            Some((_, (19, _), _)) |
            Some((_, (20, _), _)) |
            Some((_, (21, _), _)) |
            Some((_, (22, _), _)) |
            Some((_, (23, _), _)) |
            Some((_, (24, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __sym2 = __sym2.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym2.2.clone();
                let __nt = super::__action24(input, __sym0, __sym1, __sym2);
                let __nt = __Nonterminal::exp((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state73<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, String, usize)>,
        __sym1: &mut Option<(usize, &'input str, usize)>,
        __sym2: &mut Option<(usize, T, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (6, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state92(input, __tokens, __sym4));
            }
            Some((__loc1, (8, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state93(input, __tokens, __sym4));
            }
            Some((__loc1, (11, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state94(input, __tokens, __sym4));
            }
            Some((__loc1, (12, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state95(input, __tokens, __sym4));
            }
            Some((__loc1, (13, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state96(input, __tokens, __sym4));
            }
            Some((__loc1, (15, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state97(input, __tokens, __sym4));
            }
            Some((__loc1, (16, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state98(input, __tokens, __sym4));
            }
            Some((__loc1, (17, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state99(input, __tokens, __sym4));
            }
            Some((__loc1, (18, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state100(input, __tokens, __sym4));
            }
            Some((__loc1, (19, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state101(input, __tokens, __sym4));
            }
            Some((__loc1, (20, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state102(input, __tokens, __sym4));
            }
            Some((__loc1, (21, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state103(input, __tokens, __sym4));
            }
            Some((__loc1, (22, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state104(input, __tokens, __sym4));
            }
            Some((__loc1, (23, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state105(input, __tokens, __sym4));
            }
            Some((__loc1, (24, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state106(input, __tokens, __sym4));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym3.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::exp(__nt) => {
                    let __sym4 = &mut Some(__nt);
                    __result = try!(__state91(input, __tokens, __lookahead, __sym0, __sym1, __sym2, __sym3, __sym4));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state74<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, ::std::vec::Vec<T>, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (0, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state77(input, __tokens, __sym1));
            }
            Some((__loc1, (7, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state78(input, __tokens, __sym1));
            }
            Some((__loc1, (14, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state79(input, __tokens, __sym1));
            }
            Some((__loc1, (25, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state80(input, __tokens, __sym1));
            }
            Some((_, (1, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action71(input, __sym0);
                let __nt = __Nonterminal::tys((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::ty(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state107(input, __tokens, __lookahead, __sym0, __sym1));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state75<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, T, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (2, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state108(input, __tokens, __sym0, __sym1));
            }
            Some((_, (1, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action68(input, __sym0);
                let __nt = __Nonterminal::tys((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state76<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, Vec<T>, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (1, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state109(input, __tokens, __sym0, __sym1, __sym2));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state77<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (0, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state77(input, __tokens, __sym1));
            }
            Some((__loc1, (7, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state78(input, __tokens, __sym1));
            }
            Some((__loc1, (14, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state79(input, __tokens, __sym1));
            }
            Some((__loc1, (25, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state80(input, __tokens, __sym1));
            }
            Some((_, (1, _), _)) => {
                let __start = __sym0.as_ref().unwrap().2.clone();
                let __end = __lookahead.as_ref().map(|o| o.0.clone()).unwrap_or_else(|| __start.clone());
                let __nt = super::__action69(input, &__start, &__end);
                let __nt = __Nonterminal::tys((
                    __start,
                    __nt,
                    __end,
                ));
                __result = (__lookahead, __nt);
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::_28_3cty_3e_20_22_2c_22_29_2b(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state74(input, __tokens, __lookahead, __sym1));
                }
                __Nonterminal::ty(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state75(input, __tokens, __lookahead, __sym1));
                }
                __Nonterminal::tys(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state110(input, __tokens, __lookahead, __sym0, __sym1));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state78<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((_, (1, _), _)) |
            Some((_, (2, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action9(input, __sym0);
                let __nt = __Nonterminal::ty((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state79<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((_, (1, _), _)) |
            Some((_, (2, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action10(input, __sym0);
                let __nt = __Nonterminal::ty((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state80<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((_, (1, _), _)) |
            Some((_, (2, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action8(input, __sym0);
                let __nt = __Nonterminal::ty((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state81<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, ::std::vec::Vec<String>, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state23(input, __tokens, __sym1));
            }
            Some((_, (1, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action67(input, __sym0);
                let __nt = __Nonterminal::args((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::id__ty(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state111(input, __tokens, __lookahead, __sym0, __sym1));
                }
                __Nonterminal::ident(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state84(input, __tokens, __lookahead, __sym1));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state82<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, &'input str, usize)>,
        __sym3: &mut Option<(usize, Vec<String>, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (1, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state112(input, __tokens, __sym0, __sym1, __sym2, __sym3, __sym4));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state83<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, String, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (2, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state113(input, __tokens, __sym0, __sym1));
            }
            Some((_, (1, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action64(input, __sym0);
                let __nt = __Nonterminal::args((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state84<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, String, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (4, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state114(input, __tokens, __sym0, __sym1));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state85<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, &'input str, usize)>,
        __sym3: &mut Option<(usize, Vec<String>, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (1, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state115(input, __tokens, __sym0, __sym1, __sym2, __sym3, __sym4));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state86<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (6, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state119(input, __tokens, __sym4));
            }
            Some((__loc1, (8, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state120(input, __tokens, __sym4));
            }
            Some((__loc1, (11, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state121(input, __tokens, __sym4));
            }
            Some((__loc1, (12, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state122(input, __tokens, __sym4));
            }
            Some((__loc1, (13, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state123(input, __tokens, __sym4));
            }
            Some((__loc1, (15, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state124(input, __tokens, __sym4));
            }
            Some((__loc1, (16, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state125(input, __tokens, __sym4));
            }
            Some((__loc1, (17, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state126(input, __tokens, __sym4));
            }
            Some((__loc1, (18, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state127(input, __tokens, __sym4));
            }
            Some((__loc1, (19, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state128(input, __tokens, __sym4));
            }
            Some((__loc1, (20, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state129(input, __tokens, __sym4));
            }
            Some((__loc1, (21, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state130(input, __tokens, __sym4));
            }
            Some((__loc1, (22, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state131(input, __tokens, __sym4));
            }
            Some((__loc1, (23, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state132(input, __tokens, __sym4));
            }
            Some((__loc1, (24, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state133(input, __tokens, __sym4));
            }
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state23(input, __tokens, __sym4));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym3.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::exp(__nt) => {
                    let __sym4 = &mut Some(__nt);
                    __result = try!(__state116(input, __tokens, __lookahead, __sym4));
                }
                __Nonterminal::ident(__nt) => {
                    let __sym4 = &mut Some(__nt);
                    __result = try!(__state117(input, __tokens, __lookahead, __sym4));
                }
                __Nonterminal::term(__nt) => {
                    let __sym4 = &mut Some(__nt);
                    __result = try!(__state118(input, __tokens, __lookahead, __sym0, __sym1, __sym2, __sym3, __sym4));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state87<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (6, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state119(input, __tokens, __sym4));
            }
            Some((__loc1, (8, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state120(input, __tokens, __sym4));
            }
            Some((__loc1, (11, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state121(input, __tokens, __sym4));
            }
            Some((__loc1, (12, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state122(input, __tokens, __sym4));
            }
            Some((__loc1, (13, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state123(input, __tokens, __sym4));
            }
            Some((__loc1, (15, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state124(input, __tokens, __sym4));
            }
            Some((__loc1, (16, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state125(input, __tokens, __sym4));
            }
            Some((__loc1, (17, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state126(input, __tokens, __sym4));
            }
            Some((__loc1, (18, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state127(input, __tokens, __sym4));
            }
            Some((__loc1, (19, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state128(input, __tokens, __sym4));
            }
            Some((__loc1, (20, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state129(input, __tokens, __sym4));
            }
            Some((__loc1, (21, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state130(input, __tokens, __sym4));
            }
            Some((__loc1, (22, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state131(input, __tokens, __sym4));
            }
            Some((__loc1, (23, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state132(input, __tokens, __sym4));
            }
            Some((__loc1, (24, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state133(input, __tokens, __sym4));
            }
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state23(input, __tokens, __sym4));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym3.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::exp(__nt) => {
                    let __sym4 = &mut Some(__nt);
                    __result = try!(__state116(input, __tokens, __lookahead, __sym4));
                }
                __Nonterminal::ident(__nt) => {
                    let __sym4 = &mut Some(__nt);
                    __result = try!(__state117(input, __tokens, __lookahead, __sym4));
                }
                __Nonterminal::term(__nt) => {
                    let __sym4 = &mut Some(__nt);
                    __result = try!(__state134(input, __tokens, __lookahead, __sym0, __sym1, __sym2, __sym3, __sym4));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state88<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (6, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state119(input, __tokens, __sym4));
            }
            Some((__loc1, (8, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state120(input, __tokens, __sym4));
            }
            Some((__loc1, (11, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state121(input, __tokens, __sym4));
            }
            Some((__loc1, (12, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state122(input, __tokens, __sym4));
            }
            Some((__loc1, (13, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state123(input, __tokens, __sym4));
            }
            Some((__loc1, (15, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state124(input, __tokens, __sym4));
            }
            Some((__loc1, (16, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state125(input, __tokens, __sym4));
            }
            Some((__loc1, (17, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state126(input, __tokens, __sym4));
            }
            Some((__loc1, (18, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state127(input, __tokens, __sym4));
            }
            Some((__loc1, (19, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state128(input, __tokens, __sym4));
            }
            Some((__loc1, (20, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state129(input, __tokens, __sym4));
            }
            Some((__loc1, (21, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state130(input, __tokens, __sym4));
            }
            Some((__loc1, (22, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state131(input, __tokens, __sym4));
            }
            Some((__loc1, (23, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state132(input, __tokens, __sym4));
            }
            Some((__loc1, (24, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state133(input, __tokens, __sym4));
            }
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state23(input, __tokens, __sym4));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym3.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::exp(__nt) => {
                    let __sym4 = &mut Some(__nt);
                    __result = try!(__state116(input, __tokens, __lookahead, __sym4));
                }
                __Nonterminal::ident(__nt) => {
                    let __sym4 = &mut Some(__nt);
                    __result = try!(__state117(input, __tokens, __lookahead, __sym4));
                }
                __Nonterminal::term(__nt) => {
                    let __sym4 = &mut Some(__nt);
                    __result = try!(__state135(input, __tokens, __lookahead, __sym0, __sym1, __sym2, __sym3, __sym4));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state89<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, i32, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            None |
            Some((_, (6, _), _)) |
            Some((_, (8, _), _)) |
            Some((_, (11, _), _)) |
            Some((_, (12, _), _)) |
            Some((_, (13, _), _)) |
            Some((_, (15, _), _)) |
            Some((_, (16, _), _)) |
            Some((_, (17, _), _)) |
            Some((_, (18, _), _)) |
            Some((_, (19, _), _)) |
            Some((_, (20, _), _)) |
            Some((_, (21, _), _)) |
            Some((_, (22, _), _)) |
            Some((_, (23, _), _)) |
            Some((_, (24, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __sym2 = __sym2.take().unwrap();
                let __sym3 = __sym3.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym3.2.clone();
                let __nt = super::__action25(input, __sym0, __sym1, __sym2, __sym3);
                let __nt = __Nonterminal::exp((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state90<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, String, usize)>,
        __sym3: &mut Option<(usize, IdOrImm, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (28, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state44(input, __tokens, __sym4));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym3.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::int(__nt) => {
                    let __sym4 = &mut Some(__nt);
                    __result = try!(__state136(input, __tokens, __lookahead, __sym0, __sym1, __sym2, __sym3, __sym4));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state91<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, String, usize)>,
        __sym1: &mut Option<(usize, &'input str, usize)>,
        __sym2: &mut Option<(usize, T, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, Exp, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (6, __tok0), __loc2)) => {
                let mut __sym5 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state7(input, __tokens, __sym5));
            }
            Some((__loc1, (8, __tok0), __loc2)) => {
                let mut __sym5 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state8(input, __tokens, __sym5));
            }
            Some((__loc1, (11, __tok0), __loc2)) => {
                let mut __sym5 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state10(input, __tokens, __sym5));
            }
            Some((__loc1, (12, __tok0), __loc2)) => {
                let mut __sym5 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state11(input, __tokens, __sym5));
            }
            Some((__loc1, (13, __tok0), __loc2)) => {
                let mut __sym5 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state12(input, __tokens, __sym5));
            }
            Some((__loc1, (15, __tok0), __loc2)) => {
                let mut __sym5 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state13(input, __tokens, __sym5));
            }
            Some((__loc1, (16, __tok0), __loc2)) => {
                let mut __sym5 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state14(input, __tokens, __sym5));
            }
            Some((__loc1, (17, __tok0), __loc2)) => {
                let mut __sym5 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state15(input, __tokens, __sym5));
            }
            Some((__loc1, (18, __tok0), __loc2)) => {
                let mut __sym5 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state16(input, __tokens, __sym5));
            }
            Some((__loc1, (19, __tok0), __loc2)) => {
                let mut __sym5 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state17(input, __tokens, __sym5));
            }
            Some((__loc1, (20, __tok0), __loc2)) => {
                let mut __sym5 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state18(input, __tokens, __sym5));
            }
            Some((__loc1, (21, __tok0), __loc2)) => {
                let mut __sym5 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state19(input, __tokens, __sym5));
            }
            Some((__loc1, (22, __tok0), __loc2)) => {
                let mut __sym5 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state20(input, __tokens, __sym5));
            }
            Some((__loc1, (23, __tok0), __loc2)) => {
                let mut __sym5 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state21(input, __tokens, __sym5));
            }
            Some((__loc1, (24, __tok0), __loc2)) => {
                let mut __sym5 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state22(input, __tokens, __sym5));
            }
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym5 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state23(input, __tokens, __sym5));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym4.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::exp(__nt) => {
                    let __sym5 = &mut Some(__nt);
                    __result = try!(__state2(input, __tokens, __lookahead, __sym5));
                }
                __Nonterminal::ident(__nt) => {
                    let __sym5 = &mut Some(__nt);
                    __result = try!(__state4(input, __tokens, __lookahead, __sym5));
                }
                __Nonterminal::term(__nt) => {
                    let __sym5 = &mut Some(__nt);
                    __result = try!(__state137(input, __tokens, __lookahead, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state92<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state29(input, __tokens, __sym1));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::ident(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state138(input, __tokens, __lookahead, __sym0, __sym1));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state93<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state31(input, __tokens, __sym1));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::ident(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state139(input, __tokens, __lookahead, __sym0, __sym1));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state94<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state29(input, __tokens, __sym1));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::ident(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state140(input, __tokens, __lookahead, __sym0, __sym1));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state95<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state29(input, __tokens, __sym1));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::ident(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state141(input, __tokens, __lookahead, __sym0, __sym1));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state96<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state29(input, __tokens, __sym1));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::ident(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state142(input, __tokens, __lookahead, __sym0, __sym1));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state97<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state29(input, __tokens, __sym1));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::ident(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state143(input, __tokens, __lookahead, __sym0, __sym1));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state98<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state145(input, __tokens, __sym1));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::ident(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state144(input, __tokens, __lookahead, __sym0, __sym1));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state99<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state145(input, __tokens, __sym1));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::ident(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state146(input, __tokens, __lookahead, __sym0, __sym1));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state100<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((_, (6, _), _)) |
            Some((_, (8, _), _)) |
            Some((_, (11, _), _)) |
            Some((_, (12, _), _)) |
            Some((_, (13, _), _)) |
            Some((_, (15, _), _)) |
            Some((_, (16, _), _)) |
            Some((_, (17, _), _)) |
            Some((_, (18, _), _)) |
            Some((_, (19, _), _)) |
            Some((_, (20, _), _)) |
            Some((_, (21, _), _)) |
            Some((_, (22, _), _)) |
            Some((_, (23, _), _)) |
            Some((_, (24, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action18(input, __sym0);
                let __nt = __Nonterminal::exp((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state101<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state145(input, __tokens, __sym1));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::ident(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state147(input, __tokens, __lookahead, __sym0, __sym1));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state102<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state42(input, __tokens, __sym1));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::ident(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state148(input, __tokens, __lookahead, __sym0, __sym1));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state103<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (28, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state150(input, __tokens, __sym1));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::int(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state149(input, __tokens, __lookahead, __sym0, __sym1));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state104<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state145(input, __tokens, __sym1));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::ident(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state151(input, __tokens, __lookahead, __sym0, __sym1));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state105<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state42(input, __tokens, __sym1));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::ident(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state152(input, __tokens, __lookahead, __sym0, __sym1));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state106<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state29(input, __tokens, __sym1));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::ident(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state153(input, __tokens, __lookahead, __sym0, __sym1));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state107<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, ::std::vec::Vec<T>, usize)>,
        __sym1: &mut Option<(usize, T, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (2, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state154(input, __tokens, __sym0, __sym1, __sym2));
            }
            Some((_, (1, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action70(input, __sym0, __sym1);
                let __nt = __Nonterminal::tys((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state108<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, T, usize)>,
        __sym1: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((_, (0, _), _)) |
            Some((_, (1, _), _)) |
            Some((_, (7, _), _)) |
            Some((_, (14, _), _)) |
            Some((_, (25, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action60(input, __sym0, __sym1);
                let __nt = __Nonterminal::_28_3cty_3e_20_22_2c_22_29_2b((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state109<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, Vec<T>, usize)>,
        __sym2: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (3, __tok0), __loc2)) => {
                let mut __sym3 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state155(input, __tokens, __sym0, __sym1, __sym2, __sym3));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state110<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, Vec<T>, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (1, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state156(input, __tokens, __sym0, __sym1, __sym2));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state111<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, ::std::vec::Vec<String>, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (2, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state157(input, __tokens, __sym0, __sym1, __sym2));
            }
            Some((_, (1, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action66(input, __sym0, __sym1);
                let __nt = __Nonterminal::args((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state112<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, &'input str, usize)>,
        __sym3: &mut Option<(usize, Vec<String>, usize)>,
        __sym4: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            None |
            Some((_, (6, _), _)) |
            Some((_, (8, _), _)) |
            Some((_, (11, _), _)) |
            Some((_, (12, _), _)) |
            Some((_, (13, _), _)) |
            Some((_, (15, _), _)) |
            Some((_, (16, _), _)) |
            Some((_, (17, _), _)) |
            Some((_, (18, _), _)) |
            Some((_, (19, _), _)) |
            Some((_, (20, _), _)) |
            Some((_, (21, _), _)) |
            Some((_, (22, _), _)) |
            Some((_, (23, _), _)) |
            Some((_, (24, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __sym2 = __sym2.take().unwrap();
                let __sym3 = __sym3.take().unwrap();
                let __sym4 = __sym4.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym4.2.clone();
                let __nt = super::__action30(input, __sym0, __sym1, __sym2, __sym3, __sym4);
                let __nt = __Nonterminal::exp((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state113<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, String, usize)>,
        __sym1: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((_, (1, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action56(input, __sym0, __sym1);
                let __nt = __Nonterminal::_28_3cid__ty_3e_20_22_2c_22_29_2b((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state114<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, String, usize)>,
        __sym1: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (0, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state77(input, __tokens, __sym2));
            }
            Some((__loc1, (7, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state78(input, __tokens, __sym2));
            }
            Some((__loc1, (14, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state79(input, __tokens, __sym2));
            }
            Some((__loc1, (25, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state80(input, __tokens, __sym2));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym1.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::ty(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state158(input, __tokens, __lookahead, __sym0, __sym1, __sym2));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state115<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, &'input str, usize)>,
        __sym3: &mut Option<(usize, Vec<String>, usize)>,
        __sym4: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (4, __tok0), __loc2)) => {
                let mut __sym5 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state159(input, __tokens, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state116<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, Exp, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (6, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state119(input, __tokens, __sym1));
            }
            Some((__loc1, (8, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state120(input, __tokens, __sym1));
            }
            Some((__loc1, (11, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state121(input, __tokens, __sym1));
            }
            Some((__loc1, (12, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state122(input, __tokens, __sym1));
            }
            Some((__loc1, (13, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state123(input, __tokens, __sym1));
            }
            Some((__loc1, (15, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state124(input, __tokens, __sym1));
            }
            Some((__loc1, (16, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state125(input, __tokens, __sym1));
            }
            Some((__loc1, (17, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state126(input, __tokens, __sym1));
            }
            Some((__loc1, (18, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state127(input, __tokens, __sym1));
            }
            Some((__loc1, (19, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state128(input, __tokens, __sym1));
            }
            Some((__loc1, (20, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state129(input, __tokens, __sym1));
            }
            Some((__loc1, (21, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state130(input, __tokens, __sym1));
            }
            Some((__loc1, (22, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state131(input, __tokens, __sym1));
            }
            Some((__loc1, (23, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state132(input, __tokens, __sym1));
            }
            Some((__loc1, (24, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state133(input, __tokens, __sym1));
            }
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state23(input, __tokens, __sym1));
            }
            Some((_, (27, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action16(input, __sym0);
                let __nt = __Nonterminal::term((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::exp(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state116(input, __tokens, __lookahead, __sym1));
                }
                __Nonterminal::ident(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state117(input, __tokens, __lookahead, __sym1));
                }
                __Nonterminal::term(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state160(input, __tokens, __lookahead, __sym0, __sym1));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state117<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, String, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (4, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state161(input, __tokens, __sym0, __sym1));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state118<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, E, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (27, __tok0), __loc2)) => {
                let mut __sym5 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state162(input, __tokens, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state119<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state29(input, __tokens, __sym1));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::ident(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state163(input, __tokens, __lookahead, __sym0, __sym1));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state120<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state31(input, __tokens, __sym1));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::ident(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state164(input, __tokens, __lookahead, __sym0, __sym1));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state121<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state29(input, __tokens, __sym1));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::ident(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state165(input, __tokens, __lookahead, __sym0, __sym1));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state122<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state29(input, __tokens, __sym1));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::ident(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state166(input, __tokens, __lookahead, __sym0, __sym1));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state123<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state29(input, __tokens, __sym1));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::ident(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state167(input, __tokens, __lookahead, __sym0, __sym1));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state124<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state29(input, __tokens, __sym1));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::ident(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state168(input, __tokens, __lookahead, __sym0, __sym1));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state125<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state170(input, __tokens, __sym1));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::ident(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state169(input, __tokens, __lookahead, __sym0, __sym1));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state126<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state170(input, __tokens, __sym1));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::ident(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state171(input, __tokens, __lookahead, __sym0, __sym1));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state127<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((_, (6, _), _)) |
            Some((_, (8, _), _)) |
            Some((_, (11, _), _)) |
            Some((_, (12, _), _)) |
            Some((_, (13, _), _)) |
            Some((_, (15, _), _)) |
            Some((_, (16, _), _)) |
            Some((_, (17, _), _)) |
            Some((_, (18, _), _)) |
            Some((_, (19, _), _)) |
            Some((_, (20, _), _)) |
            Some((_, (21, _), _)) |
            Some((_, (22, _), _)) |
            Some((_, (23, _), _)) |
            Some((_, (24, _), _)) |
            Some((_, (27, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action18(input, __sym0);
                let __nt = __Nonterminal::exp((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state128<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state170(input, __tokens, __sym1));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::ident(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state172(input, __tokens, __lookahead, __sym0, __sym1));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state129<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state42(input, __tokens, __sym1));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::ident(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state173(input, __tokens, __lookahead, __sym0, __sym1));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state130<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (28, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state175(input, __tokens, __sym1));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::int(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state174(input, __tokens, __lookahead, __sym0, __sym1));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state131<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state170(input, __tokens, __sym1));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::ident(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state176(input, __tokens, __lookahead, __sym0, __sym1));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state132<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state42(input, __tokens, __sym1));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::ident(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state177(input, __tokens, __lookahead, __sym0, __sym1));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state133<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state29(input, __tokens, __sym1));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::ident(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state178(input, __tokens, __lookahead, __sym0, __sym1));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state134<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, E, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (27, __tok0), __loc2)) => {
                let mut __sym5 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state179(input, __tokens, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state135<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, E, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (27, __tok0), __loc2)) => {
                let mut __sym5 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state180(input, __tokens, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state136<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, String, usize)>,
        __sym3: &mut Option<(usize, IdOrImm, usize)>,
        __sym4: &mut Option<(usize, i32, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            None |
            Some((_, (6, _), _)) |
            Some((_, (8, _), _)) |
            Some((_, (11, _), _)) |
            Some((_, (12, _), _)) |
            Some((_, (13, _), _)) |
            Some((_, (15, _), _)) |
            Some((_, (16, _), _)) |
            Some((_, (17, _), _)) |
            Some((_, (18, _), _)) |
            Some((_, (19, _), _)) |
            Some((_, (20, _), _)) |
            Some((_, (21, _), _)) |
            Some((_, (22, _), _)) |
            Some((_, (23, _), _)) |
            Some((_, (24, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __sym2 = __sym2.take().unwrap();
                let __sym3 = __sym3.take().unwrap();
                let __sym4 = __sym4.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym4.2.clone();
                let __nt = super::__action26(input, __sym0, __sym1, __sym2, __sym3, __sym4);
                let __nt = __Nonterminal::exp((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state137<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, String, usize)>,
        __sym1: &mut Option<(usize, &'input str, usize)>,
        __sym2: &mut Option<(usize, T, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, Exp, usize)>,
        __sym5: &mut Option<(usize, E, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            None => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __sym2 = __sym2.take().unwrap();
                let __sym3 = __sym3.take().unwrap();
                let __sym4 = __sym4.take().unwrap();
                let __sym5 = __sym5.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym5.2.clone();
                let __nt = super::__action15(input, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5);
                let __nt = __Nonterminal::term((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state138<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (28, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state150(input, __tokens, __sym2));
            }
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state145(input, __tokens, __sym2));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym1.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::id__or__imm(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state181(input, __tokens, __lookahead, __sym0, __sym1, __sym2));
                }
                __Nonterminal::ident(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state182(input, __tokens, __lookahead, __sym2));
                }
                __Nonterminal::int(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state183(input, __tokens, __lookahead, __sym2));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state139<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (0, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state184(input, __tokens, __sym0, __sym1, __sym2));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state140<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (28, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state61(input, __tokens, __sym2));
            }
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state62(input, __tokens, __sym2));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym1.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::id__or__imm(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state185(input, __tokens, __lookahead, __sym0, __sym1, __sym2));
                }
                __Nonterminal::ident(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state59(input, __tokens, __lookahead, __sym2));
                }
                __Nonterminal::int(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state60(input, __tokens, __lookahead, __sym2));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state141<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (28, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state61(input, __tokens, __sym2));
            }
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state62(input, __tokens, __sym2));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym1.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::id__or__imm(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state186(input, __tokens, __lookahead, __sym0, __sym1, __sym2));
                }
                __Nonterminal::ident(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state59(input, __tokens, __lookahead, __sym2));
                }
                __Nonterminal::int(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state60(input, __tokens, __lookahead, __sym2));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state142<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (28, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state61(input, __tokens, __sym2));
            }
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state62(input, __tokens, __sym2));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym1.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::id__or__imm(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state187(input, __tokens, __lookahead, __sym0, __sym1, __sym2));
                }
                __Nonterminal::ident(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state59(input, __tokens, __lookahead, __sym2));
                }
                __Nonterminal::int(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state60(input, __tokens, __lookahead, __sym2));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state143<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (28, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state68(input, __tokens, __sym2));
            }
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state69(input, __tokens, __sym2));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym1.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::id__or__imm(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state188(input, __tokens, __lookahead, __sym0, __sym1, __sym2));
                }
                __Nonterminal::ident(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state66(input, __tokens, __lookahead, __sym2));
                }
                __Nonterminal::int(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state67(input, __tokens, __lookahead, __sym2));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state144<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((_, (6, _), _)) |
            Some((_, (8, _), _)) |
            Some((_, (11, _), _)) |
            Some((_, (12, _), _)) |
            Some((_, (13, _), _)) |
            Some((_, (15, _), _)) |
            Some((_, (16, _), _)) |
            Some((_, (17, _), _)) |
            Some((_, (18, _), _)) |
            Some((_, (19, _), _)) |
            Some((_, (20, _), _)) |
            Some((_, (21, _), _)) |
            Some((_, (22, _), _)) |
            Some((_, (23, _), _)) |
            Some((_, (24, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action21(input, __sym0, __sym1);
                let __nt = __Nonterminal::exp((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state145<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((_, (6, _), _)) |
            Some((_, (8, _), _)) |
            Some((_, (11, _), _)) |
            Some((_, (12, _), _)) |
            Some((_, (13, _), _)) |
            Some((_, (15, _), _)) |
            Some((_, (16, _), _)) |
            Some((_, (17, _), _)) |
            Some((_, (18, _), _)) |
            Some((_, (19, _), _)) |
            Some((_, (20, _), _)) |
            Some((_, (21, _), _)) |
            Some((_, (22, _), _)) |
            Some((_, (23, _), _)) |
            Some((_, (24, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action3(input, __sym0);
                let __nt = __Nonterminal::ident((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state146<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((_, (6, _), _)) |
            Some((_, (8, _), _)) |
            Some((_, (11, _), _)) |
            Some((_, (12, _), _)) |
            Some((_, (13, _), _)) |
            Some((_, (15, _), _)) |
            Some((_, (16, _), _)) |
            Some((_, (17, _), _)) |
            Some((_, (18, _), _)) |
            Some((_, (19, _), _)) |
            Some((_, (20, _), _)) |
            Some((_, (21, _), _)) |
            Some((_, (22, _), _)) |
            Some((_, (23, _), _)) |
            Some((_, (24, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action22(input, __sym0, __sym1);
                let __nt = __Nonterminal::exp((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state147<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((_, (6, _), _)) |
            Some((_, (8, _), _)) |
            Some((_, (11, _), _)) |
            Some((_, (12, _), _)) |
            Some((_, (13, _), _)) |
            Some((_, (15, _), _)) |
            Some((_, (16, _), _)) |
            Some((_, (17, _), _)) |
            Some((_, (18, _), _)) |
            Some((_, (19, _), _)) |
            Some((_, (20, _), _)) |
            Some((_, (21, _), _)) |
            Some((_, (22, _), _)) |
            Some((_, (23, _), _)) |
            Some((_, (24, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action32(input, __sym0, __sym1);
                let __nt = __Nonterminal::exp((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state148<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state145(input, __tokens, __sym2));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym1.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::ident(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state189(input, __tokens, __lookahead, __sym0, __sym1, __sym2));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state149<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, i32, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((_, (6, _), _)) |
            Some((_, (8, _), _)) |
            Some((_, (11, _), _)) |
            Some((_, (12, _), _)) |
            Some((_, (13, _), _)) |
            Some((_, (15, _), _)) |
            Some((_, (16, _), _)) |
            Some((_, (17, _), _)) |
            Some((_, (18, _), _)) |
            Some((_, (19, _), _)) |
            Some((_, (20, _), _)) |
            Some((_, (21, _), _)) |
            Some((_, (22, _), _)) |
            Some((_, (23, _), _)) |
            Some((_, (24, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action19(input, __sym0, __sym1);
                let __nt = __Nonterminal::exp((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state150<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((_, (6, _), _)) |
            Some((_, (8, _), _)) |
            Some((_, (11, _), _)) |
            Some((_, (12, _), _)) |
            Some((_, (13, _), _)) |
            Some((_, (15, _), _)) |
            Some((_, (16, _), _)) |
            Some((_, (17, _), _)) |
            Some((_, (18, _), _)) |
            Some((_, (19, _), _)) |
            Some((_, (20, _), _)) |
            Some((_, (21, _), _)) |
            Some((_, (22, _), _)) |
            Some((_, (23, _), _)) |
            Some((_, (24, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action2(input, __sym0);
                let __nt = __Nonterminal::int((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state151<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((_, (6, _), _)) |
            Some((_, (8, _), _)) |
            Some((_, (11, _), _)) |
            Some((_, (12, _), _)) |
            Some((_, (13, _), _)) |
            Some((_, (15, _), _)) |
            Some((_, (16, _), _)) |
            Some((_, (17, _), _)) |
            Some((_, (18, _), _)) |
            Some((_, (19, _), _)) |
            Some((_, (20, _), _)) |
            Some((_, (21, _), _)) |
            Some((_, (22, _), _)) |
            Some((_, (23, _), _)) |
            Some((_, (24, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action20(input, __sym0, __sym1);
                let __nt = __Nonterminal::exp((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state152<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state29(input, __tokens, __sym2));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym1.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::ident(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state190(input, __tokens, __lookahead, __sym0, __sym1, __sym2));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state153<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (28, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state150(input, __tokens, __sym2));
            }
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state145(input, __tokens, __sym2));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym1.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::id__or__imm(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state191(input, __tokens, __lookahead, __sym0, __sym1, __sym2));
                }
                __Nonterminal::ident(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state182(input, __tokens, __lookahead, __sym2));
                }
                __Nonterminal::int(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state183(input, __tokens, __lookahead, __sym2));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state154<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, ::std::vec::Vec<T>, usize)>,
        __sym1: &mut Option<(usize, T, usize)>,
        __sym2: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((_, (0, _), _)) |
            Some((_, (1, _), _)) |
            Some((_, (7, _), _)) |
            Some((_, (14, _), _)) |
            Some((_, (25, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __sym2 = __sym2.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym2.2.clone();
                let __nt = super::__action61(input, __sym0, __sym1, __sym2);
                let __nt = __Nonterminal::_28_3cty_3e_20_22_2c_22_29_2b((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state155<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, Vec<T>, usize)>,
        __sym2: &mut Option<(usize, &'input str, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (0, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state49(input, __tokens, __sym4));
            }
            Some((__loc1, (7, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state50(input, __tokens, __sym4));
            }
            Some((__loc1, (14, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state51(input, __tokens, __sym4));
            }
            Some((__loc1, (25, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state52(input, __tokens, __sym4));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym3.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::ty(__nt) => {
                    let __sym4 = &mut Some(__nt);
                    __result = try!(__state192(input, __tokens, __lookahead, __sym0, __sym1, __sym2, __sym3, __sym4));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state156<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, Vec<T>, usize)>,
        __sym2: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (3, __tok0), __loc2)) => {
                let mut __sym3 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state193(input, __tokens, __sym0, __sym1, __sym2, __sym3));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state157<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, ::std::vec::Vec<String>, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((_, (1, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __sym2 = __sym2.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym2.2.clone();
                let __nt = super::__action57(input, __sym0, __sym1, __sym2);
                let __nt = __Nonterminal::_28_3cid__ty_3e_20_22_2c_22_29_2b((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state158<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, String, usize)>,
        __sym1: &mut Option<(usize, &'input str, usize)>,
        __sym2: &mut Option<(usize, T, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((_, (1, _), _)) |
            Some((_, (2, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __sym2 = __sym2.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym2.2.clone();
                let __nt = super::__action7(input, __sym0, __sym1, __sym2);
                let __nt = __Nonterminal::id__ty((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state159<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, &'input str, usize)>,
        __sym3: &mut Option<(usize, Vec<String>, usize)>,
        __sym4: &mut Option<(usize, &'input str, usize)>,
        __sym5: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (0, __tok0), __loc2)) => {
                let mut __sym6 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state195(input, __tokens, __sym6));
            }
            Some((__loc1, (7, __tok0), __loc2)) => {
                let mut __sym6 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state196(input, __tokens, __sym6));
            }
            Some((__loc1, (14, __tok0), __loc2)) => {
                let mut __sym6 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state197(input, __tokens, __sym6));
            }
            Some((__loc1, (25, __tok0), __loc2)) => {
                let mut __sym6 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state198(input, __tokens, __sym6));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym5.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::ty(__nt) => {
                    let __sym6 = &mut Some(__nt);
                    __result = try!(__state194(input, __tokens, __lookahead, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5, __sym6));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state160<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, Exp, usize)>,
        __sym1: &mut Option<(usize, E, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((_, (27, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action17(input, __sym0, __sym1);
                let __nt = __Nonterminal::term((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state161<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, String, usize)>,
        __sym1: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (0, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state49(input, __tokens, __sym2));
            }
            Some((__loc1, (7, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state50(input, __tokens, __sym2));
            }
            Some((__loc1, (14, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state51(input, __tokens, __sym2));
            }
            Some((__loc1, (25, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state52(input, __tokens, __sym2));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym1.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::ty(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state199(input, __tokens, __lookahead, __sym0, __sym1, __sym2));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state162<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, E, usize)>,
        __sym5: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (10, __tok0), __loc2)) => {
                let mut __sym6 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state200(input, __tokens, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5, __sym6));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state163<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (28, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state175(input, __tokens, __sym2));
            }
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state170(input, __tokens, __sym2));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym1.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::id__or__imm(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state201(input, __tokens, __lookahead, __sym0, __sym1, __sym2));
                }
                __Nonterminal::ident(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state202(input, __tokens, __lookahead, __sym2));
                }
                __Nonterminal::int(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state203(input, __tokens, __lookahead, __sym2));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state164<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (0, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state204(input, __tokens, __sym0, __sym1, __sym2));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state165<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (28, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state61(input, __tokens, __sym2));
            }
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state62(input, __tokens, __sym2));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym1.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::id__or__imm(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state205(input, __tokens, __lookahead, __sym0, __sym1, __sym2));
                }
                __Nonterminal::ident(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state59(input, __tokens, __lookahead, __sym2));
                }
                __Nonterminal::int(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state60(input, __tokens, __lookahead, __sym2));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state166<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (28, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state61(input, __tokens, __sym2));
            }
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state62(input, __tokens, __sym2));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym1.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::id__or__imm(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state206(input, __tokens, __lookahead, __sym0, __sym1, __sym2));
                }
                __Nonterminal::ident(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state59(input, __tokens, __lookahead, __sym2));
                }
                __Nonterminal::int(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state60(input, __tokens, __lookahead, __sym2));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state167<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (28, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state61(input, __tokens, __sym2));
            }
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state62(input, __tokens, __sym2));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym1.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::id__or__imm(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state207(input, __tokens, __lookahead, __sym0, __sym1, __sym2));
                }
                __Nonterminal::ident(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state59(input, __tokens, __lookahead, __sym2));
                }
                __Nonterminal::int(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state60(input, __tokens, __lookahead, __sym2));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state168<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (28, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state68(input, __tokens, __sym2));
            }
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state69(input, __tokens, __sym2));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym1.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::id__or__imm(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state208(input, __tokens, __lookahead, __sym0, __sym1, __sym2));
                }
                __Nonterminal::ident(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state66(input, __tokens, __lookahead, __sym2));
                }
                __Nonterminal::int(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state67(input, __tokens, __lookahead, __sym2));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state169<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((_, (6, _), _)) |
            Some((_, (8, _), _)) |
            Some((_, (11, _), _)) |
            Some((_, (12, _), _)) |
            Some((_, (13, _), _)) |
            Some((_, (15, _), _)) |
            Some((_, (16, _), _)) |
            Some((_, (17, _), _)) |
            Some((_, (18, _), _)) |
            Some((_, (19, _), _)) |
            Some((_, (20, _), _)) |
            Some((_, (21, _), _)) |
            Some((_, (22, _), _)) |
            Some((_, (23, _), _)) |
            Some((_, (24, _), _)) |
            Some((_, (27, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action21(input, __sym0, __sym1);
                let __nt = __Nonterminal::exp((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state170<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((_, (6, _), _)) |
            Some((_, (8, _), _)) |
            Some((_, (11, _), _)) |
            Some((_, (12, _), _)) |
            Some((_, (13, _), _)) |
            Some((_, (15, _), _)) |
            Some((_, (16, _), _)) |
            Some((_, (17, _), _)) |
            Some((_, (18, _), _)) |
            Some((_, (19, _), _)) |
            Some((_, (20, _), _)) |
            Some((_, (21, _), _)) |
            Some((_, (22, _), _)) |
            Some((_, (23, _), _)) |
            Some((_, (24, _), _)) |
            Some((_, (27, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action3(input, __sym0);
                let __nt = __Nonterminal::ident((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state171<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((_, (6, _), _)) |
            Some((_, (8, _), _)) |
            Some((_, (11, _), _)) |
            Some((_, (12, _), _)) |
            Some((_, (13, _), _)) |
            Some((_, (15, _), _)) |
            Some((_, (16, _), _)) |
            Some((_, (17, _), _)) |
            Some((_, (18, _), _)) |
            Some((_, (19, _), _)) |
            Some((_, (20, _), _)) |
            Some((_, (21, _), _)) |
            Some((_, (22, _), _)) |
            Some((_, (23, _), _)) |
            Some((_, (24, _), _)) |
            Some((_, (27, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action22(input, __sym0, __sym1);
                let __nt = __Nonterminal::exp((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state172<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((_, (6, _), _)) |
            Some((_, (8, _), _)) |
            Some((_, (11, _), _)) |
            Some((_, (12, _), _)) |
            Some((_, (13, _), _)) |
            Some((_, (15, _), _)) |
            Some((_, (16, _), _)) |
            Some((_, (17, _), _)) |
            Some((_, (18, _), _)) |
            Some((_, (19, _), _)) |
            Some((_, (20, _), _)) |
            Some((_, (21, _), _)) |
            Some((_, (22, _), _)) |
            Some((_, (23, _), _)) |
            Some((_, (24, _), _)) |
            Some((_, (27, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action32(input, __sym0, __sym1);
                let __nt = __Nonterminal::exp((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state173<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state170(input, __tokens, __sym2));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym1.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::ident(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state209(input, __tokens, __lookahead, __sym0, __sym1, __sym2));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state174<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, i32, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((_, (6, _), _)) |
            Some((_, (8, _), _)) |
            Some((_, (11, _), _)) |
            Some((_, (12, _), _)) |
            Some((_, (13, _), _)) |
            Some((_, (15, _), _)) |
            Some((_, (16, _), _)) |
            Some((_, (17, _), _)) |
            Some((_, (18, _), _)) |
            Some((_, (19, _), _)) |
            Some((_, (20, _), _)) |
            Some((_, (21, _), _)) |
            Some((_, (22, _), _)) |
            Some((_, (23, _), _)) |
            Some((_, (24, _), _)) |
            Some((_, (27, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action19(input, __sym0, __sym1);
                let __nt = __Nonterminal::exp((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state175<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((_, (6, _), _)) |
            Some((_, (8, _), _)) |
            Some((_, (11, _), _)) |
            Some((_, (12, _), _)) |
            Some((_, (13, _), _)) |
            Some((_, (15, _), _)) |
            Some((_, (16, _), _)) |
            Some((_, (17, _), _)) |
            Some((_, (18, _), _)) |
            Some((_, (19, _), _)) |
            Some((_, (20, _), _)) |
            Some((_, (21, _), _)) |
            Some((_, (22, _), _)) |
            Some((_, (23, _), _)) |
            Some((_, (24, _), _)) |
            Some((_, (27, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action2(input, __sym0);
                let __nt = __Nonterminal::int((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state176<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((_, (6, _), _)) |
            Some((_, (8, _), _)) |
            Some((_, (11, _), _)) |
            Some((_, (12, _), _)) |
            Some((_, (13, _), _)) |
            Some((_, (15, _), _)) |
            Some((_, (16, _), _)) |
            Some((_, (17, _), _)) |
            Some((_, (18, _), _)) |
            Some((_, (19, _), _)) |
            Some((_, (20, _), _)) |
            Some((_, (21, _), _)) |
            Some((_, (22, _), _)) |
            Some((_, (23, _), _)) |
            Some((_, (24, _), _)) |
            Some((_, (27, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action20(input, __sym0, __sym1);
                let __nt = __Nonterminal::exp((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state177<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state29(input, __tokens, __sym2));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym1.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::ident(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state210(input, __tokens, __lookahead, __sym0, __sym1, __sym2));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state178<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (28, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state175(input, __tokens, __sym2));
            }
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state170(input, __tokens, __sym2));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym1.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::id__or__imm(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state211(input, __tokens, __lookahead, __sym0, __sym1, __sym2));
                }
                __Nonterminal::ident(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state202(input, __tokens, __lookahead, __sym2));
                }
                __Nonterminal::int(__nt) => {
                    let __sym2 = &mut Some(__nt);
                    __result = try!(__state203(input, __tokens, __lookahead, __sym2));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state179<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, E, usize)>,
        __sym5: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (10, __tok0), __loc2)) => {
                let mut __sym6 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state212(input, __tokens, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5, __sym6));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state180<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, E, usize)>,
        __sym5: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (10, __tok0), __loc2)) => {
                let mut __sym6 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state213(input, __tokens, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5, __sym6));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state181<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((_, (6, _), _)) |
            Some((_, (8, _), _)) |
            Some((_, (11, _), _)) |
            Some((_, (12, _), _)) |
            Some((_, (13, _), _)) |
            Some((_, (15, _), _)) |
            Some((_, (16, _), _)) |
            Some((_, (17, _), _)) |
            Some((_, (18, _), _)) |
            Some((_, (19, _), _)) |
            Some((_, (20, _), _)) |
            Some((_, (21, _), _)) |
            Some((_, (22, _), _)) |
            Some((_, (23, _), _)) |
            Some((_, (24, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __sym2 = __sym2.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym2.2.clone();
                let __nt = super::__action23(input, __sym0, __sym1, __sym2);
                let __nt = __Nonterminal::exp((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state182<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, String, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((_, (6, _), _)) |
            Some((_, (8, _), _)) |
            Some((_, (11, _), _)) |
            Some((_, (12, _), _)) |
            Some((_, (13, _), _)) |
            Some((_, (15, _), _)) |
            Some((_, (16, _), _)) |
            Some((_, (17, _), _)) |
            Some((_, (18, _), _)) |
            Some((_, (19, _), _)) |
            Some((_, (20, _), _)) |
            Some((_, (21, _), _)) |
            Some((_, (22, _), _)) |
            Some((_, (23, _), _)) |
            Some((_, (24, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action13(input, __sym0);
                let __nt = __Nonterminal::id__or__imm((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state183<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, i32, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((_, (6, _), _)) |
            Some((_, (8, _), _)) |
            Some((_, (11, _), _)) |
            Some((_, (12, _), _)) |
            Some((_, (13, _), _)) |
            Some((_, (15, _), _)) |
            Some((_, (16, _), _)) |
            Some((_, (17, _), _)) |
            Some((_, (18, _), _)) |
            Some((_, (19, _), _)) |
            Some((_, (20, _), _)) |
            Some((_, (21, _), _)) |
            Some((_, (22, _), _)) |
            Some((_, (23, _), _)) |
            Some((_, (24, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action14(input, __sym0);
                let __nt = __Nonterminal::id__or__imm((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state184<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym3 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state23(input, __tokens, __sym3));
            }
            Some((_, (1, _), _)) => {
                let __start = __sym2.as_ref().unwrap().2.clone();
                let __end = __lookahead.as_ref().map(|o| o.0.clone()).unwrap_or_else(|| __start.clone());
                let __nt = super::__action65(input, &__start, &__end);
                let __nt = __Nonterminal::args((
                    __start,
                    __nt,
                    __end,
                ));
                __result = (__lookahead, __nt);
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym2.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::_28_3cid__ty_3e_20_22_2c_22_29_2b(__nt) => {
                    let __sym3 = &mut Some(__nt);
                    __result = try!(__state81(input, __tokens, __lookahead, __sym3));
                }
                __Nonterminal::args(__nt) => {
                    let __sym3 = &mut Some(__nt);
                    __result = try!(__state214(input, __tokens, __lookahead, __sym0, __sym1, __sym2, __sym3));
                }
                __Nonterminal::id__ty(__nt) => {
                    let __sym3 = &mut Some(__nt);
                    __result = try!(__state83(input, __tokens, __lookahead, __sym3));
                }
                __Nonterminal::ident(__nt) => {
                    let __sym3 = &mut Some(__nt);
                    __result = try!(__state84(input, __tokens, __lookahead, __sym3));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state185<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (26, __tok0), __loc2)) => {
                let mut __sym3 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state215(input, __tokens, __sym0, __sym1, __sym2, __sym3));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state186<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (26, __tok0), __loc2)) => {
                let mut __sym3 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state216(input, __tokens, __sym0, __sym1, __sym2, __sym3));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state187<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (26, __tok0), __loc2)) => {
                let mut __sym3 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state217(input, __tokens, __sym0, __sym1, __sym2, __sym3));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state188<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (28, __tok0), __loc2)) => {
                let mut __sym3 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state150(input, __tokens, __sym3));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym2.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::int(__nt) => {
                    let __sym3 = &mut Some(__nt);
                    __result = try!(__state218(input, __tokens, __lookahead, __sym0, __sym1, __sym2, __sym3));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state189<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, String, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((_, (6, _), _)) |
            Some((_, (8, _), _)) |
            Some((_, (11, _), _)) |
            Some((_, (12, _), _)) |
            Some((_, (13, _), _)) |
            Some((_, (15, _), _)) |
            Some((_, (16, _), _)) |
            Some((_, (17, _), _)) |
            Some((_, (18, _), _)) |
            Some((_, (19, _), _)) |
            Some((_, (20, _), _)) |
            Some((_, (21, _), _)) |
            Some((_, (22, _), _)) |
            Some((_, (23, _), _)) |
            Some((_, (24, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __sym2 = __sym2.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym2.2.clone();
                let __nt = super::__action31(input, __sym0, __sym1, __sym2);
                let __nt = __Nonterminal::exp((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state190<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, String, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (28, __tok0), __loc2)) => {
                let mut __sym3 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state68(input, __tokens, __sym3));
            }
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym3 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state69(input, __tokens, __sym3));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym2.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::id__or__imm(__nt) => {
                    let __sym3 = &mut Some(__nt);
                    __result = try!(__state219(input, __tokens, __lookahead, __sym0, __sym1, __sym2, __sym3));
                }
                __Nonterminal::ident(__nt) => {
                    let __sym3 = &mut Some(__nt);
                    __result = try!(__state66(input, __tokens, __lookahead, __sym3));
                }
                __Nonterminal::int(__nt) => {
                    let __sym3 = &mut Some(__nt);
                    __result = try!(__state67(input, __tokens, __lookahead, __sym3));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state191<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((_, (6, _), _)) |
            Some((_, (8, _), _)) |
            Some((_, (11, _), _)) |
            Some((_, (12, _), _)) |
            Some((_, (13, _), _)) |
            Some((_, (15, _), _)) |
            Some((_, (16, _), _)) |
            Some((_, (17, _), _)) |
            Some((_, (18, _), _)) |
            Some((_, (19, _), _)) |
            Some((_, (20, _), _)) |
            Some((_, (21, _), _)) |
            Some((_, (22, _), _)) |
            Some((_, (23, _), _)) |
            Some((_, (24, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __sym2 = __sym2.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym2.2.clone();
                let __nt = super::__action24(input, __sym0, __sym1, __sym2);
                let __nt = __Nonterminal::exp((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state192<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, Vec<T>, usize)>,
        __sym2: &mut Option<(usize, &'input str, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, T, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((_, (5, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __sym2 = __sym2.take().unwrap();
                let __sym3 = __sym3.take().unwrap();
                let __sym4 = __sym4.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym4.2.clone();
                let __nt = super::__action11(input, __sym0, __sym1, __sym2, __sym3, __sym4);
                let __nt = __Nonterminal::ty((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state193<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, Vec<T>, usize)>,
        __sym2: &mut Option<(usize, &'input str, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (0, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state77(input, __tokens, __sym4));
            }
            Some((__loc1, (7, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state78(input, __tokens, __sym4));
            }
            Some((__loc1, (14, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state79(input, __tokens, __sym4));
            }
            Some((__loc1, (25, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state80(input, __tokens, __sym4));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym3.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::ty(__nt) => {
                    let __sym4 = &mut Some(__nt);
                    __result = try!(__state220(input, __tokens, __lookahead, __sym0, __sym1, __sym2, __sym3, __sym4));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state194<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, &'input str, usize)>,
        __sym3: &mut Option<(usize, Vec<String>, usize)>,
        __sym4: &mut Option<(usize, &'input str, usize)>,
        __sym5: &mut Option<(usize, &'input str, usize)>,
        __sym6: &mut Option<(usize, T, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (26, __tok0), __loc2)) => {
                let mut __sym7 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state221(input, __tokens, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5, __sym6, __sym7));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state195<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (0, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state77(input, __tokens, __sym1));
            }
            Some((__loc1, (7, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state78(input, __tokens, __sym1));
            }
            Some((__loc1, (14, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state79(input, __tokens, __sym1));
            }
            Some((__loc1, (25, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state80(input, __tokens, __sym1));
            }
            Some((_, (1, _), _)) => {
                let __start = __sym0.as_ref().unwrap().2.clone();
                let __end = __lookahead.as_ref().map(|o| o.0.clone()).unwrap_or_else(|| __start.clone());
                let __nt = super::__action69(input, &__start, &__end);
                let __nt = __Nonterminal::tys((
                    __start,
                    __nt,
                    __end,
                ));
                __result = (__lookahead, __nt);
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::_28_3cty_3e_20_22_2c_22_29_2b(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state74(input, __tokens, __lookahead, __sym1));
                }
                __Nonterminal::ty(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state75(input, __tokens, __lookahead, __sym1));
                }
                __Nonterminal::tys(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state222(input, __tokens, __lookahead, __sym0, __sym1));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state196<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((_, (26, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action9(input, __sym0);
                let __nt = __Nonterminal::ty((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state197<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((_, (26, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action10(input, __sym0);
                let __nt = __Nonterminal::ty((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state198<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((_, (26, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action8(input, __sym0);
                let __nt = __Nonterminal::ty((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state199<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, String, usize)>,
        __sym1: &mut Option<(usize, &'input str, usize)>,
        __sym2: &mut Option<(usize, T, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (5, __tok0), __loc2)) => {
                let mut __sym3 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state223(input, __tokens, __sym0, __sym1, __sym2, __sym3));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state200<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, E, usize)>,
        __sym5: &mut Option<(usize, &'input str, usize)>,
        __sym6: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (26, __tok0), __loc2)) => {
                let mut __sym7 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state224(input, __tokens, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5, __sym6, __sym7));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state201<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((_, (6, _), _)) |
            Some((_, (8, _), _)) |
            Some((_, (11, _), _)) |
            Some((_, (12, _), _)) |
            Some((_, (13, _), _)) |
            Some((_, (15, _), _)) |
            Some((_, (16, _), _)) |
            Some((_, (17, _), _)) |
            Some((_, (18, _), _)) |
            Some((_, (19, _), _)) |
            Some((_, (20, _), _)) |
            Some((_, (21, _), _)) |
            Some((_, (22, _), _)) |
            Some((_, (23, _), _)) |
            Some((_, (24, _), _)) |
            Some((_, (27, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __sym2 = __sym2.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym2.2.clone();
                let __nt = super::__action23(input, __sym0, __sym1, __sym2);
                let __nt = __Nonterminal::exp((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state202<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, String, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((_, (6, _), _)) |
            Some((_, (8, _), _)) |
            Some((_, (11, _), _)) |
            Some((_, (12, _), _)) |
            Some((_, (13, _), _)) |
            Some((_, (15, _), _)) |
            Some((_, (16, _), _)) |
            Some((_, (17, _), _)) |
            Some((_, (18, _), _)) |
            Some((_, (19, _), _)) |
            Some((_, (20, _), _)) |
            Some((_, (21, _), _)) |
            Some((_, (22, _), _)) |
            Some((_, (23, _), _)) |
            Some((_, (24, _), _)) |
            Some((_, (27, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action13(input, __sym0);
                let __nt = __Nonterminal::id__or__imm((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state203<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, i32, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((_, (6, _), _)) |
            Some((_, (8, _), _)) |
            Some((_, (11, _), _)) |
            Some((_, (12, _), _)) |
            Some((_, (13, _), _)) |
            Some((_, (15, _), _)) |
            Some((_, (16, _), _)) |
            Some((_, (17, _), _)) |
            Some((_, (18, _), _)) |
            Some((_, (19, _), _)) |
            Some((_, (20, _), _)) |
            Some((_, (21, _), _)) |
            Some((_, (22, _), _)) |
            Some((_, (23, _), _)) |
            Some((_, (24, _), _)) |
            Some((_, (27, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action14(input, __sym0);
                let __nt = __Nonterminal::id__or__imm((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state204<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym3 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state23(input, __tokens, __sym3));
            }
            Some((_, (1, _), _)) => {
                let __start = __sym2.as_ref().unwrap().2.clone();
                let __end = __lookahead.as_ref().map(|o| o.0.clone()).unwrap_or_else(|| __start.clone());
                let __nt = super::__action65(input, &__start, &__end);
                let __nt = __Nonterminal::args((
                    __start,
                    __nt,
                    __end,
                ));
                __result = (__lookahead, __nt);
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym2.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::_28_3cid__ty_3e_20_22_2c_22_29_2b(__nt) => {
                    let __sym3 = &mut Some(__nt);
                    __result = try!(__state81(input, __tokens, __lookahead, __sym3));
                }
                __Nonterminal::args(__nt) => {
                    let __sym3 = &mut Some(__nt);
                    __result = try!(__state225(input, __tokens, __lookahead, __sym0, __sym1, __sym2, __sym3));
                }
                __Nonterminal::id__ty(__nt) => {
                    let __sym3 = &mut Some(__nt);
                    __result = try!(__state83(input, __tokens, __lookahead, __sym3));
                }
                __Nonterminal::ident(__nt) => {
                    let __sym3 = &mut Some(__nt);
                    __result = try!(__state84(input, __tokens, __lookahead, __sym3));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state205<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (26, __tok0), __loc2)) => {
                let mut __sym3 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state226(input, __tokens, __sym0, __sym1, __sym2, __sym3));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state206<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (26, __tok0), __loc2)) => {
                let mut __sym3 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state227(input, __tokens, __sym0, __sym1, __sym2, __sym3));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state207<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (26, __tok0), __loc2)) => {
                let mut __sym3 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state228(input, __tokens, __sym0, __sym1, __sym2, __sym3));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state208<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (28, __tok0), __loc2)) => {
                let mut __sym3 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state175(input, __tokens, __sym3));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym2.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::int(__nt) => {
                    let __sym3 = &mut Some(__nt);
                    __result = try!(__state229(input, __tokens, __lookahead, __sym0, __sym1, __sym2, __sym3));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state209<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, String, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((_, (6, _), _)) |
            Some((_, (8, _), _)) |
            Some((_, (11, _), _)) |
            Some((_, (12, _), _)) |
            Some((_, (13, _), _)) |
            Some((_, (15, _), _)) |
            Some((_, (16, _), _)) |
            Some((_, (17, _), _)) |
            Some((_, (18, _), _)) |
            Some((_, (19, _), _)) |
            Some((_, (20, _), _)) |
            Some((_, (21, _), _)) |
            Some((_, (22, _), _)) |
            Some((_, (23, _), _)) |
            Some((_, (24, _), _)) |
            Some((_, (27, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __sym2 = __sym2.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym2.2.clone();
                let __nt = super::__action31(input, __sym0, __sym1, __sym2);
                let __nt = __Nonterminal::exp((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state210<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, String, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (28, __tok0), __loc2)) => {
                let mut __sym3 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state68(input, __tokens, __sym3));
            }
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym3 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state69(input, __tokens, __sym3));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym2.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::id__or__imm(__nt) => {
                    let __sym3 = &mut Some(__nt);
                    __result = try!(__state230(input, __tokens, __lookahead, __sym0, __sym1, __sym2, __sym3));
                }
                __Nonterminal::ident(__nt) => {
                    let __sym3 = &mut Some(__nt);
                    __result = try!(__state66(input, __tokens, __lookahead, __sym3));
                }
                __Nonterminal::int(__nt) => {
                    let __sym3 = &mut Some(__nt);
                    __result = try!(__state67(input, __tokens, __lookahead, __sym3));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state211<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((_, (6, _), _)) |
            Some((_, (8, _), _)) |
            Some((_, (11, _), _)) |
            Some((_, (12, _), _)) |
            Some((_, (13, _), _)) |
            Some((_, (15, _), _)) |
            Some((_, (16, _), _)) |
            Some((_, (17, _), _)) |
            Some((_, (18, _), _)) |
            Some((_, (19, _), _)) |
            Some((_, (20, _), _)) |
            Some((_, (21, _), _)) |
            Some((_, (22, _), _)) |
            Some((_, (23, _), _)) |
            Some((_, (24, _), _)) |
            Some((_, (27, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __sym2 = __sym2.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym2.2.clone();
                let __nt = super::__action24(input, __sym0, __sym1, __sym2);
                let __nt = __Nonterminal::exp((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state212<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, E, usize)>,
        __sym5: &mut Option<(usize, &'input str, usize)>,
        __sym6: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (26, __tok0), __loc2)) => {
                let mut __sym7 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state231(input, __tokens, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5, __sym6, __sym7));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state213<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, E, usize)>,
        __sym5: &mut Option<(usize, &'input str, usize)>,
        __sym6: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (26, __tok0), __loc2)) => {
                let mut __sym7 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state232(input, __tokens, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5, __sym6, __sym7));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state214<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, &'input str, usize)>,
        __sym3: &mut Option<(usize, Vec<String>, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (1, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state233(input, __tokens, __sym0, __sym1, __sym2, __sym3, __sym4));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state215<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (6, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state119(input, __tokens, __sym4));
            }
            Some((__loc1, (8, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state120(input, __tokens, __sym4));
            }
            Some((__loc1, (11, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state121(input, __tokens, __sym4));
            }
            Some((__loc1, (12, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state122(input, __tokens, __sym4));
            }
            Some((__loc1, (13, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state123(input, __tokens, __sym4));
            }
            Some((__loc1, (15, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state124(input, __tokens, __sym4));
            }
            Some((__loc1, (16, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state125(input, __tokens, __sym4));
            }
            Some((__loc1, (17, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state126(input, __tokens, __sym4));
            }
            Some((__loc1, (18, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state127(input, __tokens, __sym4));
            }
            Some((__loc1, (19, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state128(input, __tokens, __sym4));
            }
            Some((__loc1, (20, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state129(input, __tokens, __sym4));
            }
            Some((__loc1, (21, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state130(input, __tokens, __sym4));
            }
            Some((__loc1, (22, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state131(input, __tokens, __sym4));
            }
            Some((__loc1, (23, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state132(input, __tokens, __sym4));
            }
            Some((__loc1, (24, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state133(input, __tokens, __sym4));
            }
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state23(input, __tokens, __sym4));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym3.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::exp(__nt) => {
                    let __sym4 = &mut Some(__nt);
                    __result = try!(__state116(input, __tokens, __lookahead, __sym4));
                }
                __Nonterminal::ident(__nt) => {
                    let __sym4 = &mut Some(__nt);
                    __result = try!(__state117(input, __tokens, __lookahead, __sym4));
                }
                __Nonterminal::term(__nt) => {
                    let __sym4 = &mut Some(__nt);
                    __result = try!(__state234(input, __tokens, __lookahead, __sym0, __sym1, __sym2, __sym3, __sym4));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state216<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (6, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state119(input, __tokens, __sym4));
            }
            Some((__loc1, (8, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state120(input, __tokens, __sym4));
            }
            Some((__loc1, (11, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state121(input, __tokens, __sym4));
            }
            Some((__loc1, (12, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state122(input, __tokens, __sym4));
            }
            Some((__loc1, (13, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state123(input, __tokens, __sym4));
            }
            Some((__loc1, (15, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state124(input, __tokens, __sym4));
            }
            Some((__loc1, (16, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state125(input, __tokens, __sym4));
            }
            Some((__loc1, (17, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state126(input, __tokens, __sym4));
            }
            Some((__loc1, (18, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state127(input, __tokens, __sym4));
            }
            Some((__loc1, (19, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state128(input, __tokens, __sym4));
            }
            Some((__loc1, (20, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state129(input, __tokens, __sym4));
            }
            Some((__loc1, (21, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state130(input, __tokens, __sym4));
            }
            Some((__loc1, (22, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state131(input, __tokens, __sym4));
            }
            Some((__loc1, (23, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state132(input, __tokens, __sym4));
            }
            Some((__loc1, (24, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state133(input, __tokens, __sym4));
            }
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state23(input, __tokens, __sym4));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym3.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::exp(__nt) => {
                    let __sym4 = &mut Some(__nt);
                    __result = try!(__state116(input, __tokens, __lookahead, __sym4));
                }
                __Nonterminal::ident(__nt) => {
                    let __sym4 = &mut Some(__nt);
                    __result = try!(__state117(input, __tokens, __lookahead, __sym4));
                }
                __Nonterminal::term(__nt) => {
                    let __sym4 = &mut Some(__nt);
                    __result = try!(__state235(input, __tokens, __lookahead, __sym0, __sym1, __sym2, __sym3, __sym4));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state217<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (6, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state119(input, __tokens, __sym4));
            }
            Some((__loc1, (8, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state120(input, __tokens, __sym4));
            }
            Some((__loc1, (11, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state121(input, __tokens, __sym4));
            }
            Some((__loc1, (12, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state122(input, __tokens, __sym4));
            }
            Some((__loc1, (13, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state123(input, __tokens, __sym4));
            }
            Some((__loc1, (15, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state124(input, __tokens, __sym4));
            }
            Some((__loc1, (16, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state125(input, __tokens, __sym4));
            }
            Some((__loc1, (17, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state126(input, __tokens, __sym4));
            }
            Some((__loc1, (18, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state127(input, __tokens, __sym4));
            }
            Some((__loc1, (19, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state128(input, __tokens, __sym4));
            }
            Some((__loc1, (20, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state129(input, __tokens, __sym4));
            }
            Some((__loc1, (21, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state130(input, __tokens, __sym4));
            }
            Some((__loc1, (22, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state131(input, __tokens, __sym4));
            }
            Some((__loc1, (23, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state132(input, __tokens, __sym4));
            }
            Some((__loc1, (24, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state133(input, __tokens, __sym4));
            }
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state23(input, __tokens, __sym4));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym3.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::exp(__nt) => {
                    let __sym4 = &mut Some(__nt);
                    __result = try!(__state116(input, __tokens, __lookahead, __sym4));
                }
                __Nonterminal::ident(__nt) => {
                    let __sym4 = &mut Some(__nt);
                    __result = try!(__state117(input, __tokens, __lookahead, __sym4));
                }
                __Nonterminal::term(__nt) => {
                    let __sym4 = &mut Some(__nt);
                    __result = try!(__state236(input, __tokens, __lookahead, __sym0, __sym1, __sym2, __sym3, __sym4));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state218<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, i32, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((_, (6, _), _)) |
            Some((_, (8, _), _)) |
            Some((_, (11, _), _)) |
            Some((_, (12, _), _)) |
            Some((_, (13, _), _)) |
            Some((_, (15, _), _)) |
            Some((_, (16, _), _)) |
            Some((_, (17, _), _)) |
            Some((_, (18, _), _)) |
            Some((_, (19, _), _)) |
            Some((_, (20, _), _)) |
            Some((_, (21, _), _)) |
            Some((_, (22, _), _)) |
            Some((_, (23, _), _)) |
            Some((_, (24, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __sym2 = __sym2.take().unwrap();
                let __sym3 = __sym3.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym3.2.clone();
                let __nt = super::__action25(input, __sym0, __sym1, __sym2, __sym3);
                let __nt = __Nonterminal::exp((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state219<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, String, usize)>,
        __sym3: &mut Option<(usize, IdOrImm, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (28, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state150(input, __tokens, __sym4));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym3.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::int(__nt) => {
                    let __sym4 = &mut Some(__nt);
                    __result = try!(__state237(input, __tokens, __lookahead, __sym0, __sym1, __sym2, __sym3, __sym4));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state220<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, Vec<T>, usize)>,
        __sym2: &mut Option<(usize, &'input str, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, T, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((_, (1, _), _)) |
            Some((_, (2, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __sym2 = __sym2.take().unwrap();
                let __sym3 = __sym3.take().unwrap();
                let __sym4 = __sym4.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym4.2.clone();
                let __nt = super::__action11(input, __sym0, __sym1, __sym2, __sym3, __sym4);
                let __nt = __Nonterminal::ty((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state221<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, &'input str, usize)>,
        __sym3: &mut Option<(usize, Vec<String>, usize)>,
        __sym4: &mut Option<(usize, &'input str, usize)>,
        __sym5: &mut Option<(usize, &'input str, usize)>,
        __sym6: &mut Option<(usize, T, usize)>,
        __sym7: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (6, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state119(input, __tokens, __sym8));
            }
            Some((__loc1, (8, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state120(input, __tokens, __sym8));
            }
            Some((__loc1, (11, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state121(input, __tokens, __sym8));
            }
            Some((__loc1, (12, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state122(input, __tokens, __sym8));
            }
            Some((__loc1, (13, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state123(input, __tokens, __sym8));
            }
            Some((__loc1, (15, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state124(input, __tokens, __sym8));
            }
            Some((__loc1, (16, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state125(input, __tokens, __sym8));
            }
            Some((__loc1, (17, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state126(input, __tokens, __sym8));
            }
            Some((__loc1, (18, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state127(input, __tokens, __sym8));
            }
            Some((__loc1, (19, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state128(input, __tokens, __sym8));
            }
            Some((__loc1, (20, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state129(input, __tokens, __sym8));
            }
            Some((__loc1, (21, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state130(input, __tokens, __sym8));
            }
            Some((__loc1, (22, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state131(input, __tokens, __sym8));
            }
            Some((__loc1, (23, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state132(input, __tokens, __sym8));
            }
            Some((__loc1, (24, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state133(input, __tokens, __sym8));
            }
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state23(input, __tokens, __sym8));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym7.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::exp(__nt) => {
                    let __sym8 = &mut Some(__nt);
                    __result = try!(__state116(input, __tokens, __lookahead, __sym8));
                }
                __Nonterminal::ident(__nt) => {
                    let __sym8 = &mut Some(__nt);
                    __result = try!(__state117(input, __tokens, __lookahead, __sym8));
                }
                __Nonterminal::term(__nt) => {
                    let __sym8 = &mut Some(__nt);
                    __result = try!(__state238(input, __tokens, __lookahead, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5, __sym6, __sym7, __sym8));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state222<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, Vec<T>, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (1, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state239(input, __tokens, __sym0, __sym1, __sym2));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state223<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, String, usize)>,
        __sym1: &mut Option<(usize, &'input str, usize)>,
        __sym2: &mut Option<(usize, T, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (6, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state92(input, __tokens, __sym4));
            }
            Some((__loc1, (8, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state93(input, __tokens, __sym4));
            }
            Some((__loc1, (11, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state94(input, __tokens, __sym4));
            }
            Some((__loc1, (12, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state95(input, __tokens, __sym4));
            }
            Some((__loc1, (13, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state96(input, __tokens, __sym4));
            }
            Some((__loc1, (15, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state97(input, __tokens, __sym4));
            }
            Some((__loc1, (16, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state98(input, __tokens, __sym4));
            }
            Some((__loc1, (17, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state99(input, __tokens, __sym4));
            }
            Some((__loc1, (18, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state100(input, __tokens, __sym4));
            }
            Some((__loc1, (19, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state101(input, __tokens, __sym4));
            }
            Some((__loc1, (20, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state102(input, __tokens, __sym4));
            }
            Some((__loc1, (21, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state103(input, __tokens, __sym4));
            }
            Some((__loc1, (22, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state104(input, __tokens, __sym4));
            }
            Some((__loc1, (23, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state105(input, __tokens, __sym4));
            }
            Some((__loc1, (24, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state106(input, __tokens, __sym4));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym3.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::exp(__nt) => {
                    let __sym4 = &mut Some(__nt);
                    __result = try!(__state240(input, __tokens, __lookahead, __sym0, __sym1, __sym2, __sym3, __sym4));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state224<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, E, usize)>,
        __sym5: &mut Option<(usize, &'input str, usize)>,
        __sym6: &mut Option<(usize, &'input str, usize)>,
        __sym7: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (6, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state119(input, __tokens, __sym8));
            }
            Some((__loc1, (8, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state120(input, __tokens, __sym8));
            }
            Some((__loc1, (11, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state121(input, __tokens, __sym8));
            }
            Some((__loc1, (12, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state122(input, __tokens, __sym8));
            }
            Some((__loc1, (13, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state123(input, __tokens, __sym8));
            }
            Some((__loc1, (15, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state124(input, __tokens, __sym8));
            }
            Some((__loc1, (16, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state125(input, __tokens, __sym8));
            }
            Some((__loc1, (17, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state126(input, __tokens, __sym8));
            }
            Some((__loc1, (18, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state127(input, __tokens, __sym8));
            }
            Some((__loc1, (19, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state128(input, __tokens, __sym8));
            }
            Some((__loc1, (20, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state129(input, __tokens, __sym8));
            }
            Some((__loc1, (21, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state130(input, __tokens, __sym8));
            }
            Some((__loc1, (22, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state131(input, __tokens, __sym8));
            }
            Some((__loc1, (23, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state132(input, __tokens, __sym8));
            }
            Some((__loc1, (24, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state133(input, __tokens, __sym8));
            }
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state23(input, __tokens, __sym8));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym7.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::exp(__nt) => {
                    let __sym8 = &mut Some(__nt);
                    __result = try!(__state116(input, __tokens, __lookahead, __sym8));
                }
                __Nonterminal::ident(__nt) => {
                    let __sym8 = &mut Some(__nt);
                    __result = try!(__state117(input, __tokens, __lookahead, __sym8));
                }
                __Nonterminal::term(__nt) => {
                    let __sym8 = &mut Some(__nt);
                    __result = try!(__state241(input, __tokens, __lookahead, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5, __sym6, __sym7, __sym8));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state225<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, &'input str, usize)>,
        __sym3: &mut Option<(usize, Vec<String>, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (1, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state242(input, __tokens, __sym0, __sym1, __sym2, __sym3, __sym4));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state226<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (6, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state119(input, __tokens, __sym4));
            }
            Some((__loc1, (8, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state120(input, __tokens, __sym4));
            }
            Some((__loc1, (11, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state121(input, __tokens, __sym4));
            }
            Some((__loc1, (12, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state122(input, __tokens, __sym4));
            }
            Some((__loc1, (13, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state123(input, __tokens, __sym4));
            }
            Some((__loc1, (15, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state124(input, __tokens, __sym4));
            }
            Some((__loc1, (16, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state125(input, __tokens, __sym4));
            }
            Some((__loc1, (17, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state126(input, __tokens, __sym4));
            }
            Some((__loc1, (18, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state127(input, __tokens, __sym4));
            }
            Some((__loc1, (19, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state128(input, __tokens, __sym4));
            }
            Some((__loc1, (20, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state129(input, __tokens, __sym4));
            }
            Some((__loc1, (21, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state130(input, __tokens, __sym4));
            }
            Some((__loc1, (22, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state131(input, __tokens, __sym4));
            }
            Some((__loc1, (23, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state132(input, __tokens, __sym4));
            }
            Some((__loc1, (24, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state133(input, __tokens, __sym4));
            }
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state23(input, __tokens, __sym4));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym3.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::exp(__nt) => {
                    let __sym4 = &mut Some(__nt);
                    __result = try!(__state116(input, __tokens, __lookahead, __sym4));
                }
                __Nonterminal::ident(__nt) => {
                    let __sym4 = &mut Some(__nt);
                    __result = try!(__state117(input, __tokens, __lookahead, __sym4));
                }
                __Nonterminal::term(__nt) => {
                    let __sym4 = &mut Some(__nt);
                    __result = try!(__state243(input, __tokens, __lookahead, __sym0, __sym1, __sym2, __sym3, __sym4));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state227<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (6, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state119(input, __tokens, __sym4));
            }
            Some((__loc1, (8, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state120(input, __tokens, __sym4));
            }
            Some((__loc1, (11, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state121(input, __tokens, __sym4));
            }
            Some((__loc1, (12, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state122(input, __tokens, __sym4));
            }
            Some((__loc1, (13, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state123(input, __tokens, __sym4));
            }
            Some((__loc1, (15, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state124(input, __tokens, __sym4));
            }
            Some((__loc1, (16, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state125(input, __tokens, __sym4));
            }
            Some((__loc1, (17, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state126(input, __tokens, __sym4));
            }
            Some((__loc1, (18, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state127(input, __tokens, __sym4));
            }
            Some((__loc1, (19, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state128(input, __tokens, __sym4));
            }
            Some((__loc1, (20, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state129(input, __tokens, __sym4));
            }
            Some((__loc1, (21, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state130(input, __tokens, __sym4));
            }
            Some((__loc1, (22, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state131(input, __tokens, __sym4));
            }
            Some((__loc1, (23, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state132(input, __tokens, __sym4));
            }
            Some((__loc1, (24, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state133(input, __tokens, __sym4));
            }
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state23(input, __tokens, __sym4));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym3.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::exp(__nt) => {
                    let __sym4 = &mut Some(__nt);
                    __result = try!(__state116(input, __tokens, __lookahead, __sym4));
                }
                __Nonterminal::ident(__nt) => {
                    let __sym4 = &mut Some(__nt);
                    __result = try!(__state117(input, __tokens, __lookahead, __sym4));
                }
                __Nonterminal::term(__nt) => {
                    let __sym4 = &mut Some(__nt);
                    __result = try!(__state244(input, __tokens, __lookahead, __sym0, __sym1, __sym2, __sym3, __sym4));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state228<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (6, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state119(input, __tokens, __sym4));
            }
            Some((__loc1, (8, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state120(input, __tokens, __sym4));
            }
            Some((__loc1, (11, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state121(input, __tokens, __sym4));
            }
            Some((__loc1, (12, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state122(input, __tokens, __sym4));
            }
            Some((__loc1, (13, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state123(input, __tokens, __sym4));
            }
            Some((__loc1, (15, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state124(input, __tokens, __sym4));
            }
            Some((__loc1, (16, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state125(input, __tokens, __sym4));
            }
            Some((__loc1, (17, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state126(input, __tokens, __sym4));
            }
            Some((__loc1, (18, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state127(input, __tokens, __sym4));
            }
            Some((__loc1, (19, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state128(input, __tokens, __sym4));
            }
            Some((__loc1, (20, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state129(input, __tokens, __sym4));
            }
            Some((__loc1, (21, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state130(input, __tokens, __sym4));
            }
            Some((__loc1, (22, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state131(input, __tokens, __sym4));
            }
            Some((__loc1, (23, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state132(input, __tokens, __sym4));
            }
            Some((__loc1, (24, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state133(input, __tokens, __sym4));
            }
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state23(input, __tokens, __sym4));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym3.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::exp(__nt) => {
                    let __sym4 = &mut Some(__nt);
                    __result = try!(__state116(input, __tokens, __lookahead, __sym4));
                }
                __Nonterminal::ident(__nt) => {
                    let __sym4 = &mut Some(__nt);
                    __result = try!(__state117(input, __tokens, __lookahead, __sym4));
                }
                __Nonterminal::term(__nt) => {
                    let __sym4 = &mut Some(__nt);
                    __result = try!(__state245(input, __tokens, __lookahead, __sym0, __sym1, __sym2, __sym3, __sym4));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state229<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, i32, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((_, (6, _), _)) |
            Some((_, (8, _), _)) |
            Some((_, (11, _), _)) |
            Some((_, (12, _), _)) |
            Some((_, (13, _), _)) |
            Some((_, (15, _), _)) |
            Some((_, (16, _), _)) |
            Some((_, (17, _), _)) |
            Some((_, (18, _), _)) |
            Some((_, (19, _), _)) |
            Some((_, (20, _), _)) |
            Some((_, (21, _), _)) |
            Some((_, (22, _), _)) |
            Some((_, (23, _), _)) |
            Some((_, (24, _), _)) |
            Some((_, (27, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __sym2 = __sym2.take().unwrap();
                let __sym3 = __sym3.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym3.2.clone();
                let __nt = super::__action25(input, __sym0, __sym1, __sym2, __sym3);
                let __nt = __Nonterminal::exp((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state230<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, String, usize)>,
        __sym3: &mut Option<(usize, IdOrImm, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (28, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state175(input, __tokens, __sym4));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym3.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::int(__nt) => {
                    let __sym4 = &mut Some(__nt);
                    __result = try!(__state246(input, __tokens, __lookahead, __sym0, __sym1, __sym2, __sym3, __sym4));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state231<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, E, usize)>,
        __sym5: &mut Option<(usize, &'input str, usize)>,
        __sym6: &mut Option<(usize, &'input str, usize)>,
        __sym7: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (6, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state119(input, __tokens, __sym8));
            }
            Some((__loc1, (8, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state120(input, __tokens, __sym8));
            }
            Some((__loc1, (11, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state121(input, __tokens, __sym8));
            }
            Some((__loc1, (12, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state122(input, __tokens, __sym8));
            }
            Some((__loc1, (13, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state123(input, __tokens, __sym8));
            }
            Some((__loc1, (15, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state124(input, __tokens, __sym8));
            }
            Some((__loc1, (16, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state125(input, __tokens, __sym8));
            }
            Some((__loc1, (17, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state126(input, __tokens, __sym8));
            }
            Some((__loc1, (18, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state127(input, __tokens, __sym8));
            }
            Some((__loc1, (19, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state128(input, __tokens, __sym8));
            }
            Some((__loc1, (20, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state129(input, __tokens, __sym8));
            }
            Some((__loc1, (21, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state130(input, __tokens, __sym8));
            }
            Some((__loc1, (22, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state131(input, __tokens, __sym8));
            }
            Some((__loc1, (23, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state132(input, __tokens, __sym8));
            }
            Some((__loc1, (24, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state133(input, __tokens, __sym8));
            }
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state23(input, __tokens, __sym8));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym7.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::exp(__nt) => {
                    let __sym8 = &mut Some(__nt);
                    __result = try!(__state116(input, __tokens, __lookahead, __sym8));
                }
                __Nonterminal::ident(__nt) => {
                    let __sym8 = &mut Some(__nt);
                    __result = try!(__state117(input, __tokens, __lookahead, __sym8));
                }
                __Nonterminal::term(__nt) => {
                    let __sym8 = &mut Some(__nt);
                    __result = try!(__state247(input, __tokens, __lookahead, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5, __sym6, __sym7, __sym8));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state232<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, E, usize)>,
        __sym5: &mut Option<(usize, &'input str, usize)>,
        __sym6: &mut Option<(usize, &'input str, usize)>,
        __sym7: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (6, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state119(input, __tokens, __sym8));
            }
            Some((__loc1, (8, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state120(input, __tokens, __sym8));
            }
            Some((__loc1, (11, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state121(input, __tokens, __sym8));
            }
            Some((__loc1, (12, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state122(input, __tokens, __sym8));
            }
            Some((__loc1, (13, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state123(input, __tokens, __sym8));
            }
            Some((__loc1, (15, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state124(input, __tokens, __sym8));
            }
            Some((__loc1, (16, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state125(input, __tokens, __sym8));
            }
            Some((__loc1, (17, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state126(input, __tokens, __sym8));
            }
            Some((__loc1, (18, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state127(input, __tokens, __sym8));
            }
            Some((__loc1, (19, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state128(input, __tokens, __sym8));
            }
            Some((__loc1, (20, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state129(input, __tokens, __sym8));
            }
            Some((__loc1, (21, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state130(input, __tokens, __sym8));
            }
            Some((__loc1, (22, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state131(input, __tokens, __sym8));
            }
            Some((__loc1, (23, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state132(input, __tokens, __sym8));
            }
            Some((__loc1, (24, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state133(input, __tokens, __sym8));
            }
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state23(input, __tokens, __sym8));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym7.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::exp(__nt) => {
                    let __sym8 = &mut Some(__nt);
                    __result = try!(__state116(input, __tokens, __lookahead, __sym8));
                }
                __Nonterminal::ident(__nt) => {
                    let __sym8 = &mut Some(__nt);
                    __result = try!(__state117(input, __tokens, __lookahead, __sym8));
                }
                __Nonterminal::term(__nt) => {
                    let __sym8 = &mut Some(__nt);
                    __result = try!(__state248(input, __tokens, __lookahead, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5, __sym6, __sym7, __sym8));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state233<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, &'input str, usize)>,
        __sym3: &mut Option<(usize, Vec<String>, usize)>,
        __sym4: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((_, (6, _), _)) |
            Some((_, (8, _), _)) |
            Some((_, (11, _), _)) |
            Some((_, (12, _), _)) |
            Some((_, (13, _), _)) |
            Some((_, (15, _), _)) |
            Some((_, (16, _), _)) |
            Some((_, (17, _), _)) |
            Some((_, (18, _), _)) |
            Some((_, (19, _), _)) |
            Some((_, (20, _), _)) |
            Some((_, (21, _), _)) |
            Some((_, (22, _), _)) |
            Some((_, (23, _), _)) |
            Some((_, (24, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __sym2 = __sym2.take().unwrap();
                let __sym3 = __sym3.take().unwrap();
                let __sym4 = __sym4.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym4.2.clone();
                let __nt = super::__action30(input, __sym0, __sym1, __sym2, __sym3, __sym4);
                let __nt = __Nonterminal::exp((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state234<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, E, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (27, __tok0), __loc2)) => {
                let mut __sym5 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state249(input, __tokens, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state235<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, E, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (27, __tok0), __loc2)) => {
                let mut __sym5 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state250(input, __tokens, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state236<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, E, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (27, __tok0), __loc2)) => {
                let mut __sym5 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state251(input, __tokens, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state237<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, String, usize)>,
        __sym3: &mut Option<(usize, IdOrImm, usize)>,
        __sym4: &mut Option<(usize, i32, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((_, (6, _), _)) |
            Some((_, (8, _), _)) |
            Some((_, (11, _), _)) |
            Some((_, (12, _), _)) |
            Some((_, (13, _), _)) |
            Some((_, (15, _), _)) |
            Some((_, (16, _), _)) |
            Some((_, (17, _), _)) |
            Some((_, (18, _), _)) |
            Some((_, (19, _), _)) |
            Some((_, (20, _), _)) |
            Some((_, (21, _), _)) |
            Some((_, (22, _), _)) |
            Some((_, (23, _), _)) |
            Some((_, (24, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __sym2 = __sym2.take().unwrap();
                let __sym3 = __sym3.take().unwrap();
                let __sym4 = __sym4.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym4.2.clone();
                let __nt = super::__action26(input, __sym0, __sym1, __sym2, __sym3, __sym4);
                let __nt = __Nonterminal::exp((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state238<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, &'input str, usize)>,
        __sym3: &mut Option<(usize, Vec<String>, usize)>,
        __sym4: &mut Option<(usize, &'input str, usize)>,
        __sym5: &mut Option<(usize, &'input str, usize)>,
        __sym6: &mut Option<(usize, T, usize)>,
        __sym7: &mut Option<(usize, &'input str, usize)>,
        __sym8: &mut Option<(usize, E, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (27, __tok0), __loc2)) => {
                let mut __sym9 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state252(input, __tokens, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5, __sym6, __sym7, __sym8, __sym9));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state239<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, Vec<T>, usize)>,
        __sym2: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (3, __tok0), __loc2)) => {
                let mut __sym3 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state253(input, __tokens, __sym0, __sym1, __sym2, __sym3));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state240<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, String, usize)>,
        __sym1: &mut Option<(usize, &'input str, usize)>,
        __sym2: &mut Option<(usize, T, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, Exp, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (6, __tok0), __loc2)) => {
                let mut __sym5 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state119(input, __tokens, __sym5));
            }
            Some((__loc1, (8, __tok0), __loc2)) => {
                let mut __sym5 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state120(input, __tokens, __sym5));
            }
            Some((__loc1, (11, __tok0), __loc2)) => {
                let mut __sym5 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state121(input, __tokens, __sym5));
            }
            Some((__loc1, (12, __tok0), __loc2)) => {
                let mut __sym5 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state122(input, __tokens, __sym5));
            }
            Some((__loc1, (13, __tok0), __loc2)) => {
                let mut __sym5 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state123(input, __tokens, __sym5));
            }
            Some((__loc1, (15, __tok0), __loc2)) => {
                let mut __sym5 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state124(input, __tokens, __sym5));
            }
            Some((__loc1, (16, __tok0), __loc2)) => {
                let mut __sym5 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state125(input, __tokens, __sym5));
            }
            Some((__loc1, (17, __tok0), __loc2)) => {
                let mut __sym5 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state126(input, __tokens, __sym5));
            }
            Some((__loc1, (18, __tok0), __loc2)) => {
                let mut __sym5 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state127(input, __tokens, __sym5));
            }
            Some((__loc1, (19, __tok0), __loc2)) => {
                let mut __sym5 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state128(input, __tokens, __sym5));
            }
            Some((__loc1, (20, __tok0), __loc2)) => {
                let mut __sym5 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state129(input, __tokens, __sym5));
            }
            Some((__loc1, (21, __tok0), __loc2)) => {
                let mut __sym5 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state130(input, __tokens, __sym5));
            }
            Some((__loc1, (22, __tok0), __loc2)) => {
                let mut __sym5 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state131(input, __tokens, __sym5));
            }
            Some((__loc1, (23, __tok0), __loc2)) => {
                let mut __sym5 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state132(input, __tokens, __sym5));
            }
            Some((__loc1, (24, __tok0), __loc2)) => {
                let mut __sym5 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state133(input, __tokens, __sym5));
            }
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym5 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state23(input, __tokens, __sym5));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym4.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::exp(__nt) => {
                    let __sym5 = &mut Some(__nt);
                    __result = try!(__state116(input, __tokens, __lookahead, __sym5));
                }
                __Nonterminal::ident(__nt) => {
                    let __sym5 = &mut Some(__nt);
                    __result = try!(__state117(input, __tokens, __lookahead, __sym5));
                }
                __Nonterminal::term(__nt) => {
                    let __sym5 = &mut Some(__nt);
                    __result = try!(__state254(input, __tokens, __lookahead, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state241<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, E, usize)>,
        __sym5: &mut Option<(usize, &'input str, usize)>,
        __sym6: &mut Option<(usize, &'input str, usize)>,
        __sym7: &mut Option<(usize, &'input str, usize)>,
        __sym8: &mut Option<(usize, E, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (27, __tok0), __loc2)) => {
                let mut __sym9 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state255(input, __tokens, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5, __sym6, __sym7, __sym8, __sym9));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state242<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, &'input str, usize)>,
        __sym3: &mut Option<(usize, Vec<String>, usize)>,
        __sym4: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((_, (6, _), _)) |
            Some((_, (8, _), _)) |
            Some((_, (11, _), _)) |
            Some((_, (12, _), _)) |
            Some((_, (13, _), _)) |
            Some((_, (15, _), _)) |
            Some((_, (16, _), _)) |
            Some((_, (17, _), _)) |
            Some((_, (18, _), _)) |
            Some((_, (19, _), _)) |
            Some((_, (20, _), _)) |
            Some((_, (21, _), _)) |
            Some((_, (22, _), _)) |
            Some((_, (23, _), _)) |
            Some((_, (24, _), _)) |
            Some((_, (27, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __sym2 = __sym2.take().unwrap();
                let __sym3 = __sym3.take().unwrap();
                let __sym4 = __sym4.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym4.2.clone();
                let __nt = super::__action30(input, __sym0, __sym1, __sym2, __sym3, __sym4);
                let __nt = __Nonterminal::exp((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state243<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, E, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (27, __tok0), __loc2)) => {
                let mut __sym5 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state256(input, __tokens, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state244<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, E, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (27, __tok0), __loc2)) => {
                let mut __sym5 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state257(input, __tokens, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state245<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, E, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (27, __tok0), __loc2)) => {
                let mut __sym5 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state258(input, __tokens, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state246<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, String, usize)>,
        __sym3: &mut Option<(usize, IdOrImm, usize)>,
        __sym4: &mut Option<(usize, i32, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((_, (6, _), _)) |
            Some((_, (8, _), _)) |
            Some((_, (11, _), _)) |
            Some((_, (12, _), _)) |
            Some((_, (13, _), _)) |
            Some((_, (15, _), _)) |
            Some((_, (16, _), _)) |
            Some((_, (17, _), _)) |
            Some((_, (18, _), _)) |
            Some((_, (19, _), _)) |
            Some((_, (20, _), _)) |
            Some((_, (21, _), _)) |
            Some((_, (22, _), _)) |
            Some((_, (23, _), _)) |
            Some((_, (24, _), _)) |
            Some((_, (27, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __sym2 = __sym2.take().unwrap();
                let __sym3 = __sym3.take().unwrap();
                let __sym4 = __sym4.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym4.2.clone();
                let __nt = super::__action26(input, __sym0, __sym1, __sym2, __sym3, __sym4);
                let __nt = __Nonterminal::exp((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state247<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, E, usize)>,
        __sym5: &mut Option<(usize, &'input str, usize)>,
        __sym6: &mut Option<(usize, &'input str, usize)>,
        __sym7: &mut Option<(usize, &'input str, usize)>,
        __sym8: &mut Option<(usize, E, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (27, __tok0), __loc2)) => {
                let mut __sym9 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state259(input, __tokens, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5, __sym6, __sym7, __sym8, __sym9));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state248<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, E, usize)>,
        __sym5: &mut Option<(usize, &'input str, usize)>,
        __sym6: &mut Option<(usize, &'input str, usize)>,
        __sym7: &mut Option<(usize, &'input str, usize)>,
        __sym8: &mut Option<(usize, E, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (27, __tok0), __loc2)) => {
                let mut __sym9 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state260(input, __tokens, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5, __sym6, __sym7, __sym8, __sym9));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state249<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, E, usize)>,
        __sym5: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (10, __tok0), __loc2)) => {
                let mut __sym6 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state261(input, __tokens, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5, __sym6));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state250<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, E, usize)>,
        __sym5: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (10, __tok0), __loc2)) => {
                let mut __sym6 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state262(input, __tokens, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5, __sym6));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state251<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, E, usize)>,
        __sym5: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (10, __tok0), __loc2)) => {
                let mut __sym6 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state263(input, __tokens, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5, __sym6));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state252<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, &'input str, usize)>,
        __sym3: &mut Option<(usize, Vec<String>, usize)>,
        __sym4: &mut Option<(usize, &'input str, usize)>,
        __sym5: &mut Option<(usize, &'input str, usize)>,
        __sym6: &mut Option<(usize, T, usize)>,
        __sym7: &mut Option<(usize, &'input str, usize)>,
        __sym8: &mut Option<(usize, E, usize)>,
        __sym9: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((_, (6, _), _)) |
            Some((_, (8, _), _)) |
            Some((_, (9, _), _)) |
            Some((_, (11, _), _)) |
            Some((_, (12, _), _)) |
            Some((_, (13, _), _)) |
            Some((_, (15, _), _)) |
            Some((_, (16, _), _)) |
            Some((_, (17, _), _)) |
            Some((_, (18, _), _)) |
            Some((_, (19, _), _)) |
            Some((_, (20, _), _)) |
            Some((_, (21, _), _)) |
            Some((_, (22, _), _)) |
            Some((_, (23, _), _)) |
            Some((_, (24, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __sym2 = __sym2.take().unwrap();
                let __sym3 = __sym3.take().unwrap();
                let __sym4 = __sym4.take().unwrap();
                let __sym5 = __sym5.take().unwrap();
                let __sym6 = __sym6.take().unwrap();
                let __sym7 = __sym7.take().unwrap();
                let __sym8 = __sym8.take().unwrap();
                let __sym9 = __sym9.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym9.2.clone();
                let __nt = super::__action5(input, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5, __sym6, __sym7, __sym8, __sym9);
                let __nt = __Nonterminal::fundef((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state253<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, Vec<T>, usize)>,
        __sym2: &mut Option<(usize, &'input str, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (0, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state195(input, __tokens, __sym4));
            }
            Some((__loc1, (7, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state196(input, __tokens, __sym4));
            }
            Some((__loc1, (14, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state197(input, __tokens, __sym4));
            }
            Some((__loc1, (25, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state198(input, __tokens, __sym4));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym3.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::ty(__nt) => {
                    let __sym4 = &mut Some(__nt);
                    __result = try!(__state264(input, __tokens, __lookahead, __sym0, __sym1, __sym2, __sym3, __sym4));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state254<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, String, usize)>,
        __sym1: &mut Option<(usize, &'input str, usize)>,
        __sym2: &mut Option<(usize, T, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, Exp, usize)>,
        __sym5: &mut Option<(usize, E, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((_, (27, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __sym2 = __sym2.take().unwrap();
                let __sym3 = __sym3.take().unwrap();
                let __sym4 = __sym4.take().unwrap();
                let __sym5 = __sym5.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym5.2.clone();
                let __nt = super::__action15(input, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5);
                let __nt = __Nonterminal::term((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state255<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, E, usize)>,
        __sym5: &mut Option<(usize, &'input str, usize)>,
        __sym6: &mut Option<(usize, &'input str, usize)>,
        __sym7: &mut Option<(usize, &'input str, usize)>,
        __sym8: &mut Option<(usize, E, usize)>,
        __sym9: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            None |
            Some((_, (6, _), _)) |
            Some((_, (8, _), _)) |
            Some((_, (11, _), _)) |
            Some((_, (12, _), _)) |
            Some((_, (13, _), _)) |
            Some((_, (15, _), _)) |
            Some((_, (16, _), _)) |
            Some((_, (17, _), _)) |
            Some((_, (18, _), _)) |
            Some((_, (19, _), _)) |
            Some((_, (20, _), _)) |
            Some((_, (21, _), _)) |
            Some((_, (22, _), _)) |
            Some((_, (23, _), _)) |
            Some((_, (24, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __sym2 = __sym2.take().unwrap();
                let __sym3 = __sym3.take().unwrap();
                let __sym4 = __sym4.take().unwrap();
                let __sym5 = __sym5.take().unwrap();
                let __sym6 = __sym6.take().unwrap();
                let __sym7 = __sym7.take().unwrap();
                let __sym8 = __sym8.take().unwrap();
                let __sym9 = __sym9.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym9.2.clone();
                let __nt = super::__action27(input, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5, __sym6, __sym7, __sym8, __sym9);
                let __nt = __Nonterminal::exp((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state256<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, E, usize)>,
        __sym5: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (10, __tok0), __loc2)) => {
                let mut __sym6 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state265(input, __tokens, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5, __sym6));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state257<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, E, usize)>,
        __sym5: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (10, __tok0), __loc2)) => {
                let mut __sym6 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state266(input, __tokens, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5, __sym6));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state258<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, E, usize)>,
        __sym5: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (10, __tok0), __loc2)) => {
                let mut __sym6 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state267(input, __tokens, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5, __sym6));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state259<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, E, usize)>,
        __sym5: &mut Option<(usize, &'input str, usize)>,
        __sym6: &mut Option<(usize, &'input str, usize)>,
        __sym7: &mut Option<(usize, &'input str, usize)>,
        __sym8: &mut Option<(usize, E, usize)>,
        __sym9: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            None |
            Some((_, (6, _), _)) |
            Some((_, (8, _), _)) |
            Some((_, (11, _), _)) |
            Some((_, (12, _), _)) |
            Some((_, (13, _), _)) |
            Some((_, (15, _), _)) |
            Some((_, (16, _), _)) |
            Some((_, (17, _), _)) |
            Some((_, (18, _), _)) |
            Some((_, (19, _), _)) |
            Some((_, (20, _), _)) |
            Some((_, (21, _), _)) |
            Some((_, (22, _), _)) |
            Some((_, (23, _), _)) |
            Some((_, (24, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __sym2 = __sym2.take().unwrap();
                let __sym3 = __sym3.take().unwrap();
                let __sym4 = __sym4.take().unwrap();
                let __sym5 = __sym5.take().unwrap();
                let __sym6 = __sym6.take().unwrap();
                let __sym7 = __sym7.take().unwrap();
                let __sym8 = __sym8.take().unwrap();
                let __sym9 = __sym9.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym9.2.clone();
                let __nt = super::__action29(input, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5, __sym6, __sym7, __sym8, __sym9);
                let __nt = __Nonterminal::exp((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state260<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, E, usize)>,
        __sym5: &mut Option<(usize, &'input str, usize)>,
        __sym6: &mut Option<(usize, &'input str, usize)>,
        __sym7: &mut Option<(usize, &'input str, usize)>,
        __sym8: &mut Option<(usize, E, usize)>,
        __sym9: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            None |
            Some((_, (6, _), _)) |
            Some((_, (8, _), _)) |
            Some((_, (11, _), _)) |
            Some((_, (12, _), _)) |
            Some((_, (13, _), _)) |
            Some((_, (15, _), _)) |
            Some((_, (16, _), _)) |
            Some((_, (17, _), _)) |
            Some((_, (18, _), _)) |
            Some((_, (19, _), _)) |
            Some((_, (20, _), _)) |
            Some((_, (21, _), _)) |
            Some((_, (22, _), _)) |
            Some((_, (23, _), _)) |
            Some((_, (24, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __sym2 = __sym2.take().unwrap();
                let __sym3 = __sym3.take().unwrap();
                let __sym4 = __sym4.take().unwrap();
                let __sym5 = __sym5.take().unwrap();
                let __sym6 = __sym6.take().unwrap();
                let __sym7 = __sym7.take().unwrap();
                let __sym8 = __sym8.take().unwrap();
                let __sym9 = __sym9.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym9.2.clone();
                let __nt = super::__action28(input, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5, __sym6, __sym7, __sym8, __sym9);
                let __nt = __Nonterminal::exp((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state261<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, E, usize)>,
        __sym5: &mut Option<(usize, &'input str, usize)>,
        __sym6: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (26, __tok0), __loc2)) => {
                let mut __sym7 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state268(input, __tokens, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5, __sym6, __sym7));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state262<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, E, usize)>,
        __sym5: &mut Option<(usize, &'input str, usize)>,
        __sym6: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (26, __tok0), __loc2)) => {
                let mut __sym7 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state269(input, __tokens, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5, __sym6, __sym7));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state263<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, E, usize)>,
        __sym5: &mut Option<(usize, &'input str, usize)>,
        __sym6: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (26, __tok0), __loc2)) => {
                let mut __sym7 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state270(input, __tokens, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5, __sym6, __sym7));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state264<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, Vec<T>, usize)>,
        __sym2: &mut Option<(usize, &'input str, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, T, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((_, (26, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __sym2 = __sym2.take().unwrap();
                let __sym3 = __sym3.take().unwrap();
                let __sym4 = __sym4.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym4.2.clone();
                let __nt = super::__action11(input, __sym0, __sym1, __sym2, __sym3, __sym4);
                let __nt = __Nonterminal::ty((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state265<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, E, usize)>,
        __sym5: &mut Option<(usize, &'input str, usize)>,
        __sym6: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (26, __tok0), __loc2)) => {
                let mut __sym7 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state271(input, __tokens, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5, __sym6, __sym7));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state266<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, E, usize)>,
        __sym5: &mut Option<(usize, &'input str, usize)>,
        __sym6: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (26, __tok0), __loc2)) => {
                let mut __sym7 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state272(input, __tokens, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5, __sym6, __sym7));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state267<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, E, usize)>,
        __sym5: &mut Option<(usize, &'input str, usize)>,
        __sym6: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (26, __tok0), __loc2)) => {
                let mut __sym7 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state273(input, __tokens, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5, __sym6, __sym7));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state268<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, E, usize)>,
        __sym5: &mut Option<(usize, &'input str, usize)>,
        __sym6: &mut Option<(usize, &'input str, usize)>,
        __sym7: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (6, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state119(input, __tokens, __sym8));
            }
            Some((__loc1, (8, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state120(input, __tokens, __sym8));
            }
            Some((__loc1, (11, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state121(input, __tokens, __sym8));
            }
            Some((__loc1, (12, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state122(input, __tokens, __sym8));
            }
            Some((__loc1, (13, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state123(input, __tokens, __sym8));
            }
            Some((__loc1, (15, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state124(input, __tokens, __sym8));
            }
            Some((__loc1, (16, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state125(input, __tokens, __sym8));
            }
            Some((__loc1, (17, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state126(input, __tokens, __sym8));
            }
            Some((__loc1, (18, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state127(input, __tokens, __sym8));
            }
            Some((__loc1, (19, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state128(input, __tokens, __sym8));
            }
            Some((__loc1, (20, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state129(input, __tokens, __sym8));
            }
            Some((__loc1, (21, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state130(input, __tokens, __sym8));
            }
            Some((__loc1, (22, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state131(input, __tokens, __sym8));
            }
            Some((__loc1, (23, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state132(input, __tokens, __sym8));
            }
            Some((__loc1, (24, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state133(input, __tokens, __sym8));
            }
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state23(input, __tokens, __sym8));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym7.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::exp(__nt) => {
                    let __sym8 = &mut Some(__nt);
                    __result = try!(__state116(input, __tokens, __lookahead, __sym8));
                }
                __Nonterminal::ident(__nt) => {
                    let __sym8 = &mut Some(__nt);
                    __result = try!(__state117(input, __tokens, __lookahead, __sym8));
                }
                __Nonterminal::term(__nt) => {
                    let __sym8 = &mut Some(__nt);
                    __result = try!(__state274(input, __tokens, __lookahead, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5, __sym6, __sym7, __sym8));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state269<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, E, usize)>,
        __sym5: &mut Option<(usize, &'input str, usize)>,
        __sym6: &mut Option<(usize, &'input str, usize)>,
        __sym7: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (6, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state119(input, __tokens, __sym8));
            }
            Some((__loc1, (8, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state120(input, __tokens, __sym8));
            }
            Some((__loc1, (11, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state121(input, __tokens, __sym8));
            }
            Some((__loc1, (12, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state122(input, __tokens, __sym8));
            }
            Some((__loc1, (13, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state123(input, __tokens, __sym8));
            }
            Some((__loc1, (15, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state124(input, __tokens, __sym8));
            }
            Some((__loc1, (16, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state125(input, __tokens, __sym8));
            }
            Some((__loc1, (17, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state126(input, __tokens, __sym8));
            }
            Some((__loc1, (18, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state127(input, __tokens, __sym8));
            }
            Some((__loc1, (19, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state128(input, __tokens, __sym8));
            }
            Some((__loc1, (20, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state129(input, __tokens, __sym8));
            }
            Some((__loc1, (21, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state130(input, __tokens, __sym8));
            }
            Some((__loc1, (22, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state131(input, __tokens, __sym8));
            }
            Some((__loc1, (23, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state132(input, __tokens, __sym8));
            }
            Some((__loc1, (24, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state133(input, __tokens, __sym8));
            }
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state23(input, __tokens, __sym8));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym7.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::exp(__nt) => {
                    let __sym8 = &mut Some(__nt);
                    __result = try!(__state116(input, __tokens, __lookahead, __sym8));
                }
                __Nonterminal::ident(__nt) => {
                    let __sym8 = &mut Some(__nt);
                    __result = try!(__state117(input, __tokens, __lookahead, __sym8));
                }
                __Nonterminal::term(__nt) => {
                    let __sym8 = &mut Some(__nt);
                    __result = try!(__state275(input, __tokens, __lookahead, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5, __sym6, __sym7, __sym8));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state270<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, E, usize)>,
        __sym5: &mut Option<(usize, &'input str, usize)>,
        __sym6: &mut Option<(usize, &'input str, usize)>,
        __sym7: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (6, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state119(input, __tokens, __sym8));
            }
            Some((__loc1, (8, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state120(input, __tokens, __sym8));
            }
            Some((__loc1, (11, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state121(input, __tokens, __sym8));
            }
            Some((__loc1, (12, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state122(input, __tokens, __sym8));
            }
            Some((__loc1, (13, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state123(input, __tokens, __sym8));
            }
            Some((__loc1, (15, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state124(input, __tokens, __sym8));
            }
            Some((__loc1, (16, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state125(input, __tokens, __sym8));
            }
            Some((__loc1, (17, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state126(input, __tokens, __sym8));
            }
            Some((__loc1, (18, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state127(input, __tokens, __sym8));
            }
            Some((__loc1, (19, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state128(input, __tokens, __sym8));
            }
            Some((__loc1, (20, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state129(input, __tokens, __sym8));
            }
            Some((__loc1, (21, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state130(input, __tokens, __sym8));
            }
            Some((__loc1, (22, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state131(input, __tokens, __sym8));
            }
            Some((__loc1, (23, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state132(input, __tokens, __sym8));
            }
            Some((__loc1, (24, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state133(input, __tokens, __sym8));
            }
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state23(input, __tokens, __sym8));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym7.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::exp(__nt) => {
                    let __sym8 = &mut Some(__nt);
                    __result = try!(__state116(input, __tokens, __lookahead, __sym8));
                }
                __Nonterminal::ident(__nt) => {
                    let __sym8 = &mut Some(__nt);
                    __result = try!(__state117(input, __tokens, __lookahead, __sym8));
                }
                __Nonterminal::term(__nt) => {
                    let __sym8 = &mut Some(__nt);
                    __result = try!(__state276(input, __tokens, __lookahead, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5, __sym6, __sym7, __sym8));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state271<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, E, usize)>,
        __sym5: &mut Option<(usize, &'input str, usize)>,
        __sym6: &mut Option<(usize, &'input str, usize)>,
        __sym7: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (6, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state119(input, __tokens, __sym8));
            }
            Some((__loc1, (8, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state120(input, __tokens, __sym8));
            }
            Some((__loc1, (11, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state121(input, __tokens, __sym8));
            }
            Some((__loc1, (12, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state122(input, __tokens, __sym8));
            }
            Some((__loc1, (13, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state123(input, __tokens, __sym8));
            }
            Some((__loc1, (15, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state124(input, __tokens, __sym8));
            }
            Some((__loc1, (16, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state125(input, __tokens, __sym8));
            }
            Some((__loc1, (17, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state126(input, __tokens, __sym8));
            }
            Some((__loc1, (18, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state127(input, __tokens, __sym8));
            }
            Some((__loc1, (19, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state128(input, __tokens, __sym8));
            }
            Some((__loc1, (20, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state129(input, __tokens, __sym8));
            }
            Some((__loc1, (21, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state130(input, __tokens, __sym8));
            }
            Some((__loc1, (22, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state131(input, __tokens, __sym8));
            }
            Some((__loc1, (23, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state132(input, __tokens, __sym8));
            }
            Some((__loc1, (24, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state133(input, __tokens, __sym8));
            }
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state23(input, __tokens, __sym8));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym7.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::exp(__nt) => {
                    let __sym8 = &mut Some(__nt);
                    __result = try!(__state116(input, __tokens, __lookahead, __sym8));
                }
                __Nonterminal::ident(__nt) => {
                    let __sym8 = &mut Some(__nt);
                    __result = try!(__state117(input, __tokens, __lookahead, __sym8));
                }
                __Nonterminal::term(__nt) => {
                    let __sym8 = &mut Some(__nt);
                    __result = try!(__state277(input, __tokens, __lookahead, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5, __sym6, __sym7, __sym8));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state272<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, E, usize)>,
        __sym5: &mut Option<(usize, &'input str, usize)>,
        __sym6: &mut Option<(usize, &'input str, usize)>,
        __sym7: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (6, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state119(input, __tokens, __sym8));
            }
            Some((__loc1, (8, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state120(input, __tokens, __sym8));
            }
            Some((__loc1, (11, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state121(input, __tokens, __sym8));
            }
            Some((__loc1, (12, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state122(input, __tokens, __sym8));
            }
            Some((__loc1, (13, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state123(input, __tokens, __sym8));
            }
            Some((__loc1, (15, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state124(input, __tokens, __sym8));
            }
            Some((__loc1, (16, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state125(input, __tokens, __sym8));
            }
            Some((__loc1, (17, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state126(input, __tokens, __sym8));
            }
            Some((__loc1, (18, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state127(input, __tokens, __sym8));
            }
            Some((__loc1, (19, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state128(input, __tokens, __sym8));
            }
            Some((__loc1, (20, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state129(input, __tokens, __sym8));
            }
            Some((__loc1, (21, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state130(input, __tokens, __sym8));
            }
            Some((__loc1, (22, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state131(input, __tokens, __sym8));
            }
            Some((__loc1, (23, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state132(input, __tokens, __sym8));
            }
            Some((__loc1, (24, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state133(input, __tokens, __sym8));
            }
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state23(input, __tokens, __sym8));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym7.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::exp(__nt) => {
                    let __sym8 = &mut Some(__nt);
                    __result = try!(__state116(input, __tokens, __lookahead, __sym8));
                }
                __Nonterminal::ident(__nt) => {
                    let __sym8 = &mut Some(__nt);
                    __result = try!(__state117(input, __tokens, __lookahead, __sym8));
                }
                __Nonterminal::term(__nt) => {
                    let __sym8 = &mut Some(__nt);
                    __result = try!(__state278(input, __tokens, __lookahead, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5, __sym6, __sym7, __sym8));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state273<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, E, usize)>,
        __sym5: &mut Option<(usize, &'input str, usize)>,
        __sym6: &mut Option<(usize, &'input str, usize)>,
        __sym7: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (6, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state119(input, __tokens, __sym8));
            }
            Some((__loc1, (8, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state120(input, __tokens, __sym8));
            }
            Some((__loc1, (11, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state121(input, __tokens, __sym8));
            }
            Some((__loc1, (12, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state122(input, __tokens, __sym8));
            }
            Some((__loc1, (13, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state123(input, __tokens, __sym8));
            }
            Some((__loc1, (15, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state124(input, __tokens, __sym8));
            }
            Some((__loc1, (16, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state125(input, __tokens, __sym8));
            }
            Some((__loc1, (17, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state126(input, __tokens, __sym8));
            }
            Some((__loc1, (18, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state127(input, __tokens, __sym8));
            }
            Some((__loc1, (19, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state128(input, __tokens, __sym8));
            }
            Some((__loc1, (20, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state129(input, __tokens, __sym8));
            }
            Some((__loc1, (21, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state130(input, __tokens, __sym8));
            }
            Some((__loc1, (22, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state131(input, __tokens, __sym8));
            }
            Some((__loc1, (23, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state132(input, __tokens, __sym8));
            }
            Some((__loc1, (24, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state133(input, __tokens, __sym8));
            }
            Some((__loc1, (29, __tok0), __loc2)) => {
                let mut __sym8 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state23(input, __tokens, __sym8));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym7.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::exp(__nt) => {
                    let __sym8 = &mut Some(__nt);
                    __result = try!(__state116(input, __tokens, __lookahead, __sym8));
                }
                __Nonterminal::ident(__nt) => {
                    let __sym8 = &mut Some(__nt);
                    __result = try!(__state117(input, __tokens, __lookahead, __sym8));
                }
                __Nonterminal::term(__nt) => {
                    let __sym8 = &mut Some(__nt);
                    __result = try!(__state279(input, __tokens, __lookahead, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5, __sym6, __sym7, __sym8));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state274<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, E, usize)>,
        __sym5: &mut Option<(usize, &'input str, usize)>,
        __sym6: &mut Option<(usize, &'input str, usize)>,
        __sym7: &mut Option<(usize, &'input str, usize)>,
        __sym8: &mut Option<(usize, E, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (27, __tok0), __loc2)) => {
                let mut __sym9 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state280(input, __tokens, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5, __sym6, __sym7, __sym8, __sym9));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state275<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, E, usize)>,
        __sym5: &mut Option<(usize, &'input str, usize)>,
        __sym6: &mut Option<(usize, &'input str, usize)>,
        __sym7: &mut Option<(usize, &'input str, usize)>,
        __sym8: &mut Option<(usize, E, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (27, __tok0), __loc2)) => {
                let mut __sym9 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state281(input, __tokens, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5, __sym6, __sym7, __sym8, __sym9));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state276<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, E, usize)>,
        __sym5: &mut Option<(usize, &'input str, usize)>,
        __sym6: &mut Option<(usize, &'input str, usize)>,
        __sym7: &mut Option<(usize, &'input str, usize)>,
        __sym8: &mut Option<(usize, E, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (27, __tok0), __loc2)) => {
                let mut __sym9 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state282(input, __tokens, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5, __sym6, __sym7, __sym8, __sym9));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state277<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, E, usize)>,
        __sym5: &mut Option<(usize, &'input str, usize)>,
        __sym6: &mut Option<(usize, &'input str, usize)>,
        __sym7: &mut Option<(usize, &'input str, usize)>,
        __sym8: &mut Option<(usize, E, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (27, __tok0), __loc2)) => {
                let mut __sym9 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state283(input, __tokens, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5, __sym6, __sym7, __sym8, __sym9));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state278<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, E, usize)>,
        __sym5: &mut Option<(usize, &'input str, usize)>,
        __sym6: &mut Option<(usize, &'input str, usize)>,
        __sym7: &mut Option<(usize, &'input str, usize)>,
        __sym8: &mut Option<(usize, E, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (27, __tok0), __loc2)) => {
                let mut __sym9 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state284(input, __tokens, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5, __sym6, __sym7, __sym8, __sym9));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state279<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, E, usize)>,
        __sym5: &mut Option<(usize, &'input str, usize)>,
        __sym6: &mut Option<(usize, &'input str, usize)>,
        __sym7: &mut Option<(usize, &'input str, usize)>,
        __sym8: &mut Option<(usize, E, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (27, __tok0), __loc2)) => {
                let mut __sym9 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state285(input, __tokens, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5, __sym6, __sym7, __sym8, __sym9));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state280<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, E, usize)>,
        __sym5: &mut Option<(usize, &'input str, usize)>,
        __sym6: &mut Option<(usize, &'input str, usize)>,
        __sym7: &mut Option<(usize, &'input str, usize)>,
        __sym8: &mut Option<(usize, E, usize)>,
        __sym9: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((_, (6, _), _)) |
            Some((_, (8, _), _)) |
            Some((_, (11, _), _)) |
            Some((_, (12, _), _)) |
            Some((_, (13, _), _)) |
            Some((_, (15, _), _)) |
            Some((_, (16, _), _)) |
            Some((_, (17, _), _)) |
            Some((_, (18, _), _)) |
            Some((_, (19, _), _)) |
            Some((_, (20, _), _)) |
            Some((_, (21, _), _)) |
            Some((_, (22, _), _)) |
            Some((_, (23, _), _)) |
            Some((_, (24, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __sym2 = __sym2.take().unwrap();
                let __sym3 = __sym3.take().unwrap();
                let __sym4 = __sym4.take().unwrap();
                let __sym5 = __sym5.take().unwrap();
                let __sym6 = __sym6.take().unwrap();
                let __sym7 = __sym7.take().unwrap();
                let __sym8 = __sym8.take().unwrap();
                let __sym9 = __sym9.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym9.2.clone();
                let __nt = super::__action27(input, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5, __sym6, __sym7, __sym8, __sym9);
                let __nt = __Nonterminal::exp((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state281<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, E, usize)>,
        __sym5: &mut Option<(usize, &'input str, usize)>,
        __sym6: &mut Option<(usize, &'input str, usize)>,
        __sym7: &mut Option<(usize, &'input str, usize)>,
        __sym8: &mut Option<(usize, E, usize)>,
        __sym9: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((_, (6, _), _)) |
            Some((_, (8, _), _)) |
            Some((_, (11, _), _)) |
            Some((_, (12, _), _)) |
            Some((_, (13, _), _)) |
            Some((_, (15, _), _)) |
            Some((_, (16, _), _)) |
            Some((_, (17, _), _)) |
            Some((_, (18, _), _)) |
            Some((_, (19, _), _)) |
            Some((_, (20, _), _)) |
            Some((_, (21, _), _)) |
            Some((_, (22, _), _)) |
            Some((_, (23, _), _)) |
            Some((_, (24, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __sym2 = __sym2.take().unwrap();
                let __sym3 = __sym3.take().unwrap();
                let __sym4 = __sym4.take().unwrap();
                let __sym5 = __sym5.take().unwrap();
                let __sym6 = __sym6.take().unwrap();
                let __sym7 = __sym7.take().unwrap();
                let __sym8 = __sym8.take().unwrap();
                let __sym9 = __sym9.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym9.2.clone();
                let __nt = super::__action29(input, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5, __sym6, __sym7, __sym8, __sym9);
                let __nt = __Nonterminal::exp((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state282<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, E, usize)>,
        __sym5: &mut Option<(usize, &'input str, usize)>,
        __sym6: &mut Option<(usize, &'input str, usize)>,
        __sym7: &mut Option<(usize, &'input str, usize)>,
        __sym8: &mut Option<(usize, E, usize)>,
        __sym9: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((_, (6, _), _)) |
            Some((_, (8, _), _)) |
            Some((_, (11, _), _)) |
            Some((_, (12, _), _)) |
            Some((_, (13, _), _)) |
            Some((_, (15, _), _)) |
            Some((_, (16, _), _)) |
            Some((_, (17, _), _)) |
            Some((_, (18, _), _)) |
            Some((_, (19, _), _)) |
            Some((_, (20, _), _)) |
            Some((_, (21, _), _)) |
            Some((_, (22, _), _)) |
            Some((_, (23, _), _)) |
            Some((_, (24, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __sym2 = __sym2.take().unwrap();
                let __sym3 = __sym3.take().unwrap();
                let __sym4 = __sym4.take().unwrap();
                let __sym5 = __sym5.take().unwrap();
                let __sym6 = __sym6.take().unwrap();
                let __sym7 = __sym7.take().unwrap();
                let __sym8 = __sym8.take().unwrap();
                let __sym9 = __sym9.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym9.2.clone();
                let __nt = super::__action28(input, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5, __sym6, __sym7, __sym8, __sym9);
                let __nt = __Nonterminal::exp((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state283<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, E, usize)>,
        __sym5: &mut Option<(usize, &'input str, usize)>,
        __sym6: &mut Option<(usize, &'input str, usize)>,
        __sym7: &mut Option<(usize, &'input str, usize)>,
        __sym8: &mut Option<(usize, E, usize)>,
        __sym9: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((_, (6, _), _)) |
            Some((_, (8, _), _)) |
            Some((_, (11, _), _)) |
            Some((_, (12, _), _)) |
            Some((_, (13, _), _)) |
            Some((_, (15, _), _)) |
            Some((_, (16, _), _)) |
            Some((_, (17, _), _)) |
            Some((_, (18, _), _)) |
            Some((_, (19, _), _)) |
            Some((_, (20, _), _)) |
            Some((_, (21, _), _)) |
            Some((_, (22, _), _)) |
            Some((_, (23, _), _)) |
            Some((_, (24, _), _)) |
            Some((_, (27, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __sym2 = __sym2.take().unwrap();
                let __sym3 = __sym3.take().unwrap();
                let __sym4 = __sym4.take().unwrap();
                let __sym5 = __sym5.take().unwrap();
                let __sym6 = __sym6.take().unwrap();
                let __sym7 = __sym7.take().unwrap();
                let __sym8 = __sym8.take().unwrap();
                let __sym9 = __sym9.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym9.2.clone();
                let __nt = super::__action27(input, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5, __sym6, __sym7, __sym8, __sym9);
                let __nt = __Nonterminal::exp((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state284<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, E, usize)>,
        __sym5: &mut Option<(usize, &'input str, usize)>,
        __sym6: &mut Option<(usize, &'input str, usize)>,
        __sym7: &mut Option<(usize, &'input str, usize)>,
        __sym8: &mut Option<(usize, E, usize)>,
        __sym9: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((_, (6, _), _)) |
            Some((_, (8, _), _)) |
            Some((_, (11, _), _)) |
            Some((_, (12, _), _)) |
            Some((_, (13, _), _)) |
            Some((_, (15, _), _)) |
            Some((_, (16, _), _)) |
            Some((_, (17, _), _)) |
            Some((_, (18, _), _)) |
            Some((_, (19, _), _)) |
            Some((_, (20, _), _)) |
            Some((_, (21, _), _)) |
            Some((_, (22, _), _)) |
            Some((_, (23, _), _)) |
            Some((_, (24, _), _)) |
            Some((_, (27, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __sym2 = __sym2.take().unwrap();
                let __sym3 = __sym3.take().unwrap();
                let __sym4 = __sym4.take().unwrap();
                let __sym5 = __sym5.take().unwrap();
                let __sym6 = __sym6.take().unwrap();
                let __sym7 = __sym7.take().unwrap();
                let __sym8 = __sym8.take().unwrap();
                let __sym9 = __sym9.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym9.2.clone();
                let __nt = super::__action29(input, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5, __sym6, __sym7, __sym8, __sym9);
                let __nt = __Nonterminal::exp((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state285<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, String, usize)>,
        __sym2: &mut Option<(usize, IdOrImm, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, E, usize)>,
        __sym5: &mut Option<(usize, &'input str, usize)>,
        __sym6: &mut Option<(usize, &'input str, usize)>,
        __sym7: &mut Option<(usize, &'input str, usize)>,
        __sym8: &mut Option<(usize, E, usize)>,
        __sym9: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((_, (6, _), _)) |
            Some((_, (8, _), _)) |
            Some((_, (11, _), _)) |
            Some((_, (12, _), _)) |
            Some((_, (13, _), _)) |
            Some((_, (15, _), _)) |
            Some((_, (16, _), _)) |
            Some((_, (17, _), _)) |
            Some((_, (18, _), _)) |
            Some((_, (19, _), _)) |
            Some((_, (20, _), _)) |
            Some((_, (21, _), _)) |
            Some((_, (22, _), _)) |
            Some((_, (23, _), _)) |
            Some((_, (24, _), _)) |
            Some((_, (27, _), _)) |
            Some((_, (29, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __sym2 = __sym2.take().unwrap();
                let __sym3 = __sym3.take().unwrap();
                let __sym4 = __sym4.take().unwrap();
                let __sym5 = __sym5.take().unwrap();
                let __sym6 = __sym6.take().unwrap();
                let __sym7 = __sym7.take().unwrap();
                let __sym8 = __sym8.take().unwrap();
                let __sym9 = __sym9.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym9.2.clone();
                let __nt = super::__action28(input, __sym0, __sym1, __sym2, __sym3, __sym4, __sym5, __sym6, __sym7, __sym8, __sym9);
                let __nt = __Nonterminal::exp((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }
}
pub use self::__parse__program::parse_program;

mod __parse__ty {
    #![allow(non_snake_case, non_camel_case_types, unused_mut, unused_variables, unused_imports)]

    use std::str::FromStr;
    use ast::*;
    extern crate lalrpop_util as __lalrpop_util;
    use self::__lalrpop_util::ParseError as __ParseError;
    pub fn parse_ty<
        'input,
    >(
        input: &'input str,
    ) -> Result<T, __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __tokens = super::__intern_token::__Matcher::new(input);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match try!(__state0(input, &mut __tokens, __lookahead)) {
            (Some(__lookahead), _) => {
                Err(__ParseError::ExtraToken { token: __lookahead })
            }
            (None, __Nonterminal::____ty((_, __nt, _))) => {
                Ok(__nt)
            }
            _ => unreachable!(),
        }
    }

    #[allow(dead_code)]
    pub enum __Nonterminal<> {
        _28_3cfundef_3e_29((usize, Fundef, usize)),
        _28_3cfundef_3e_29_2a((usize, ::std::vec::Vec<Fundef>, usize)),
        _28_3cfundef_3e_29_2b((usize, ::std::vec::Vec<Fundef>, usize)),
        _28_3cid__ty_3e_20_22_2c_22_29((usize, String, usize)),
        _28_3cid__ty_3e_20_22_2c_22_29_2a((usize, ::std::vec::Vec<String>, usize)),
        _28_3cid__ty_3e_20_22_2c_22_29_2b((usize, ::std::vec::Vec<String>, usize)),
        _28_3cty_3e_20_22_2c_22_29((usize, T, usize)),
        _28_3cty_3e_20_22_2c_22_29_2a((usize, ::std::vec::Vec<T>, usize)),
        _28_3cty_3e_20_22_2c_22_29_2b((usize, ::std::vec::Vec<T>, usize)),
        ____program((usize, Prog, usize)),
        ____ty((usize, T, usize)),
        args((usize, Vec<String>, usize)),
        exp((usize, Exp, usize)),
        fundef((usize, Fundef, usize)),
        id__or__imm((usize, IdOrImm, usize)),
        id__ty((usize, String, usize)),
        id__ty_3f((usize, ::std::option::Option<String>, usize)),
        ident((usize, String, usize)),
        int((usize, i32, usize)),
        program((usize, Prog, usize)),
        term((usize, E, usize)),
        ty((usize, T, usize)),
        ty_3f((usize, ::std::option::Option<T>, usize)),
        tys((usize, Vec<T>, usize)),
    }

    pub fn __state0<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (0, __tok0), __loc2)) => {
                let mut __sym0 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state2(input, __tokens, __sym0));
            }
            Some((__loc1, (7, __tok0), __loc2)) => {
                let mut __sym0 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state3(input, __tokens, __sym0));
            }
            Some((__loc1, (14, __tok0), __loc2)) => {
                let mut __sym0 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state4(input, __tokens, __sym0));
            }
            Some((__loc1, (25, __tok0), __loc2)) => {
                let mut __sym0 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state5(input, __tokens, __sym0));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        loop {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::ty(__nt) => {
                    let __sym0 = &mut Some(__nt);
                    __result = try!(__state1(input, __tokens, __lookahead, __sym0));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
    }

    pub fn __state1<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, T, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            None => {
                let __sym0 = __sym0.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action1(input, __sym0);
                let __nt = __Nonterminal::____ty((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state2<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (0, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state9(input, __tokens, __sym1));
            }
            Some((__loc1, (7, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state10(input, __tokens, __sym1));
            }
            Some((__loc1, (14, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state11(input, __tokens, __sym1));
            }
            Some((__loc1, (25, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state12(input, __tokens, __sym1));
            }
            Some((_, (1, _), _)) => {
                let __start = __sym0.as_ref().unwrap().2.clone();
                let __end = __lookahead.as_ref().map(|o| o.0.clone()).unwrap_or_else(|| __start.clone());
                let __nt = super::__action69(input, &__start, &__end);
                let __nt = __Nonterminal::tys((
                    __start,
                    __nt,
                    __end,
                ));
                __result = (__lookahead, __nt);
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::_28_3cty_3e_20_22_2c_22_29_2b(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state6(input, __tokens, __lookahead, __sym1));
                }
                __Nonterminal::ty(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state7(input, __tokens, __lookahead, __sym1));
                }
                __Nonterminal::tys(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state8(input, __tokens, __lookahead, __sym0, __sym1));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state3<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            None => {
                let __sym0 = __sym0.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action9(input, __sym0);
                let __nt = __Nonterminal::ty((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state4<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            None => {
                let __sym0 = __sym0.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action10(input, __sym0);
                let __nt = __Nonterminal::ty((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state5<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            None => {
                let __sym0 = __sym0.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action8(input, __sym0);
                let __nt = __Nonterminal::ty((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state6<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, ::std::vec::Vec<T>, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (0, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state9(input, __tokens, __sym1));
            }
            Some((__loc1, (7, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state10(input, __tokens, __sym1));
            }
            Some((__loc1, (14, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state11(input, __tokens, __sym1));
            }
            Some((__loc1, (25, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state12(input, __tokens, __sym1));
            }
            Some((_, (1, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action71(input, __sym0);
                let __nt = __Nonterminal::tys((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::ty(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state13(input, __tokens, __lookahead, __sym0, __sym1));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state7<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, T, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (2, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state14(input, __tokens, __sym0, __sym1));
            }
            Some((_, (1, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action68(input, __sym0);
                let __nt = __Nonterminal::tys((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state8<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, Vec<T>, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (1, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state15(input, __tokens, __sym0, __sym1, __sym2));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state9<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (0, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state9(input, __tokens, __sym1));
            }
            Some((__loc1, (7, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state10(input, __tokens, __sym1));
            }
            Some((__loc1, (14, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state11(input, __tokens, __sym1));
            }
            Some((__loc1, (25, __tok0), __loc2)) => {
                let mut __sym1 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state12(input, __tokens, __sym1));
            }
            Some((_, (1, _), _)) => {
                let __start = __sym0.as_ref().unwrap().2.clone();
                let __end = __lookahead.as_ref().map(|o| o.0.clone()).unwrap_or_else(|| __start.clone());
                let __nt = super::__action69(input, &__start, &__end);
                let __nt = __Nonterminal::tys((
                    __start,
                    __nt,
                    __end,
                ));
                __result = (__lookahead, __nt);
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym0.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::_28_3cty_3e_20_22_2c_22_29_2b(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state6(input, __tokens, __lookahead, __sym1));
                }
                __Nonterminal::ty(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state7(input, __tokens, __lookahead, __sym1));
                }
                __Nonterminal::tys(__nt) => {
                    let __sym1 = &mut Some(__nt);
                    __result = try!(__state16(input, __tokens, __lookahead, __sym0, __sym1));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state10<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((_, (1, _), _)) |
            Some((_, (2, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action9(input, __sym0);
                let __nt = __Nonterminal::ty((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state11<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((_, (1, _), _)) |
            Some((_, (2, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action10(input, __sym0);
                let __nt = __Nonterminal::ty((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state12<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((_, (1, _), _)) |
            Some((_, (2, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym0.2.clone();
                let __nt = super::__action8(input, __sym0);
                let __nt = __Nonterminal::ty((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state13<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, ::std::vec::Vec<T>, usize)>,
        __sym1: &mut Option<(usize, T, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (2, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state17(input, __tokens, __sym0, __sym1, __sym2));
            }
            Some((_, (1, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action70(input, __sym0, __sym1);
                let __nt = __Nonterminal::tys((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state14<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, T, usize)>,
        __sym1: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((_, (0, _), _)) |
            Some((_, (1, _), _)) |
            Some((_, (7, _), _)) |
            Some((_, (14, _), _)) |
            Some((_, (25, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym1.2.clone();
                let __nt = super::__action60(input, __sym0, __sym1);
                let __nt = __Nonterminal::_28_3cty_3e_20_22_2c_22_29_2b((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state15<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, Vec<T>, usize)>,
        __sym2: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (3, __tok0), __loc2)) => {
                let mut __sym3 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state18(input, __tokens, __sym0, __sym1, __sym2, __sym3));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state16<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, Vec<T>, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((__loc1, (1, __tok0), __loc2)) => {
                let mut __sym2 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state19(input, __tokens, __sym0, __sym1, __sym2));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state17<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, ::std::vec::Vec<T>, usize)>,
        __sym1: &mut Option<(usize, T, usize)>,
        __sym2: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((_, (0, _), _)) |
            Some((_, (1, _), _)) |
            Some((_, (7, _), _)) |
            Some((_, (14, _), _)) |
            Some((_, (25, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __sym2 = __sym2.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym2.2.clone();
                let __nt = super::__action61(input, __sym0, __sym1, __sym2);
                let __nt = __Nonterminal::_28_3cty_3e_20_22_2c_22_29_2b((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state18<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, Vec<T>, usize)>,
        __sym2: &mut Option<(usize, &'input str, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (0, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state2(input, __tokens, __sym4));
            }
            Some((__loc1, (7, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state3(input, __tokens, __sym4));
            }
            Some((__loc1, (14, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state4(input, __tokens, __sym4));
            }
            Some((__loc1, (25, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state5(input, __tokens, __sym4));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym3.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::ty(__nt) => {
                    let __sym4 = &mut Some(__nt);
                    __result = try!(__state20(input, __tokens, __lookahead, __sym0, __sym1, __sym2, __sym3, __sym4));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state19<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, Vec<T>, usize)>,
        __sym2: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (3, __tok0), __loc2)) => {
                let mut __sym3 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state21(input, __tokens, __sym0, __sym1, __sym2, __sym3));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        return Ok(__result);
    }

    pub fn __state20<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, Vec<T>, usize)>,
        __sym2: &mut Option<(usize, &'input str, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, T, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            None => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __sym2 = __sym2.take().unwrap();
                let __sym3 = __sym3.take().unwrap();
                let __sym4 = __sym4.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym4.2.clone();
                let __nt = super::__action11(input, __sym0, __sym1, __sym2, __sym3, __sym4);
                let __nt = __Nonterminal::ty((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }

    pub fn __state21<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, Vec<T>, usize)>,
        __sym2: &mut Option<(usize, &'input str, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        let __lookahead = match __tokens.next() {
            Some(Ok(v)) => Some(v),
            None => None,
            Some(Err(e)) => return Err(e),
        };
        match __lookahead {
            Some((__loc1, (0, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state9(input, __tokens, __sym4));
            }
            Some((__loc1, (7, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state10(input, __tokens, __sym4));
            }
            Some((__loc1, (14, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state11(input, __tokens, __sym4));
            }
            Some((__loc1, (25, __tok0), __loc2)) => {
                let mut __sym4 = &mut Some((__loc1, (__tok0), __loc2));
                __result = try!(__state12(input, __tokens, __sym4));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
        while __sym3.is_some() {
            let (__lookahead, __nt) = __result;
            match __nt {
                __Nonterminal::ty(__nt) => {
                    let __sym4 = &mut Some(__nt);
                    __result = try!(__state22(input, __tokens, __lookahead, __sym0, __sym1, __sym2, __sym3, __sym4));
                }
                _ => {
                    return Ok((__lookahead, __nt));
                }
            }
        }
        return Ok(__result);
    }

    pub fn __state22<
        'input,
        __TOKENS: Iterator<Item=Result<(usize, (usize, &'input str), usize),__ParseError<usize,(usize, &'input str),()>>>,
    >(
        input: &'input str,
        __tokens: &mut __TOKENS,
        __lookahead: Option<(usize, (usize, &'input str), usize)>,
        __sym0: &mut Option<(usize, &'input str, usize)>,
        __sym1: &mut Option<(usize, Vec<T>, usize)>,
        __sym2: &mut Option<(usize, &'input str, usize)>,
        __sym3: &mut Option<(usize, &'input str, usize)>,
        __sym4: &mut Option<(usize, T, usize)>,
    ) -> Result<(Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>), __ParseError<usize,(usize, &'input str),()>>
    {
        let mut __result: (Option<(usize, (usize, &'input str), usize)>, __Nonterminal<>);
        match __lookahead {
            Some((_, (1, _), _)) |
            Some((_, (2, _), _)) => {
                let __sym0 = __sym0.take().unwrap();
                let __sym1 = __sym1.take().unwrap();
                let __sym2 = __sym2.take().unwrap();
                let __sym3 = __sym3.take().unwrap();
                let __sym4 = __sym4.take().unwrap();
                let __start = __sym0.0.clone();
                let __end = __sym4.2.clone();
                let __nt = super::__action11(input, __sym0, __sym1, __sym2, __sym3, __sym4);
                let __nt = __Nonterminal::ty((
                    __start,
                    __nt,
                    __end,
                ));
                return Ok((__lookahead, __nt));
            }
            _ => {
                return Err(__ParseError::UnrecognizedToken {
                    token: __lookahead,
                    expected: vec![],
                });
            }
        }
    }
}
pub use self::__parse__ty::parse_ty;
mod __intern_token {
    extern crate lalrpop_util as __lalrpop_util;
    use self::__lalrpop_util::ParseError as __ParseError;
    pub struct __Matcher<'input> {
        text: &'input str,
        consumed: usize,
    }

    fn __tokenize(text: &str) -> Option<(usize, usize)> {
        let mut __chars = text.char_indices();
        let mut __current_match: Option<(usize, usize)> = None;
        let mut __current_state: usize = 0;
        loop {
            match __current_state {
                0 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        40 => /* '(' */ {
                            __current_match = Some((0, __index + 1));
                            __current_state = 1;
                            continue;
                        }
                        41 => /* ')' */ {
                            __current_match = Some((1, __index + 1));
                            __current_state = 2;
                            continue;
                        }
                        44 => /* ',' */ {
                            __current_match = Some((2, __index + 1));
                            __current_state = 3;
                            continue;
                        }
                        45 => /* '-' */ {
                            __current_state = 4;
                            continue;
                        }
                        48 ... 57 => {
                            __current_match = Some((28, __index + __ch.len_utf8()));
                            __current_state = 5;
                            continue;
                        }
                        58 => /* ':' */ {
                            __current_match = Some((4, __index + 1));
                            __current_state = 6;
                            continue;
                        }
                        61 => /* '=' */ {
                            __current_match = Some((5, __index + 1));
                            __current_state = 7;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 8;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 8;
                            continue;
                        }
                        97 => /* 'a' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 9;
                            continue;
                        }
                        98 => /* 'b' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 10;
                            continue;
                        }
                        99 => /* 'c' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 11;
                            continue;
                        }
                        100 => /* 'd' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 12;
                            continue;
                        }
                        101 => /* 'e' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 13;
                            continue;
                        }
                        102 ... 104 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 8;
                            continue;
                        }
                        105 => /* 'i' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 14;
                            continue;
                        }
                        106 ... 107 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 8;
                            continue;
                        }
                        108 => /* 'l' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 15;
                            continue;
                        }
                        109 => /* 'm' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 16;
                            continue;
                        }
                        110 => /* 'n' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 17;
                            continue;
                        }
                        111 ... 113 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 8;
                            continue;
                        }
                        114 => /* 'r' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 18;
                            continue;
                        }
                        115 => /* 's' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 19;
                            continue;
                        }
                        116 => /* 't' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 8;
                            continue;
                        }
                        117 => /* 'u' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 20;
                            continue;
                        }
                        118 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 8;
                            continue;
                        }
                        123 => /* '{' */ {
                            __current_match = Some((26, __index + 1));
                            __current_state = 21;
                            continue;
                        }
                        125 => /* '}' */ {
                            __current_match = Some((27, __index + 1));
                            __current_state = 22;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                1 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        _ => {
                            return __current_match;
                        }
                    }
                }
                2 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        _ => {
                            return __current_match;
                        }
                    }
                }
                3 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        _ => {
                            return __current_match;
                        }
                    }
                }
                4 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        62 => /* '>' */ {
                            __current_match = Some((3, __index + 1));
                            __current_state = 24;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                5 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((28, __index + __ch.len_utf8()));
                            __current_state = 25;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                6 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        _ => {
                            return __current_match;
                        }
                    }
                }
                7 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        _ => {
                            return __current_match;
                        }
                    }
                }
                8 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                9 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 ... 99 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        100 => /* 'd' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 27;
                            continue;
                        }
                        101 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                10 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 ... 110 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        111 => /* 'o' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 28;
                            continue;
                        }
                        112 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                11 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 => /* 'a' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 29;
                            continue;
                        }
                        98 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                12 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 ... 100 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        101 => /* 'e' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 30;
                            continue;
                        }
                        102 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                13 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 ... 107 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        108 => /* 'l' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 31;
                            continue;
                        }
                        109 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                14 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 ... 101 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        102 => /* 'f' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 32;
                            continue;
                        }
                        103 ... 109 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        110 => /* 'n' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 33;
                            continue;
                        }
                        111 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                15 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 ... 99 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        100 => /* 'd' */ {
                            __current_match = Some((15, __index + 1));
                            __current_state = 34;
                            continue;
                        }
                        101 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                16 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 ... 110 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        111 => /* 'o' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 35;
                            continue;
                        }
                        112 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                17 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 ... 100 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        101 => /* 'e' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 36;
                            continue;
                        }
                        102 ... 110 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        111 => /* 'o' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 37;
                            continue;
                        }
                        112 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                18 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 ... 100 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        101 => /* 'e' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 38;
                            continue;
                        }
                        102 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                19 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 => /* 'a' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 39;
                            continue;
                        }
                        98 ... 100 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        101 => /* 'e' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 40;
                            continue;
                        }
                        102 ... 115 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        116 => /* 't' */ {
                            __current_match = Some((23, __index + 1));
                            __current_state = 41;
                            continue;
                        }
                        117 => /* 'u' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 42;
                            continue;
                        }
                        118 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                20 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 ... 109 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        110 => /* 'n' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 43;
                            continue;
                        }
                        111 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                21 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        _ => {
                            return __current_match;
                        }
                    }
                }
                22 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        _ => {
                            return __current_match;
                        }
                    }
                }
                23 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        _ => {
                            return __current_match;
                        }
                    }
                }
                24 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        _ => {
                            return __current_match;
                        }
                    }
                }
                25 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((28, __index + __ch.len_utf8()));
                            __current_state = 25;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                26 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                27 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 ... 99 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        100 => /* 'd' */ {
                            __current_match = Some((6, __index + 1));
                            __current_state = 44;
                            continue;
                        }
                        101 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                28 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 ... 110 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        111 => /* 'o' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 45;
                            continue;
                        }
                        112 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                29 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 ... 107 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        108 => /* 'l' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 46;
                            continue;
                        }
                        109 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                30 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 ... 101 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        102 => /* 'f' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 47;
                            continue;
                        }
                        103 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                31 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 ... 114 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        115 => /* 's' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 48;
                            continue;
                        }
                        116 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                32 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 ... 100 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        101 => /* 'e' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 49;
                            continue;
                        }
                        102 => /* 'f' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        103 => /* 'g' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 50;
                            continue;
                        }
                        104 ... 107 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        108 => /* 'l' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 51;
                            continue;
                        }
                        109 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                33 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 ... 115 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        116 => /* 't' */ {
                            __current_match = Some((14, __index + 1));
                            __current_state = 52;
                            continue;
                        }
                        117 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                34 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                35 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 ... 117 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        118 => /* 'v' */ {
                            __current_match = Some((16, __index + 1));
                            __current_state = 53;
                            continue;
                        }
                        119 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                36 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 ... 102 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        103 => /* 'g' */ {
                            __current_match = Some((17, __index + 1));
                            __current_state = 54;
                            continue;
                        }
                        104 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                37 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 ... 111 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        112 => /* 'p' */ {
                            __current_match = Some((18, __index + 1));
                            __current_state = 55;
                            continue;
                        }
                        113 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                38 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 ... 114 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        115 => /* 's' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 56;
                            continue;
                        }
                        116 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                39 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 ... 117 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        118 => /* 'v' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 57;
                            continue;
                        }
                        119 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                40 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 ... 115 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        116 => /* 't' */ {
                            __current_match = Some((21, __index + 1));
                            __current_state = 58;
                            continue;
                        }
                        117 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                41 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                42 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 => /* 'a' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        98 => /* 'b' */ {
                            __current_match = Some((24, __index + 1));
                            __current_state = 59;
                            continue;
                        }
                        99 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                43 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 ... 104 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        105 => /* 'i' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 60;
                            continue;
                        }
                        106 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                44 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                45 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 ... 107 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        108 => /* 'l' */ {
                            __current_match = Some((7, __index + 1));
                            __current_state = 61;
                            continue;
                        }
                        109 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                46 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 ... 107 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        108 => /* 'l' */ {
                            __current_match = Some((8, __index + 1));
                            __current_state = 62;
                            continue;
                        }
                        109 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                47 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 ... 104 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        105 => /* 'i' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 63;
                            continue;
                        }
                        106 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                48 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 ... 100 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        101 => /* 'e' */ {
                            __current_match = Some((10, __index + 1));
                            __current_state = 64;
                            continue;
                        }
                        102 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                49 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 ... 112 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        113 => /* 'q' */ {
                            __current_match = Some((11, __index + 1));
                            __current_state = 65;
                            continue;
                        }
                        114 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                50 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 ... 100 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        101 => /* 'e' */ {
                            __current_match = Some((12, __index + 1));
                            __current_state = 66;
                            continue;
                        }
                        102 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                51 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 ... 100 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        101 => /* 'e' */ {
                            __current_match = Some((13, __index + 1));
                            __current_state = 67;
                            continue;
                        }
                        102 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                52 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                53 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                54 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                55 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                56 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 ... 115 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        116 => /* 't' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 68;
                            continue;
                        }
                        117 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                57 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 ... 100 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        101 => /* 'e' */ {
                            __current_match = Some((20, __index + 1));
                            __current_state = 69;
                            continue;
                        }
                        102 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                58 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 ... 107 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        108 => /* 'l' */ {
                            __current_match = Some((22, __index + 1));
                            __current_state = 70;
                            continue;
                        }
                        109 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                59 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                60 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 ... 115 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        116 => /* 't' */ {
                            __current_match = Some((25, __index + 1));
                            __current_state = 71;
                            continue;
                        }
                        117 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                61 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                62 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                63 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 ... 109 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        110 => /* 'n' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 72;
                            continue;
                        }
                        111 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                64 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                65 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                66 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                67 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                68 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 ... 110 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        111 => /* 'o' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 73;
                            continue;
                        }
                        112 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                69 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                70 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                71 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                72 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 ... 100 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        101 => /* 'e' */ {
                            __current_match = Some((9, __index + 1));
                            __current_state = 74;
                            continue;
                        }
                        102 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                73 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 ... 113 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        114 => /* 'r' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 75;
                            continue;
                        }
                        115 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                74 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                75 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 ... 100 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        101 => /* 'e' */ {
                            __current_match = Some((19, __index + 1));
                            __current_state = 76;
                            continue;
                        }
                        102 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                76 => {
                    let (__index, __ch) = match __chars.next() { Some(p) => p, None => return __current_match };
                    match __ch as u32 {
                        48 ... 57 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        65 ... 90 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        95 => /* '_' */ {
                            __current_match = Some((29, __index + 1));
                            __current_state = 26;
                            continue;
                        }
                        97 ... 122 => {
                            __current_match = Some((29, __index + __ch.len_utf8()));
                            __current_state = 26;
                            continue;
                        }
                        _ => {
                            return __current_match;
                        }
                    }
                }
                _ => { panic!("invalid state {}", __current_state); }
            }
        }
    }

    impl<'input> __Matcher<'input> {
        pub fn new(s: &'input str) -> __Matcher<'input> {
            __Matcher { text: s, consumed: 0 }
        }
    }

    impl<'input> Iterator for __Matcher<'input> {
        type Item = Result<(usize, (usize, &'input str), usize), __ParseError<usize,(usize, &'input str),()>>;

        fn next(&mut self) -> Option<Self::Item> {
            let __text = self.text.trim_left();
            let __whitespace = self.text.len() - __text.len();
            let __start_offset = self.consumed + __whitespace;
            if __text.is_empty() {
                self.text = __text;
                self.consumed = __start_offset;
                None
            } else {
                match __tokenize(__text) {
                    Some((__index, __length)) => {
                        let __result = &__text[..__length];
                        let __remaining = &__text[__length..];
                        let __end_offset = __start_offset + __length;
                        self.text = __remaining;
                        self.consumed = __end_offset;
                        Some(Ok((__start_offset, (__index, __result), __end_offset)))
                    }
                    None => {
                        Some(Err(__ParseError::InvalidToken { location: __start_offset }))
                    }
                }
            }
        }
    }
}

pub fn __action0<
    'input,
>(
    input: &'input str,
    (_, __0, _): (usize, Prog, usize),
) -> Prog
{
    (__0)
}

pub fn __action1<
    'input,
>(
    input: &'input str,
    (_, __0, _): (usize, T, usize),
) -> T
{
    (__0)
}

pub fn __action2<
    'input,
>(
    input: &'input str,
    (_, __0, _): (usize, &'input str, usize),
) -> i32
{
    i32::from_str(__0).unwrap()
}

pub fn __action3<
    'input,
>(
    input: &'input str,
    (_, __0, _): (usize, &'input str, usize),
) -> String
{
    match __0 {"_"=>genid("_tmp"),a=>format!("{}",a)}
}

pub fn __action4<
    'input,
>(
    input: &'input str,
    (_, __0, _): (usize, ::std::vec::Vec<Fundef>, usize),
    (_, __1, _): (usize, E, usize),
) -> Prog
{
    Prog::Prog(__0, __1)
}

pub fn __action5<
    'input,
>(
    input: &'input str,
    (_, _, _): (usize, &'input str, usize),
    (_, i, _): (usize, String, usize),
    (_, _, _): (usize, &'input str, usize),
    (_, a, _): (usize, Vec<String>, usize),
    (_, _, _): (usize, &'input str, usize),
    (_, _, _): (usize, &'input str, usize),
    (_, t, _): (usize, T, usize),
    (_, _, _): (usize, &'input str, usize),
    (_, e, _): (usize, E, usize),
    (_, _, _): (usize, &'input str, usize),
) -> Fundef
{
    Fundef::Fundef(i,a,e,t)
}

pub fn __action6<
    'input,
>(
    input: &'input str,
    (_, ts, _): (usize, ::std::vec::Vec<String>, usize),
    (_, t, _): (usize, ::std::option::Option<String>, usize),
) -> Vec<String>
{
    match t {
                                                        None => ts,
                                                        Some(t) => {let mut ts = ts; ts.push(t); ts} }
}

pub fn __action7<
    'input,
>(
    input: &'input str,
    (_, __0, _): (usize, String, usize),
    (_, _, _): (usize, &'input str, usize),
    (_, _, _): (usize, T, usize),
) -> String
{
    (__0)
}

pub fn __action8<
    'input,
>(
    input: &'input str,
    (_, __0, _): (usize, &'input str, usize),
) -> T
{
    T::Unit
}

pub fn __action9<
    'input,
>(
    input: &'input str,
    (_, __0, _): (usize, &'input str, usize),
) -> T
{
    T::Bool
}

pub fn __action10<
    'input,
>(
    input: &'input str,
    (_, __0, _): (usize, &'input str, usize),
) -> T
{
    T::Int
}

pub fn __action11<
    'input,
>(
    input: &'input str,
    (_, _, _): (usize, &'input str, usize),
    (_, ts, _): (usize, Vec<T>, usize),
    (_, _, _): (usize, &'input str, usize),
    (_, _, _): (usize, &'input str, usize),
    (_, t, _): (usize, T, usize),
) -> T
{
    T::Fun(ts, Box::new(t))
}

pub fn __action12<
    'input,
>(
    input: &'input str,
    (_, ts1, _): (usize, ::std::vec::Vec<T>, usize),
    (_, ty, _): (usize, ::std::option::Option<T>, usize),
) -> Vec<T>
{
    match ty {
                                                        Some(t) => { let mut ts = ts1; ts.push(t); ts }
                                                        None    => ts1, }
}

pub fn __action13<
    'input,
>(
    input: &'input str,
    (_, __0, _): (usize, String, usize),
) -> IdOrImm
{
    IdOrImm::V(__0)
}

pub fn __action14<
    'input,
>(
    input: &'input str,
    (_, __0, _): (usize, i32, usize),
) -> IdOrImm
{
    IdOrImm::C(__0)
}

pub fn __action15<
    'input,
>(
    input: &'input str,
    (_, i, _): (usize, String, usize),
    (_, _, _): (usize, &'input str, usize),
    (_, t, _): (usize, T, usize),
    (_, _, _): (usize, &'input str, usize),
    (_, e, _): (usize, Exp, usize),
    (_, ee, _): (usize, E, usize),
) -> E
{
    E::Let(i,t,e,Box::new(ee))
}

pub fn __action16<
    'input,
>(
    input: &'input str,
    (_, __0, _): (usize, Exp, usize),
) -> E
{
    E::Ans(__0)
}

pub fn __action17<
    'input,
>(
    input: &'input str,
    (_, __0, _): (usize, Exp, usize),
    (_, __1, _): (usize, E, usize),
) -> E
{
    seq(__0, __1)
}

pub fn __action18<
    'input,
>(
    input: &'input str,
    (_, __0, _): (usize, &'input str, usize),
) -> Exp
{
    Exp::Nop
}

pub fn __action19<
    'input,
>(
    input: &'input str,
    (_, _, _): (usize, &'input str, usize),
    (_, __0, _): (usize, i32, usize),
) -> Exp
{
    Exp::Set(__0)
}

pub fn __action20<
    'input,
>(
    input: &'input str,
    (_, _, _): (usize, &'input str, usize),
    (_, __0, _): (usize, String, usize),
) -> Exp
{
    Exp::SetL(__0)
}

pub fn __action21<
    'input,
>(
    input: &'input str,
    (_, _, _): (usize, &'input str, usize),
    (_, __0, _): (usize, String, usize),
) -> Exp
{
    Exp::Mov(__0)
}

pub fn __action22<
    'input,
>(
    input: &'input str,
    (_, _, _): (usize, &'input str, usize),
    (_, __0, _): (usize, String, usize),
) -> Exp
{
    Exp::Neg(__0)
}

pub fn __action23<
    'input,
>(
    input: &'input str,
    (_, _, _): (usize, &'input str, usize),
    (_, __0, _): (usize, String, usize),
    (_, __1, _): (usize, IdOrImm, usize),
) -> Exp
{
    Exp::Add(__0, __1)
}

pub fn __action24<
    'input,
>(
    input: &'input str,
    (_, _, _): (usize, &'input str, usize),
    (_, __0, _): (usize, String, usize),
    (_, __1, _): (usize, IdOrImm, usize),
) -> Exp
{
    Exp::Sub(__0, __1)
}

pub fn __action25<
    'input,
>(
    input: &'input str,
    (_, _, _): (usize, &'input str, usize),
    (_, __0, _): (usize, String, usize),
    (_, __1, _): (usize, IdOrImm, usize),
    (_, __2, _): (usize, i32, usize),
) -> Exp
{
    Exp::Ld(__0, __1, __2)
}

pub fn __action26<
    'input,
>(
    input: &'input str,
    (_, _, _): (usize, &'input str, usize),
    (_, __0, _): (usize, String, usize),
    (_, __1, _): (usize, String, usize),
    (_, __2, _): (usize, IdOrImm, usize),
    (_, __3, _): (usize, i32, usize),
) -> Exp
{
    Exp::St(__0, __1, __2, __3)
}

pub fn __action27<
    'input,
>(
    input: &'input str,
    (_, _, _): (usize, &'input str, usize),
    (_, i, _): (usize, String, usize),
    (_, m, _): (usize, IdOrImm, usize),
    (_, _, _): (usize, &'input str, usize),
    (_, x, _): (usize, E, usize),
    (_, _, _): (usize, &'input str, usize),
    (_, _, _): (usize, &'input str, usize),
    (_, _, _): (usize, &'input str, usize),
    (_, y, _): (usize, E, usize),
    (_, _, _): (usize, &'input str, usize),
) -> Exp
{
    Exp::IfEq(i,m,Box::new(x),Box::new(y))
}

pub fn __action28<
    'input,
>(
    input: &'input str,
    (_, _, _): (usize, &'input str, usize),
    (_, i, _): (usize, String, usize),
    (_, m, _): (usize, IdOrImm, usize),
    (_, _, _): (usize, &'input str, usize),
    (_, x, _): (usize, E, usize),
    (_, _, _): (usize, &'input str, usize),
    (_, _, _): (usize, &'input str, usize),
    (_, _, _): (usize, &'input str, usize),
    (_, y, _): (usize, E, usize),
    (_, _, _): (usize, &'input str, usize),
) -> Exp
{
    Exp::IfLE(i,m,Box::new(x),Box::new(y))
}

pub fn __action29<
    'input,
>(
    input: &'input str,
    (_, _, _): (usize, &'input str, usize),
    (_, i, _): (usize, String, usize),
    (_, m, _): (usize, IdOrImm, usize),
    (_, _, _): (usize, &'input str, usize),
    (_, x, _): (usize, E, usize),
    (_, _, _): (usize, &'input str, usize),
    (_, _, _): (usize, &'input str, usize),
    (_, _, _): (usize, &'input str, usize),
    (_, y, _): (usize, E, usize),
    (_, _, _): (usize, &'input str, usize),
) -> Exp
{
    Exp::IfGE(i,m,Box::new(x),Box::new(y))
}

pub fn __action30<
    'input,
>(
    input: &'input str,
    (_, _, _): (usize, &'input str, usize),
    (_, __0, _): (usize, String, usize),
    (_, _, _): (usize, &'input str, usize),
    (_, __1, _): (usize, Vec<String>, usize),
    (_, _, _): (usize, &'input str, usize),
) -> Exp
{
    Exp::Call(__0, __1)
}

pub fn __action31<
    'input,
>(
    input: &'input str,
    (_, _, _): (usize, &'input str, usize),
    (_, __0, _): (usize, String, usize),
    (_, __1, _): (usize, String, usize),
) -> Exp
{
    Exp::Save(__0, __1)
}

pub fn __action32<
    'input,
>(
    input: &'input str,
    (_, _, _): (usize, &'input str, usize),
    (_, __0, _): (usize, String, usize),
) -> Exp
{
    Exp::Restore(__0)
}

pub fn __action33<
    'input,
>(
    input: &'input str,
    (_, __0, _): (usize, T, usize),
) -> ::std::option::Option<T>
{
    Some(__0)
}

pub fn __action34<
    'input,
>(
    input: &'input str,
    __lookbehind: &usize,
    __lookahead: &usize,
) -> ::std::option::Option<T>
{
    None
}

pub fn __action35<
    'input,
>(
    input: &'input str,
    __lookbehind: &usize,
    __lookahead: &usize,
) -> ::std::vec::Vec<T>
{
    vec![]
}

pub fn __action36<
    'input,
>(
    input: &'input str,
    (_, v, _): (usize, ::std::vec::Vec<T>, usize),
) -> ::std::vec::Vec<T>
{
    v
}

pub fn __action37<
    'input,
>(
    input: &'input str,
    (_, __0, _): (usize, T, usize),
    (_, _, _): (usize, &'input str, usize),
) -> T
{
    (__0)
}

pub fn __action38<
    'input,
>(
    input: &'input str,
    (_, __0, _): (usize, String, usize),
) -> ::std::option::Option<String>
{
    Some(__0)
}

pub fn __action39<
    'input,
>(
    input: &'input str,
    __lookbehind: &usize,
    __lookahead: &usize,
) -> ::std::option::Option<String>
{
    None
}

pub fn __action40<
    'input,
>(
    input: &'input str,
    __lookbehind: &usize,
    __lookahead: &usize,
) -> ::std::vec::Vec<String>
{
    vec![]
}

pub fn __action41<
    'input,
>(
    input: &'input str,
    (_, v, _): (usize, ::std::vec::Vec<String>, usize),
) -> ::std::vec::Vec<String>
{
    v
}

pub fn __action42<
    'input,
>(
    input: &'input str,
    (_, __0, _): (usize, String, usize),
    (_, _, _): (usize, &'input str, usize),
) -> String
{
    (__0)
}

pub fn __action43<
    'input,
>(
    input: &'input str,
    __lookbehind: &usize,
    __lookahead: &usize,
) -> ::std::vec::Vec<Fundef>
{
    vec![]
}

pub fn __action44<
    'input,
>(
    input: &'input str,
    (_, v, _): (usize, ::std::vec::Vec<Fundef>, usize),
) -> ::std::vec::Vec<Fundef>
{
    v
}

pub fn __action45<
    'input,
>(
    input: &'input str,
    (_, __0, _): (usize, Fundef, usize),
) -> Fundef
{
    (__0)
}

pub fn __action46<
    'input,
>(
    input: &'input str,
    (_, __0, _): (usize, Fundef, usize),
) -> ::std::vec::Vec<Fundef>
{
    vec![__0]
}

pub fn __action47<
    'input,
>(
    input: &'input str,
    (_, v, _): (usize, ::std::vec::Vec<Fundef>, usize),
    (_, e, _): (usize, Fundef, usize),
) -> ::std::vec::Vec<Fundef>
{
    { let mut v = v; v.push(e); v }
}

pub fn __action48<
    'input,
>(
    input: &'input str,
    (_, __0, _): (usize, String, usize),
) -> ::std::vec::Vec<String>
{
    vec![__0]
}

pub fn __action49<
    'input,
>(
    input: &'input str,
    (_, v, _): (usize, ::std::vec::Vec<String>, usize),
    (_, e, _): (usize, String, usize),
) -> ::std::vec::Vec<String>
{
    { let mut v = v; v.push(e); v }
}

pub fn __action50<
    'input,
>(
    input: &'input str,
    (_, __0, _): (usize, T, usize),
) -> ::std::vec::Vec<T>
{
    vec![__0]
}

pub fn __action51<
    'input,
>(
    input: &'input str,
    (_, v, _): (usize, ::std::vec::Vec<T>, usize),
    (_, e, _): (usize, T, usize),
) -> ::std::vec::Vec<T>
{
    { let mut v = v; v.push(e); v }
}

pub fn __action52<
    'input,
>(
    input: &'input str,
    __0: (usize, Fundef, usize),
) -> ::std::vec::Vec<Fundef>
{
    let __start0 = __0.0.clone();
    let __end0 = __0.2.clone();
    let __temp0 = __action45(
        input,
        __0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action46(
        input,
        __temp0,
    )
}

pub fn __action53<
    'input,
>(
    input: &'input str,
    __0: (usize, ::std::vec::Vec<Fundef>, usize),
    __1: (usize, Fundef, usize),
) -> ::std::vec::Vec<Fundef>
{
    let __start0 = __1.0.clone();
    let __end0 = __1.2.clone();
    let __temp0 = __action45(
        input,
        __1,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action47(
        input,
        __0,
        __temp0,
    )
}

pub fn __action54<
    'input,
>(
    input: &'input str,
    __0: (usize, E, usize),
) -> Prog
{
    let __start0 = __0.0.clone();
    let __end0 = __0.0.clone();
    let __temp0 = __action43(
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action4(
        input,
        __temp0,
        __0,
    )
}

pub fn __action55<
    'input,
>(
    input: &'input str,
    __0: (usize, ::std::vec::Vec<Fundef>, usize),
    __1: (usize, E, usize),
) -> Prog
{
    let __start0 = __0.0.clone();
    let __end0 = __0.2.clone();
    let __temp0 = __action44(
        input,
        __0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action4(
        input,
        __temp0,
        __1,
    )
}

pub fn __action56<
    'input,
>(
    input: &'input str,
    __0: (usize, String, usize),
    __1: (usize, &'input str, usize),
) -> ::std::vec::Vec<String>
{
    let __start0 = __0.0.clone();
    let __end0 = __1.2.clone();
    let __temp0 = __action42(
        input,
        __0,
        __1,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action48(
        input,
        __temp0,
    )
}

pub fn __action57<
    'input,
>(
    input: &'input str,
    __0: (usize, ::std::vec::Vec<String>, usize),
    __1: (usize, String, usize),
    __2: (usize, &'input str, usize),
) -> ::std::vec::Vec<String>
{
    let __start0 = __1.0.clone();
    let __end0 = __2.2.clone();
    let __temp0 = __action42(
        input,
        __1,
        __2,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action49(
        input,
        __0,
        __temp0,
    )
}

pub fn __action58<
    'input,
>(
    input: &'input str,
    __0: (usize, ::std::option::Option<String>, usize),
) -> Vec<String>
{
    let __start0 = __0.0.clone();
    let __end0 = __0.0.clone();
    let __temp0 = __action40(
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action6(
        input,
        __temp0,
        __0,
    )
}

pub fn __action59<
    'input,
>(
    input: &'input str,
    __0: (usize, ::std::vec::Vec<String>, usize),
    __1: (usize, ::std::option::Option<String>, usize),
) -> Vec<String>
{
    let __start0 = __0.0.clone();
    let __end0 = __0.2.clone();
    let __temp0 = __action41(
        input,
        __0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action6(
        input,
        __temp0,
        __1,
    )
}

pub fn __action60<
    'input,
>(
    input: &'input str,
    __0: (usize, T, usize),
    __1: (usize, &'input str, usize),
) -> ::std::vec::Vec<T>
{
    let __start0 = __0.0.clone();
    let __end0 = __1.2.clone();
    let __temp0 = __action37(
        input,
        __0,
        __1,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action50(
        input,
        __temp0,
    )
}

pub fn __action61<
    'input,
>(
    input: &'input str,
    __0: (usize, ::std::vec::Vec<T>, usize),
    __1: (usize, T, usize),
    __2: (usize, &'input str, usize),
) -> ::std::vec::Vec<T>
{
    let __start0 = __1.0.clone();
    let __end0 = __2.2.clone();
    let __temp0 = __action37(
        input,
        __1,
        __2,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action51(
        input,
        __0,
        __temp0,
    )
}

pub fn __action62<
    'input,
>(
    input: &'input str,
    __0: (usize, ::std::option::Option<T>, usize),
) -> Vec<T>
{
    let __start0 = __0.0.clone();
    let __end0 = __0.0.clone();
    let __temp0 = __action35(
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action12(
        input,
        __temp0,
        __0,
    )
}

pub fn __action63<
    'input,
>(
    input: &'input str,
    __0: (usize, ::std::vec::Vec<T>, usize),
    __1: (usize, ::std::option::Option<T>, usize),
) -> Vec<T>
{
    let __start0 = __0.0.clone();
    let __end0 = __0.2.clone();
    let __temp0 = __action36(
        input,
        __0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action12(
        input,
        __temp0,
        __1,
    )
}

pub fn __action64<
    'input,
>(
    input: &'input str,
    __0: (usize, String, usize),
) -> Vec<String>
{
    let __start0 = __0.0.clone();
    let __end0 = __0.2.clone();
    let __temp0 = __action38(
        input,
        __0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action58(
        input,
        __temp0,
    )
}

pub fn __action65<
    'input,
>(
    input: &'input str,
    __lookbehind: &usize,
    __lookahead: &usize,
) -> Vec<String>
{
    let __start0 = __lookbehind.clone();
    let __end0 = __lookahead.clone();
    let __temp0 = __action39(
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action58(
        input,
        __temp0,
    )
}

pub fn __action66<
    'input,
>(
    input: &'input str,
    __0: (usize, ::std::vec::Vec<String>, usize),
    __1: (usize, String, usize),
) -> Vec<String>
{
    let __start0 = __1.0.clone();
    let __end0 = __1.2.clone();
    let __temp0 = __action38(
        input,
        __1,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action59(
        input,
        __0,
        __temp0,
    )
}

pub fn __action67<
    'input,
>(
    input: &'input str,
    __0: (usize, ::std::vec::Vec<String>, usize),
) -> Vec<String>
{
    let __start0 = __0.2.clone();
    let __end0 = __0.2.clone();
    let __temp0 = __action39(
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action59(
        input,
        __0,
        __temp0,
    )
}

pub fn __action68<
    'input,
>(
    input: &'input str,
    __0: (usize, T, usize),
) -> Vec<T>
{
    let __start0 = __0.0.clone();
    let __end0 = __0.2.clone();
    let __temp0 = __action33(
        input,
        __0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action62(
        input,
        __temp0,
    )
}

pub fn __action69<
    'input,
>(
    input: &'input str,
    __lookbehind: &usize,
    __lookahead: &usize,
) -> Vec<T>
{
    let __start0 = __lookbehind.clone();
    let __end0 = __lookahead.clone();
    let __temp0 = __action34(
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action62(
        input,
        __temp0,
    )
}

pub fn __action70<
    'input,
>(
    input: &'input str,
    __0: (usize, ::std::vec::Vec<T>, usize),
    __1: (usize, T, usize),
) -> Vec<T>
{
    let __start0 = __1.0.clone();
    let __end0 = __1.2.clone();
    let __temp0 = __action33(
        input,
        __1,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action63(
        input,
        __0,
        __temp0,
    )
}

pub fn __action71<
    'input,
>(
    input: &'input str,
    __0: (usize, ::std::vec::Vec<T>, usize),
) -> Vec<T>
{
    let __start0 = __0.2.clone();
    let __end0 = __0.2.clone();
    let __temp0 = __action34(
        input,
        &__start0,
        &__end0,
    );
    let __temp0 = (__start0, __temp0, __end0);
    __action63(
        input,
        __0,
        __temp0,
    )
}

pub trait __ToTriple<'input, > {
    type Error;
    fn to_triple(value: Self) -> Result<(usize,(usize, &'input str),usize),Self::Error>;
}

impl<'input, > __ToTriple<'input, > for (usize, (usize, &'input str), usize) {
    type Error = ();
    fn to_triple(value: Self) -> Result<(usize,(usize, &'input str),usize),()> {
        Ok(value)
    }
}
impl<'input, > __ToTriple<'input, > for Result<(usize, (usize, &'input str), usize),()> {
    type Error = ();
    fn to_triple(value: Self) -> Result<(usize,(usize, &'input str),usize),()> {
        value
    }
}
