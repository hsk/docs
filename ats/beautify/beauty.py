import re
import sys

sys.setrecursionlimit(1000*1000)


class Parser(object):
    def __rshift__(self, that):
        return p(self, that) ^ (lambda a: a[1])

    def __lshift__(self, that):
        return p(self, that) ^ (lambda a: a[0])

    def __xor__(self, that):
        return Action(self, that)

    def __neg__(self):
        return Action(self, lambda a: [NestP, a, NestM])

    class static(type):
        def __getitem__(self, i):
            return self(*i) if isinstance(i, tuple) else self(i)
    __metaclass__ = static


class nreg(Parser):
    def __init__(self, param):
        self.param = re.compile(param)

    def __call__(self, i):
        m = self.param.search(i)
        return None if m is None else [m.group(0), i[len(m.group(0)):]]


class orp(Parser):
    def __init__(self, *params):
        self.params = map(lambda v: st(v) if isinstance(v, basestring) else v, params)

    def __call__(self, i):
        for v in self.params:
            r = v(i)
            if r is not None:
                return r
        return None


class nstr(Parser):
    def __init__(self, param):
        self.param = param

    def __call__(self, i):
        return None if not i.startswith(self.param) else [self.param, i[len(self.param):]]


class p(Parser):
    def __init__(self, *params):
        self.params = map(lambda v: st(v) if isinstance(v, basestring) else v, params)

    def __call__(self, i):
        rs = []
        for v in self.params:
            r = v(i)
            if r is None:
                return None
            rs.append(r[0])
            i = r[1]
        return [rs, i]


class Action(Parser):
    def __init__(self, thiz, action):
        self.thiz = thiz
        self.action = action

    def __call__(self, i):
        r = self.thiz(i)
        if r is None:
            return None
        else:
            r2 = self.action(r[0])
            return None if r2 is None else [r2, r[1]]


class opt(Parser):
    def __init__(self, *thiz):
        self.thiz = p(*thiz)

    def __call__(self, i):
        r = self.thiz(i)
        return [[], i] if r is None else r


class rep(Parser):
    def __init__(self, *thiz):
        self.thiz = p(*thiz)

    def __call__(self, i):
        rs = []
        while(True):
            r = self.thiz(i)
            if r is None:
                return [rs, i]
            rs.append(r[0])
            i = r[1]


blockcomment = p(nstr("(*"), rep(orp(nreg(r"([^*]|\*[^)])+"), lambda i: blockcomment(i))), nstr("*)"))
skip = rep(orp(nreg(r'^(\s|/\*([^*]|\*[^/])*\*/|//[^\r\n]*)+'), blockcomment))


def st(s):
    return p(skip, nstr(s))

def rep1(*thiz):
    return rep(*thiz) ^ (lambda p: None if len(p) < 1 else p)


class notp(Parser):
    def __init__(self, *thiz):
        self.thiz = orp(*thiz)

    def __call__(self, i):
        return [[], i] if self.thiz(i) is None else None


def reg(s):
    return p(skip, nreg(s))


class Nest:
    def __init__(self, name, n):
        self.name = name
        self.n = n

    def __repr__(self):
        return self.name
NestP = Nest("NestP", 1)
NestM = Nest("NestM", -1)


def flat(a, rc):
    if isinstance(a, list):
        for i in a:
            rc = flat(i, rc)
    else:
        rc.append(a)
    return rc


def cnv(e):
    reg2 = re.compile(r'\n')
    whiteSpace = re.compile(r'^(\s|\s*\(\*)')
    reg1 = re.compile(r'\n')

    e = flat(e, [])
    e2 = []
    i = 0
    while(i < len(e)):
        s = [e[i]]
        if isinstance(s[0], basestring) and reg2.search(s[0]) is not None and whiteSpace.search(s[0]) is not None:
            s = []
            while(i < len(e)):
                s2 = e[i]
                if s2 is NestM:
                    e2.append(s2)
                else:
                    s.append(s2)
                    if whiteSpace.search(s2) is None:
                        break
                i += 1
        i += 1
        e2.extend(s)

    nest = 0
    e3 = []
    for s in e2:
        if isinstance(s, Nest):
            nest += s.n
        else:
            m = reg2.search(s)
            if m is not None:
                s = reg1.sub("\n"+("  " * nest), s)
            e3.append(s)
    return "".join(e3)

# pritty ocaml grammer

keywords = reg(r"^(begin|end|if|else|then|let|in|val|implement|local|typedef|lam|try|with|fnx|fn|fun|open|struct|module|and|while|do|done)\b")
semi = notp(";;") >> p(";")
exp = p(lambda i: p(exp4, rep[semi, exp4], opt(semi))(i))
exps = p(exp, opt(semi))
id = orp(
    notp(keywords) >> reg(r"^\$?[_a-zA-Z0-9]+"),
    reg(r'^[+\-*/.<>:@=^|~?]+') ^ (lambda i: i if re.search(r'^(=|=>|->|\|)$', i[1]) is None else None),
    reg(r'^[!]'),
    reg(r'^("(\\.|[^"])*"|\'(\\.|[^\'])\')')
)
assign = p(lambda i: p(app, "=", -exp)(i))
sexp = orp(
    p("@(", -opt(exp), ")"),
    p("@{", -p(assign, rep(",", assign)), "}"),
    p("@[", -opt(exp), "]"),
    p("'(", -opt(exp), ")"),
    p("'{", -p(assign, rep(",", assign)), "}"),
    p("'[", -opt(exp), "]"),
    id,
    p("begin", -exp, "end"),
    p("(", -opt(exp), ")"),
    p("{", -p(lambda i: rep(toplevel)(i)), "}"),
    p("[", -opt(exp), "]")
)
app = rep1(sexp)
exp1 = orp(
    p("lam", -p[app], orp("=>", "=<cloptr1>"), -p(lambda i: exp2(i))),
    p("let", -p(lambda i: rep(toplevel, opt(";;"))(i)), "in", -opt(exp), "end"),
    p("if", -exps, "then", -p(lambda i: exp4(i)), opt(p("else", exp))),
    p("case", -exps, "of", opt("|"), -exps, rep["|", -exps]),
    p("try", -exps, "with", opt("|"), -exps, rep["|", -exps]),
    p("while", -exps, "do", -exps, "done"),
    app
)

exp2 = p(exp1, opt("=", exp1))
exp3 = p(exp2, rep(",", exp2))
exp4 = p(exp3, opt("->", exp))
prog = p(lambda i: rep(toplevel)(i))
struct = p(lambda i: orp(
    p("struct", -prog, "end"),
    -struct_exp
)(i))
struct_exp = rep1(orp(
    id,
    p("(", -opt(struct), ")"),
))
toplevel = orp(
    p(orp("fn", "fnx"), -p[app], opt("=", -exp)),
    p("fun", -p[app], opt("=", -exp)),
    p("val", -p[app], "=", -exp),
    p("implement", -p[app], "=", -exp),
    p("typedef", -p[app], "=", -exp),
    p("and", -p[app], "=", -exp),
    p("#include", -exp),
    p("#define", -sexp, opt("(", -exps, ")"), -sexp),
    p("local", -p[prog], "in", -p[prog], "end"),
    p("type", -p[id, "=", opt("|"), exp, rep("|", exp)]),
    p("open", -p[id, rep(".", id)]),
    p(exp, opt(semi)),
    p("module", -p[app], "=", struct)
)

regparse = re.compile(r"^[ \t]+", re.M)


def parse(s):
    return cnv(prog(regparse.sub("", s)))
