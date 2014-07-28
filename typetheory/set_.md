# 証明を忘れたプログラマの為の集合論

集合論を勉強するには、写経をするのが近道だったはずです。

## 集合と元

### 集合

集合とは対象となる要素の集まりです。

### 要素(元)

集合を構成する対象を、集合の要素(element)、または元といいます。
集合はその要素を含む(contain)あるいは要素は集合に属する(belong to)といいます。
要素sが集合Sに含まれるとき、s∈Sと書きます。

### 外延表記と内包表記

- 外延表記は集合の要素を明示的に記述します。例:{1,2,3}
- 内包表記は集合の要素を|で区切って右側に条件を記述します。例:{x | xは実数}
- Nは自然数全体を対象とした集合で、N={0,1,2,3,...}です。
- Zは整数全体を対象とした集合で、Z={...-3,-2,-1,0,1,2,3,...}です。
- Rは自然数全体を対象とした集合で、R={x | xは実数}です。

*N,Z,Rといった集合は型と考える事が出来そうですが、実際に型理論では型を集合として扱います。
そのため型理論では集合論が重要になるのです。*

### 空集合

空の集合を∅と記述します。∅={}です。

### 部分集合

- S の要素が常に T の要素であるときは T の部分集合(subset)であり、S⊆Tと書きます。
- 任意の集合 S に対して ∅ ⊆ S かつ S ⊆ S です。

### ￼真部分集合

- S ⊆ T かつ S =/ T のとき、 S を T の真部分集合 (proper subset)と呼び S ⊂ T と書きます。

### 差集合、和集合、積集合

S と T が集合のとき

- 差集合 S \ T は S の要素から T の要素を取り除いた集合です。
- 和集合 S ∪ T は S の要素と T の要素を合わせた集合です。
- 積集合 S ∩ T は S の要素でかつ T の要素である集合です。

### 大きさ、冪集合（べき集合)

- 集合の大きさ(濃度)を |S| と記述します。例えば A = {1,2,3} のとき |A| は3です。
- 集合のすべての部分集合の集合をべき集合といいます。

ここまで、勉強した事をOCamlで書いてみましょう。

```
open Format

let pp_ls f ppf ls =
  let rec loop ppf = function
    | [] -> ()
    | [x] -> fprintf ppf "%a@?" f x
    | x::xs -> fprintf ppf "%a, %a" f x loop xs
  in
  fprintf ppf "{%a}@?" loop ls
let pp_i ppf i = fprintf ppf "%d@?" i

module Z = struct
  module IntSet = Set.Make(
    struct
      let compare = Pervasives.compare
      type t = int
    end )
  include IntSet
  let of_list l = List.fold_left (fun s e -> add e s) empty l
  let pp ppf s = pp_ls pp_i ppf (elements s)
  let powerset s =
    let rec ps = function
    | [] -> [[]]
    | h::t -> List.fold_right (fun t xs -> (h::t)::t::xs) (ps t) []
    in ps(elements s)
  let pp_l ppf ls = pp_ls pp_i ppf ls
  let pp_ss ppf ss = pp_ls pp_l ppf ss
end


let _ =
  let a = Z.of_list [1;2;3] in
  let b = Z.of_list [3;4;5;6] in

  let r = [[1; 2; 3]; [3; 4]] in
  fprintf std_formatter "a = %a@." Z.pp a;
  fprintf std_formatter "a = %a@." Z.pp a;
  fprintf std_formatter "b = %a@." Z.pp b;
  fprintf std_formatter "a ∪ b = %a 和集合@." Z.pp (Z.union a b);
  fprintf std_formatter "a ∩ b = %a 積集合@." Z.pp (Z.inter a b);
  fprintf std_formatter "a \\ b = %a 差集合@." Z.pp (Z.diff a b);
  fprintf std_formatter "|a| = %d aの大きさ@." (Z.cardinal a);
  fprintf std_formatter "|a ∪ b| = %d aの大きさ@." (Z.cardinal (Z.union a b));
  fprintf std_formatter "aの冪集合Ρ(a) %a@." Z.pp_ss (Z.powerset a);
  let p s = (Z.cardinal b) == (Z.cardinal s) && (Z.cardinal (Z.diff b s)) == 0 in
  fprintf std_formatter "p s %b@." (p (Z.of_list[3;4;5]));
  fprintf std_formatter "p s %b@." (p (Z.of_list[3;4;5;6]));
  fprintf std_formatter "{x|x∈Ｚ,x>=0,x<=5}=%a@." (p (Z.of_list[0,1,2,3,4,5]));
```

### 可算

## 命題

### 命題

真偽が定まる主張を命題といいます。

例えば、次のP、Qは命題です。

- P:2+3=5です。
- Q:7は偶数です。

Pは真であり、Qは偽です。

次のようなP、Qは命題ではありません。

- P:s+3=5
- Q:sは偶数です。

なぜなら、これらの主張はsが定まっていなければ、真偽の判定ができないからです。
このような主張を述語といいます。
述語は、sについての関数と言えます。

### 論理積∧、論理積∨、ならば→、否定¬

命題P、Qから新しい命題を作成する方法として、次のような演算があります。

- 論理積 P ∧ Q
- 論理和 P ∨ Q
- 論理包含 P → Q
- 否定 ¬ P

各記号は次のように読みます。

- P ∧ Q : PかつQ
- P ∨ Q : PまたはQ
- P → Q : PならばQ
- ¬ P : Pの否定

### 真偽表

P∧Qに次の(1)〜(4)のような意味を付けます。

1. PとQが真ならばP∧Qは真である。
2. Pが真でQが偽ならばP∧Qは偽である。
3. Pが偽でQが真ならばP∧Qは偽である。
4. PとQが偽ならばP∧Qは偽である。

これらを表にまとめて次のように表します。

<table>
  <tr>
    <td>P</td>
    <td>Q</td>
    <td>P ∧ Q</td>
  </tr>
  <tr>
    <td>真</td>
    <td>真</td>
    <td>真</td>
  </tr>
  <tr>
    <td>真</td>
    <td>偽</td>
    <td>偽</td>
  </tr>
  <tr>
    <td>偽</td>
    <td>真</td>
    <td>偽</td>
  </tr>
  <tr>
    <td>偽</td>
    <td>偽</td>
    <td>偽</td>
  </tr>
</table>

この表を『真偽表』といいます。

### 命題公理

各記号の真偽表は以下のようになります:

<table><tr><td>

<table>
  <tr>
    <td>P</td>
    <td>Q</td>
    <td>P ∧ Q</td>
  </tr>
  <tr>
    <td>真</td>
    <td>真</td>
    <td>真</td>
  </tr>
  <tr>
    <td>真</td>
    <td>偽</td>
    <td>偽</td>
  </tr>
  <tr>
    <td>偽</td>
    <td>真</td>
    <td>偽</td>
  </tr>
  <tr>
    <td>偽</td>
    <td>偽</td>
    <td>偽</td>
  </tr>
</table>

</td><td>

<table>
  <tr>
    <td>P</td>
    <td>Q</td>
    <td>P ∨ Q</td>
  </tr>
  <tr>
    <td>真</td>
    <td>真</td>
    <td>真</td>
  </tr>
  <tr>
    <td>真</td>
    <td>偽</td>
    <td>真</td>
  </tr>
  <tr>
    <td>偽</td>
    <td>真</td>
    <td>真</td>
  </tr>
  <tr>
    <td>偽</td>
    <td>偽</td>
    <td>偽</td>
  </tr>
</table>

</td><td>

<table>
  <tr>
    <td>P</td>
    <td>Q</td>
    <td>P → Q</td>
  </tr>
  <tr>
    <td>真</td>
    <td>真</td>
    <td>真</td>
  </tr>
  <tr>
    <td>真</td>
    <td>偽</td>
    <td>偽</td>
  </tr>
  <tr>
    <td>偽</td>
    <td>真</td>
    <td>真</td>
  </tr>
  <tr>
    <td>偽</td>
    <td>偽</td>
    <td>真</td>
  </tr>
</table>

</td><td>

<table>
  <tr>
    <td>P</td>
    <td>¬ P</td>
  </tr>
  <tr>
    <td>真</td>
    <td>偽</td>
  </tr>
  <tr>
    <td>偽</td>
    <td>真</td>
  </tr>
</table>

</td></tr></table>

  ここで気になるのはP→Qだろう。例えば
  Xが1->Yは3

  このとき、

  Xが1ならばYは3は嘘ではないから真
  Xが1ならばYは3でないは嘘だから偽

  そして、Xが1でない場合はYの値は決まらないので

  Xが1でないならばYは3は嘘ではないので真
  Xが1でないならばYは3ではないも嘘ではないので真

  となる。このPの条件が間違えていれば、Qが命題なら真になるというところが納得できればよい。

  →はちょっと普通の意味とはちがう所が注意する必要がある。

### 恒真命題(トートロジ)

  議論しているときに、
  トートロジとか言い出されるとムカつくんですけど、
  それは、トートロジを知らない人にとって
  相手はトートロジという知識を知っていて優位にある
  自分はトートロジを知らずに不利である。
  相手は論理の正しい正しくないに関わらずその優位である立場を利用して
  ねじ伏せようとしているように感じるからだろう。

  しかしトートロジとは、単に必ず真になる命題である。
  猿ならば猿、人ならば人、はトートロジである。
  猿または猿でないも真である。

  P∨(¬P)、「P→P」、「(P∧(P→Q))→Q」はトートロジである。

命題 P1, ..., Pnに対して、∧、∨、→、¬を用いて命題f(P1,...,Pn)を作成する。
各命題P1,...,Pnの真偽に関わらず、f(P1,...,Pn)が真であるとする。
このとき、f(P1,...,Pn)を『恒真命題(トートロジ)』という

## 命題と集合

TODO: ラーメンは止める。

おいしいラーメンにはコクがあるとします。
おいしいラーメン集合の中にコクのあるラーメン集合が含まれるという事です。
命題は集合で表す事が出来る訳です。

<table><tr><td>
コクがない
ラーメン集合A<br/>
これはまずい
</td></tr></table>

<table><tr><td>
おいしいラーメン集合

<table><tr><td>
おいしくてコクのあるラーメン集合

<table><tr><td>ラーメン集合B'
おいしくて コクがある
</td></tr></table>

</td></tr></table>

</td></tr></table>

<table><tr><td>
おいしくないコクのあるラーメン集合

<table><tr><td>ラーメン集合B''
おいしくないけど コクがある
</td></tr></table>

</td></tr></table>


コクのないラーメンAはおいしいラーメン集合には含まれません。
コクのあるラーメンBはおいしいラーメン集合に含まれる可能性がありますが、
他の条件が揃っていなければ美味しくないかもしれません。ラーメンBがB'に含まれるかB''に含まれるかは分かりません。

P(x): xは自然数でかつx>10という述語があった場合を考えると、
PはP={x|x∈N,x > 10}という集合と考える事が出来ます。
xはPの集合の要素ですから、x∈Pでx∈NのときにP(x)は述語といえます。
5は自然数なのでPに含まれる可能性がありますが、x > 10を満たさないのでP(x)は偽であり、Pの集合には含まれません。
20は自然数なのでPに含まれる可能性があり、x > 10を満たすのでP(x)は真であり、Pの集合に含まれます。

*ところで、Nの集合は型のように考えて、集合は命題と考える事が出来るなら、型は命題と考える事も出来そうです。
この命題を集合と考える事がいわゆるカリーハワード対応の発見に繋がっていく訳です。*

## 関係

### 述語 単項関係

変数 s の値を定めることで、真偽を判定することができる主張を述語といいます。
とくに、変数sを自由変数といいいます。
自由変数sをもつ述語をP(s)と表します。
P(s)に値aを代入したものをP(a)と表します。
述語は条件と呼ばれることもあります。
例えば、次のP(s),Q(s)は述語です。

P(s): s+3=5

Q(s): s は偶数

これらの主張は変数 x を定めることで真偽の判定が可能になります。
例えば、P(2)は真で、P(3)は偽です。
また、Q(3)は偽で、Q(4)は真です。

命題を集合として考えれば、PやQは集合です。s ∈ N であるときに s ∈ PのときP(s)は述語です。
s∈Nであるときにs∈QのときQ(s)は述語です。
述語は真偽値を返す関数とみなす事が出来ます。

### 2項関係

(s,t) ∈ R を s R t と書き２項関係と呼びます。

### n項関係

ああ


反射的 集合 R= {a,b,c}
関係 [(a,b);(b,a);(b,c)]なら反射的閉包は複数回の反射をした結果の集合になるので
[(a,a);(a,b);(b,b);(b,a);(b,c);(c,c)]になる
