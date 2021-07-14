---
title: "コンパイラ内部の AST 表現について"
date: 2015-12-29T14:25:44.000Z
tags: ["OCaml", "コンパイラ"]
---

コンパイラは大体，ソースコードを構文解析し，AST を作り，意味解析，コード生成という流れで実装されると思います．

さて，AST は単純に書くと

```
type expr =
  | Int of int
  | Add of expr * expr
  | Apply of expr * expr list
  | ...

```

みたいな感じに書けると思います．

これで確かにソースコードの syntax 上の余計な飾りをとっぱらった木構造になっているので抽象構文木としては十分機能します．
一方，既存のコンパイラを見ると，構文解析の後，型検査などの意味解析時にプログラムの不正を見つけた場合，きちんとソース上の位置を合わせて通知してくれます．
このためには，AST に位置情報を含める必要があります．

また，型推論の前後で，木構造としては同じ構造だけれども，型情報の持ち方に違いがあるという状況もあります．

```
(* 型推論前 *)
type expr =
  | Int of int
  | Add of expr * expr
  | Apply of expr * expr list

(* 型推論後 *)
type texpr =
  | Typed\_int of int
  | Typed\_add of texpr * texpr
  | Typed\_apply of texpr * texpr list

```

このように AST の表現は，木構造としては同じだが付随する情報だけが異なるという場合があります．

いろいろな言語のコンパイラの AST 表現を調査してみたところ，Elm コンパイラの方式が良かったのでまとめておきたいと思います．

```
type Expr annotation definition variable tipe =
    A.Annotated annotation (Expr' annotation definition variable tipe)


data Expr' ann def var typ
    = Literal Literal.Literal
    | Var var
    | Range (Expr ann def var typ) (Expr ann def var typ)
    | ExplicitList [Expr ann def var typ]
    | Binop var (Expr ann def var typ) (Expr ann def var typ)
    | Lambda (Pattern.Pattern ann var) (Expr ann def var typ)
    | App (Expr ann def var typ) (Expr ann def var typ)
    | If [(Expr ann def var typ, Expr ann def var typ)] (Expr ann def var typ)
    | Let [def] (Expr ann def var typ)
    | Case (Expr ann def var typ) [(Pattern.Pattern ann var, Expr ann def var typ)]
    | Data String [Expr ann def var typ]
    | Access (Expr ann def var typ) String
    | Update (Expr ann def var typ) [(String, Expr ann def var typ)]
    | Record [(String, Expr ann def var typ)]
    -- for type checking and code gen only
    | Port (PortImpl (Expr ann def var typ) typ)
    | GLShader String String Literal.GLShaderTipe

```

Elm コンパイラは Haskell で実装されています．
`Expr` が型引数として，`annotation` などを持っています．(`definition`, `tipe` についてはいまいちなんのための抽象化か理解していません...)
`annotation` は，AST に付随する情報です．`A.Annotated` という型が，核となる情報に，情報を annotate する役割を担います．

```
data Annotated annotation a
    = A annotation a

```

そして，AST の核となる構造自体は `Expr'` が持ちます．

こうすることで，`annotation` の内容を変えるだけで，木構造を何度も書き直す必要なく，コンパイラの各ステップに適した AST 表現を作る事ができます．
ちなみに `variable` はどうやら変数などの名前を表現する型を表しているようです．(始めは単なる `String`)

---

---
