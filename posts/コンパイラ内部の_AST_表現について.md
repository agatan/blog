---
title: "コンパイラ内部の AST 表現について"
date: 2015-12-29T14:25:44.000Z
tags: []
---

<p><a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D1%A5%A4%A5%E9">コンパイラ</a>は大体，<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%BD%A1%BC%A5%B9%A5%B3%A1%BC%A5%C9">ソースコード</a>を<a class="keyword" href="http://d.hatena.ne.jp/keyword/%B9%BD%CA%B8%B2%F2%C0%CF">構文解析</a>し，AST を作り，意味解析，コード生成という流れで実装されると思います．</p>

<p>さて，AST は単純に書くと</p>

<pre class="code lang-ocaml" data-lang="ocaml" data-unlink><span class="synStatement">type</span> expr <span class="synStatement">=</span>
  <span class="synStatement">|</span> <span class="synConstant">Int</span> <span class="synStatement">of</span> <span class="synType">int</span>
  <span class="synStatement">|</span> <span class="synConstant">Add</span> <span class="synStatement">of</span> expr <span class="synStatement">*</span> expr
  <span class="synStatement">|</span> <span class="synConstant">Apply</span> <span class="synStatement">of</span> expr <span class="synStatement">*</span> expr <span class="synType">list</span>
  <span class="synStatement">|</span> ...
</pre>

<p>みたいな感じに書けると思います．</p>

<p>これで確かに<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%BD%A1%BC%A5%B9%A5%B3%A1%BC%A5%C9">ソースコード</a>の syntax 上の余計な飾りをとっぱらった<a class="keyword" href="http://d.hatena.ne.jp/keyword/%CC%DA%B9%BD%C2%A4">木構造</a>になっているので抽象<a class="keyword" href="http://d.hatena.ne.jp/keyword/%B9%BD%CA%B8%CC%DA">構文木</a>としては十分機能します．
一方，既存の<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D1%A5%A4%A5%E9">コンパイラ</a>を見ると，<a class="keyword" href="http://d.hatena.ne.jp/keyword/%B9%BD%CA%B8%B2%F2%C0%CF">構文解析</a>の後，型検査などの意味解析時にプログラムの不正を見つけた場合，きちんとソース上の位置を合わせて通知してくれます．
このためには，AST に位置情報を含める必要があります．</p>

<p>また，<a class="keyword" href="http://d.hatena.ne.jp/keyword/%B7%BF%BF%E4%CF%C0">型推論</a>の前後で，<a class="keyword" href="http://d.hatena.ne.jp/keyword/%CC%DA%B9%BD%C2%A4">木構造</a>としては同じ構造だけれども，型情報の持ち方に違いがあるという状況もあります．</p>

<pre class="code lang-ocaml" data-lang="ocaml" data-unlink><span class="synComment">(* 型推論前 *)</span>
<span class="synStatement">type</span> expr <span class="synStatement">=</span>
  <span class="synStatement">|</span> <span class="synConstant">Int</span> <span class="synStatement">of</span> <span class="synType">int</span>
  <span class="synStatement">|</span> <span class="synConstant">Add</span> <span class="synStatement">of</span> expr <span class="synStatement">*</span> expr
  <span class="synStatement">|</span> <span class="synConstant">Apply</span> <span class="synStatement">of</span> expr <span class="synStatement">*</span> expr <span class="synType">list</span>

<span class="synComment">(* 型推論後 *)</span>
<span class="synStatement">type</span> texpr <span class="synStatement">=</span>
  <span class="synStatement">|</span> <span class="synConstant">Typed_int</span> <span class="synStatement">of</span> <span class="synType">int</span>
  <span class="synStatement">|</span> <span class="synConstant">Typed_add</span> <span class="synStatement">of</span> texpr <span class="synStatement">*</span> texpr
  <span class="synStatement">|</span> <span class="synConstant">Typed_apply</span> <span class="synStatement">of</span> texpr <span class="synStatement">*</span> texpr <span class="synType">list</span>
</pre>

<p>このように AST の表現は，<a class="keyword" href="http://d.hatena.ne.jp/keyword/%CC%DA%B9%BD%C2%A4">木構造</a>としては同じだが付随する情報だけが異なるという場合があります．</p>

<p>いろいろな言語の<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D1%A5%A4%A5%E9">コンパイラ</a>の AST 表現を調査してみたところ，Elm <a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D1%A5%A4%A5%E9">コンパイラ</a>の方式が良かったのでまとめておきたいと思います．</p>

<pre class="code lang-haskell" data-lang="haskell" data-unlink><span class="synType">type</span> Expr annotation definition variable tipe <span class="synStatement">=</span>
    A.Annotated annotation (Expr' annotation definition variable tipe)


<span class="synType">data</span> Expr' ann def var typ
    <span class="synStatement">=</span> Literal Literal.Literal
    <span class="synStatement">|</span> Var var
    <span class="synStatement">|</span> Range (Expr ann def var typ) (Expr ann def var typ)
    <span class="synStatement">|</span> ExplicitList [Expr ann def var typ]
    <span class="synStatement">|</span> Binop var (Expr ann def var typ) (Expr ann def var typ)
    <span class="synStatement">|</span> Lambda (Pattern.Pattern ann var) (Expr ann def var typ)
    <span class="synStatement">|</span> App (Expr ann def var typ) (Expr ann def var typ)
    <span class="synStatement">|</span> If [(Expr ann def var typ, Expr ann def var typ)] (Expr ann def var typ)
    <span class="synStatement">|</span> Let [def] (Expr ann def var typ)
    <span class="synStatement">|</span> Case (Expr ann def var typ) [(Pattern.Pattern ann var, Expr ann def var typ)]
    <span class="synStatement">|</span> Data String [Expr ann def var typ]
    <span class="synStatement">|</span> Access (Expr ann def var typ) String
    <span class="synStatement">|</span> Update (Expr ann def var typ) [(String, Expr ann def var typ)]
    <span class="synStatement">|</span> Record [(String, Expr ann def var typ)]
    <span class="synComment">-- for type checking and code gen only</span>
    <span class="synStatement">|</span> Port (PortImpl (Expr ann def var typ) typ)
    <span class="synStatement">|</span> GLShader String String Literal.GLShaderTipe
</pre>

<p>Elm <a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D1%A5%A4%A5%E9">コンパイラ</a>は <a class="keyword" href="http://d.hatena.ne.jp/keyword/Haskell">Haskell</a> で実装されています．
<code>Expr</code> が型引数として，<code>annotation</code> などを持っています．(<code>definition</code>, <code>tipe</code> についてはいまいちなんのための抽象化か理解していません...)
<code>annotation</code> は，AST に付随する情報です．<code>A.Annotated</code> という型が，核となる情報に，情報を annotate する役割を担います．</p>

<pre class="code lang-haskell" data-lang="haskell" data-unlink><span class="synType">data</span> Annotated annotation a
    <span class="synStatement">=</span> A annotation a
</pre>

<p>そして，AST の核となる構造自体は <code>Expr'</code> が持ちます．</p>

<p>こうすることで，<code>annotation</code> の内容を変えるだけで，<a class="keyword" href="http://d.hatena.ne.jp/keyword/%CC%DA%B9%BD%C2%A4">木構造</a>を何度も書き直す必要なく，<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D1%A5%A4%A5%E9">コンパイラ</a>の各ステップに適した AST 表現を作る事ができます．
ちなみに <code>variable</code> はどうやら変数などの名前を表現する型を表しているようです．(始めは単なる <code>String</code>)</p>

---

---
