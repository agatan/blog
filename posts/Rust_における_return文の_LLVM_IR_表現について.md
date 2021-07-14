---
title: "Rust における return文の LLVM IR 表現について"
date: 2016-04-13T09:34:03.000Z
tags: ["Rust", "LLVM", "コンパイラ"]
---

<ul>
<li><code>if</code> 文が値を返す</li>
<li><code>return</code> 文を持つ</li>
</ul>

<p>以上のような特徴を持つ言語はどういう感じで<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D1%A5%A4%A5%EB">コンパイル</a>されるのか知りたくて，Rust について調べてみました．</p>

<p>Rust では以下の様なことが出来ます．</p>

<pre class="code lang-rust" data-lang="rust" data-unlink><span class="synStatement">fn</span> <span class="synIdentifier">f</span>() {
  <span class="synStatement">let</span> x = <span class="synStatement">if</span> cond {
    return None;
  } <span class="synStatement">else</span> {
    <span class="synConstant">1</span>
  };
  ...
}
</pre>

<p><a class="keyword" href="http://d.hatena.ne.jp/keyword/Scala">Scala</a> とかもできると思います．<code>cond</code> が真だった場合は，<code>x</code> の値を返すのではなく，関数から抜けてしまうという意味です．</p>

<p>これを Rust ではどんな <a class="keyword" href="http://d.hatena.ne.jp/keyword/LLVM">LLVM</a> IR に落とし込んでいるのか．</p>

<h1><code>return</code> 文がない場合</h1>

<pre class="code lang-rust" data-lang="rust" data-unlink><span class="synStatement">fn</span> <span class="synIdentifier">noreturn</span>(x: isize) -&gt; isize {
  x
}
</pre>

<p>最も単純な場合です．この場合，生成される <a class="keyword" href="http://d.hatena.ne.jp/keyword/LLVM">LLVM</a> IR は，</p>

<pre class="code" data-lang="" data-unlink>define internal i64 @_ZN4hoge8noreturn17h811bf1a871f85432E(i64) unnamed_addr #0 {
entry-block:
  %x = alloca i64
  store i64 %0, i64* %x
  %1 = load i64, i64* %x
  ret i64 %1
}</pre>

<p>となります．
名前がマングルされていますが，上記の <code>noreturn</code> 関数です．
やっていることは単純で，第一引数を読み込んで返すだけです．</p>

<h1><code>return</code> に相当する文が一つのみの場合</h1>

<pre class="code lang-rust" data-lang="rust" data-unlink><span class="synStatement">fn</span> <span class="synIdentifier">onereturn</span>(x: isize) -&gt; isize {
  <span class="synStatement">let</span> y = <span class="synStatement">if</span> x == <span class="synConstant">0</span> {
    <span class="synConstant">1</span>
  } <span class="synStatement">else</span> {
    x
  };
  return x;
}
</pre>

<p>実際に値を返す部分が一箇所しかない場合です．途中に分岐があっても最終的に一箇所になっていれば多分同じ結果になります．</p>

<pre class="code" data-lang="" data-unlink>define internal i64 @_ZN4hoge9onereturn17h8b718f32daa6a379E(i64) unnamed_addr #0 {
entry-block:
  %x = alloca i64
  %y = alloca i64
  store i64 %0, i64* %x
  %1 = load i64, i64* %x
  %2 = icmp eq i64 %1, 0
  br i1 %2, label %then-block-18-, label %else-block

then-block-18-:                                   ; preds = %entry-block
  store i64 1, i64* %y
  br label %join

else-block:                                       ; preds = %entry-block
  %3 = load i64, i64* %x
  store i64 %3, i64* %y
  br label %join

join:                                             ; preds = %else-block, %then-block-18-
  %4 = load i64, i64* %x
  br label %clean_ast_10_

return:                                           ; preds = %clean_ast_10_
  ret i64 %4

clean_ast_10_:                                    ; preds = %join
  br label %return
}</pre>

<p><code>return</code> という BasicBlock ができています．これは <code>return</code> 文が現れると作られるよう？です．
で，その中では単純に <code>x</code> に該当する値を返しています．</p>

<p>最後の <code>return x;</code> 文を 単純に <code>x</code> に置き換えてみると，</p>

<pre class="code" data-lang="" data-unlink>define internal i64 @_ZN4hoge9onereturn17h8b718f32daa6a379E(i64) unnamed_addr #0 {
entry-block:
  %x = alloca i64
  %y = alloca i64
  store i64 %0, i64* %x
  %1 = load i64, i64* %x
  %2 = icmp eq i64 %1, 0
  br i1 %2, label %then-block-18-, label %else-block

then-block-18-:                                   ; preds = %entry-block
  store i64 1, i64* %y
  br label %join

else-block:                                       ; preds = %entry-block
  %3 = load i64, i64* %x
  store i64 %3, i64* %y
  br label %join

join:                                             ; preds = %else-block, %then-block-18-
  %4 = load i64, i64* %x
  ret i64 %4
}</pre>

<p>となります． <code>return</code> ブロックが消えていますね．なので <code>return</code> 文があると <code>return</code> ブロックが作られる、で良さそう？</p>

<h1><a class="keyword" href="http://d.hatena.ne.jp/keyword/%CA%A3%BF%F4">複数</a>のパスから値を返す</h1>

<pre class="code lang-rust" data-lang="rust" data-unlink><span class="synStatement">fn</span> <span class="synIdentifier">multireturn</span>(x: isize) -&gt; isize {
  <span class="synStatement">let</span> y = <span class="synStatement">if</span> x == <span class="synConstant">0</span> {
    return -<span class="synConstant">1</span>;
  } <span class="synStatement">else</span> {
    x
  };
  y
}
</pre>

<p>さて，では最初に述べた，<code>if</code> の分岐内にある <code>return</code> についてです．
これは，</p>

<pre class="code" data-lang="" data-unlink>define internal i64 @_ZN4hoge11multireturn17had379e8ce5a18f08E(i64) unnamed_addr #0 {
entry-block:
  %sret_slot = alloca i64
  %x = alloca i64
  %y = alloca i64
  store i64 %0, i64* %x
  %1 = load i64, i64* %x
  %2 = icmp eq i64 %1, 0
  br i1 %2, label %then-block-18-, label %else-block

then-block-18-:                                   ; preds = %entry-block
  store i64 -1, i64* %sret_slot
  br label %return

else-block:                                       ; preds = %entry-block
  %3 = load i64, i64* %x
  store i64 %3, i64* %y
  br label %join

join:                                             ; preds = %else-block
  %4 = load i64, i64* %y
  store i64 %4, i64* %sret_slot
  br label %return

return:                                           ; preds = %join, %then-block-18-
  %5 = load i64, i64* %sret_slot
  ret i64 %5
}</pre>

<p>こうなりました．
まず，<code>return</code> 文があるため？，<code>return</code> ブロックが作られています．
しかし今回は，パスによって返すものが違います．(値が違うという意味ではなく，同じ変数ですらないという意味です...)</p>

<p>よく IR を読むと，関数の頭で <code>%sret_slot</code> という名前でスタック領域を確保していることがわかります．
そして，<code>return</code> ブロック内では，これを読んできて返しています．<br/>
さらに，<code>if</code> 文の then 節にあたる，<code>then-block-18-</code> というブロックでは，<code>%sret_slot</code> に値を格納して <code>return</code> ブロックへジャンプしています．
else 節のあとの部分 (<code>join</code> ブロック) でも同様に, <code>%sret_slot</code> に値を格納して <code>return</code> ブロックへジャンプしています．</p>

<h1>まとめ</h1>

<p>というわけで，様々な Rust コードを <a class="keyword" href="http://d.hatena.ne.jp/keyword/LLVM">LLVM</a> IR に変換して見てみた結果，<a class="keyword" href="http://d.hatena.ne.jp/keyword/%CA%A3%BF%F4">複数</a>のパスから値を返す場合は，「ローカル変数として返り値を定義し，そこに返したい値を格納してから <code>return</code> に goto」という形になっていることがわかりました．</p>

<p>(ほとんど <a class="keyword" href="http://d.hatena.ne.jp/keyword/LLVM">LLVM</a> IR を乗っけるだけになってしまった...)</p>

<h2>ちなみに ...</h2>

<h1><code>if</code> 文の返す値をそのまま返す</h1>

<pre class="code lang-rust" data-lang="rust" data-unlink><span class="synStatement">fn</span> <span class="synIdentifier">ifreturn</span>(x: isize) -&gt; isize {
  <span class="synStatement">if</span> x == <span class="synConstant">0</span> {
    <span class="synConstant">1</span>
  } <span class="synStatement">else</span> {
    x
  }
}
</pre>

<p>Rust に慣れていないとちょっとわかりにくいですが，<code>x == 0</code> の場合は 1 を返し，そうでない場合は <code>x</code> を返す関数です．</p>

<p>これは，</p>

<pre class="code" data-lang="" data-unlink>define internal i64 @_ZN4hoge8ifreturn17hcdaab6e376d6c95cE(i64) unnamed_addr #0 {
entry-block:
  %sret_slot = alloca i64
  %x = alloca i64
  store i64 %0, i64* %x
  %1 = load i64, i64* %x
  %2 = icmp eq i64 %1, 0
  br i1 %2, label %then-block-15-, label %else-block

then-block-15-:                                   ; preds = %entry-block
  store i64 1, i64* %sret_slot
  br label %join

else-block:                                       ; preds = %entry-block
  %3 = load i64, i64* %x
  store i64 %3, i64* %sret_slot
  br label %join

join:                                             ; preds = %else-block, %then-block-15-
  %4 = load i64, i64* %sret_slot
  ret i64 %4
}</pre>

<p>こうなります．やっていることは上記の例たちとあまり変わりません．
しかし，<code>return</code> 文がないので？，<code>return</code> ブロックが作られていません．が, <code>%sret_slot</code> は定義されていますね...<br/>
これはどういうことなんでしょう．<code>rustc</code> のコードを読むべきなのかもしれませんが，イマイチ内部処理が想像しにくいです...</p>

<p>普通に翻訳していったら，</p>

<pre class="code" data-lang="" data-unlink>let x = if x == 0 { 1 } else { x };
x</pre>

<p>と同じ感じになる気がするので，<code>%sret_slot</code> という名前が出てくる余地は無い気がするのですが...(実質同じ処理ではあります)
分岐が直接返戻値になる場合は特別扱いしているのかな？</p>

---

---
