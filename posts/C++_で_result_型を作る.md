---
title: "C++ で result 型を作る"
date: 2016-07-01T14:30:09.000Z
tags: []
---
<p><a class="keyword" href="http://d.hatena.ne.jp/keyword/Haskell">Haskell</a> や Rust など多くの強力な型システムを持つ<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%D7%A5%ED%A5%B0%A5%E9%A5%DF%A5%F3%A5%B0%B8%C0%B8%EC">プログラミング言語</a>は、<code>Either</code> とか <code>Result</code> といった「失敗するかもしれない」計算の値を示す型を持っています。</p>

<p>現在の <a class="keyword" href="http://d.hatena.ne.jp/keyword/C%2B%2B">C++</a> の標準ライブラリにはこのような型はありませんので、それを自作してみようというわけです。</p>

<p>ちなみに現在策定中の<a class="keyword" href="http://d.hatena.ne.jp/keyword/C%2B%2B">C++</a>17には <code>std::optional</code> が入ることが決定しているようです。これは、<code>result</code> と同様に失敗するかもしれな値を示しますが、失敗した原因がなんなのかを通知する仕組みを持っていません。</p>

<h2>そもそもどういう型か</h2>

<p>Rust の <code>Result</code> 型を例にみてみます。</p>

<pre class="code lang-rust" data-lang="rust" data-unlink><span class="synStatement">enum</span> <span class="synIdentifier">Result</span><span class="synStatement">&lt;</span>V, E<span class="synStatement">&gt;</span> {
  <span class="synConstant">Ok</span>(V),
  <span class="synConstant">Err</span>(E),
}
</pre>


<p>Rust における <code>Result</code> 型はだいたいこんな感じで定義されています。</p>

<p><code>Result</code> は型引数を２つとります。<code>V</code> が成功時の値で、<code>E</code> が失敗時のエラー情報です。
例えば、<code>fn parse_int(s: &amp;str) -&gt; Result&lt;isize, String&gt;;</code> は、文字列を受け取り、それが整数としてパース出来れば <code>isize</code> に変換し、<code>Ok(isize)</code> として返します。
もし整数としてパース出来ないなどのエラーがあれば、それを <code>String</code> で表現し、<code>Err(String)</code> で返します。</p>

<p>本質的にはこれが全てです。ここに、<code>Result</code> から中身を取り出す(<code>Err</code> なら <code>panic</code> する)関数などを定義してあげれば便利にエラー状態を表現できます。<br/>
(Rust の <code>try</code> マクロはとても便利ですよね)</p>

<h2>まずはベースとなる result を作る</h2>

<p>まずはベースとなる <code>result</code> 型を作ってみます。</p>

<pre class="code lang-cpp" data-lang="cpp" data-unlink><span class="synType">template</span> &lt;<span class="synType">typename</span> T, <span class="synType">typename</span> E&gt;
<span class="synType">struct</span> result {

  result(T <span class="synType">const</span>&amp; ok) : t(tag::OK) {
    ok_ = ok;
  }

  result(E <span class="synType">const</span>&amp; e) : t(tag::OK) {
    err_ = e;
  }

  ~result() {
    <span class="synStatement">switch</span> (t) {
      <span class="synStatement">case</span> tag::OK:
        ok_.~T();
        <span class="synStatement">break</span>;
      <span class="synStatement">case</span> tag::ERROR:
        err_.~E();
        <span class="synStatement">break</span>;
    }
  }

  result(result <span class="synType">const</span>&amp; r): t(r.t) {
    <span class="synStatement">switch</span> (t) {
      <span class="synStatement">case</span> tag::OK:
        ok_ = r.ok_;
        <span class="synStatement">break</span>;
      <span class="synStatement">case</span> tag::ERROR:
        err_ = r.err_;
        <span class="synStatement">break</span>;
    }
  }

  T <span class="synType">const</span>&amp; get() <span class="synType">const</span> {
    <span class="synStatement">if</span> (t != tag::OK) {
      <span class="synStatement">throw</span> <span class="synConstant">&quot;invalid get operation&quot;</span>;
    }
    <span class="synStatement">return</span> ok_;
  }

  E <span class="synType">const</span>&amp; get_error() <span class="synType">const</span> {
    <span class="synStatement">if</span> (t != tag::ERROR) {
      <span class="synStatement">throw</span> <span class="synConstant">&quot;invalid get operation&quot;</span>;
    }
    <span class="synStatement">return</span> err_;
  }

<span class="synStatement">private</span>:
  <span class="synType">enum</span> <span class="synType">class</span> tag {
    OK,
    ERROR,
  };
  tag t;
  <span class="synType">union</span> {
    T ok_;
    E err_;
  };

};
</pre>


<p>かなり雑ですが、ざっくりこんな感じになるはずです。
<a class="keyword" href="http://d.hatena.ne.jp/keyword/C%2B%2B">C++</a>11 から拡張されて自由度がかなり高くなった <code>union</code> がとても便利です。</p>

<p>これで <code>result&lt;int, std::string&gt;(1).get()</code> とやれば <code>1</code> が返るし <code>result&lt;int, std::string&gt;(std::string("test")).get_error()</code> で <code>"test"</code> が返るはずです。</p>

<h2><a class="keyword" href="http://d.hatena.ne.jp/keyword/C%2B%2B">C++</a> でやると何が難しいか</h2>

<p><a class="keyword" href="http://d.hatena.ne.jp/keyword/C%2B%2B">C++</a> で難しいのは、Rustより弱い<a class="keyword" href="http://d.hatena.ne.jp/keyword/%B7%BF%BF%E4%CF%C0">型推論</a>が引き起こす問題です。<br/>
Rust では、<code>Ok(1isize)</code> とか <code>Err("error!".to_owned())</code> とすれば、その値がどういう型であることが期待されているのかまで含めて<a class="keyword" href="http://d.hatena.ne.jp/keyword/%B7%BF%BF%E4%CF%C0">型推論</a>や単一化が行われます。
すなわち、<code>Ok(1isize)</code> だけを見てもエラーの型がわからないため、<code>Result&lt;isize, E&gt;</code> の <code>E</code> を決定することが出来ないが、Rust は強力な<a class="keyword" href="http://d.hatena.ne.jp/keyword/%B7%BF%BF%E4%CF%C0">型推論</a>機構を持つため、これを決定することが出来ます。</p>

<p>一方、<a class="keyword" href="http://d.hatena.ne.jp/keyword/C%2B%2B">C++</a> では <code>result&lt;int, std::string&gt; f() { return 1; }</code> は <code>int</code> から <code>result&lt;int, std::string&gt;</code> の暗黙変換がきくので可能ですが、<code>result&lt;int, int&gt;</code> などとした瞬間、暗黙変換に頼ることはできなくなります。
そこで、出来れば <code>ok(1)</code> とか <code>err("test")</code> という感じにしたいのですが、これは一筋縄では行きません。</p>

<pre class="code lang-cpp" data-lang="cpp" data-unlink><span class="synType">template</span> &lt;<span class="synType">typename</span> T, <span class="synType">typename</span> E&gt; 
result&lt;T, E&gt; ok(T);
</pre>


<p>これだと <code>T</code> は推論されても <code>E</code> が推論されないので、<code>ok&lt;int, std::string&gt;(1)</code> などとしなければなりません。これは使いづらすぎます。</p>

<h2>じゃあどうするか</h2>

<p>先ほどとは違う形ですが、やっぱり暗黙の型変換を応用します。</p>

<p>要するに <code>ok</code> を表す型と <code>error</code> を表す型を区別しつつ、<code>result&lt;V, E&gt;</code> とはなるべくシームレスに変換をしたいというわけですから、それぞれ専用の型を作ってしまえば良いのです。</p>

<pre class="code lang-cpp" data-lang="cpp" data-unlink><span class="synType">template</span> &lt;<span class="synType">typename</span> T&gt;
<span class="synType">struct</span> ok_value {
  <span class="synType">explicit</span> ok_value(T t): t(t) {}

  <span class="synType">template</span> &lt;<span class="synType">typename</span> V, <span class="synType">typename</span> E&gt;
  <span class="synStatement">operator</span> result&lt;V, E&gt; () <span class="synType">const</span>;

<span class="synStatement">private</span>:
  T t;
};

<span class="synType">template</span> &lt;<span class="synType">typename</span> T&gt;
<span class="synType">template</span> &lt;<span class="synType">typename</span> V, <span class="synType">typename</span> E&gt;
ok_value&lt;T&gt;::<span class="synStatement">operator</span> result&lt;V, E&gt; () <span class="synType">const</span> {
  <span class="synStatement">return</span> result&lt;V, E&gt;(t);
}

<span class="synType">template</span> &lt;<span class="synType">typename</span> T&gt;
ok_value&lt;T&gt; ok(T t) {
  <span class="synStatement">return</span> ok_value&lt;T&gt;(t);
}
</pre>


<p><code>ok</code> 側だけ示しました。<br/>
<code>ok</code> 関数はテンプレートになっており、<code>T</code> 型の値をとって <code>ok_value&lt;T&gt;</code> を返します。（本当は値渡し以外にも対応すべきですが、簡単のために値渡しだけ実装しています）</p>

<p><code>ok_value&lt;T&gt;</code> は型変換<a class="keyword" href="http://d.hatena.ne.jp/keyword/%B1%E9%BB%BB%BB%D2">演算子</a> <code>operator result&lt;V, E&gt;() const</code> を持ちます。これによって <code>ok_value</code> から <code>result</code> への暗黙変換が可能になります。</p>

<p><code>ok_value&lt;T&gt;</code> は <code>result&lt;T, E&gt;</code> に変換出来れば良さそうに見えるのですが、それでは不十分です。<br/>
<code>ok("test")</code> は <code>ok_value&lt;const char*&gt;</code> を返します。<code>ok_value&lt;T&gt; -&gt; result&lt;T, E&gt;</code> の変換しか提供していない場合は、<code>result&lt;std::string, E&gt;</code> への変換ができなくなってしまいます。これは不便ですよね。<br/>
そこで新たにテンプレート引数を導入することでこれを解決しています。もっときちんとやるなら <code>std::is_constructible</code> などを使ってチェックをするべきだとは思いますが。</p>

<p><code>error</code> 側もほぼ同様のコードを書いてやれば、</p>

<pre class="code lang-cpp" data-lang="cpp" data-unlink>result&lt;<span class="synType">int</span>, std::string&gt; parse_digit(<span class="synType">char</span> c) {
  <span class="synStatement">if</span> (c &lt; <span class="synConstant">'0'</span> || <span class="synConstant">'9'</span> &lt; c) {
    <span class="synStatement">return</span> error(<span class="synConstant">&quot;invalid character&quot;</span>);
  }
  <span class="synStatement">return</span> ok(c - <span class="synConstant">'0'</span>);
}
</pre>


<p>というように書けます。</p>

<h2>まとめ</h2>

<p><code>T</code> から <code>result&lt;T, E&gt;</code> への暗黙変換を許すという方針も全然ありだとは思いますが、個人的に Rust などで <code>ok</code> なら <code>ok</code> と明示するスタイルに慣れているので、こっちのほうが気に入っています。<br/>
明らかに正常に値を返していそうな感じがコードにあらわれて好きです。</p>

<p>暗黙の型変換って危険だしあまり良いイメージはないと思うのですが、やっぱりあれば便利ですね。
<a class="keyword" href="http://d.hatena.ne.jp/keyword/C%2B%2B">C++</a> を使っている時点で気を抜いたら死なので、「取り扱いを把握して全力で注意しながら使えば危険じゃない」という気持ちで便利に使いたいものです。</p>

-----
--------