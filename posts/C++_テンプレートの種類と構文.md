---
title: "C++ テンプレートの種類と構文"
date: 2016-05-31T08:57:56.000Z
tags: []
---
<p>前回テンプレートがなぜ必要なのかについて簡単にまとめたので、今回はその構文や種類についてまとめたいと思います。</p>

<p><iframe src="http://agtn.hatenablog.com/embed/2016/05/30/234647" title="C++ のテンプレートについてまとめる（１）なぜテンプレートが必要なのか - refer to 右上➚" class="embed-card embed-blogcard" scrolling="no" frameborder="0" style="display: block; width: 100%; height: 190px; max-width: 500px; margin: 10px 0px;"></iframe><cite class="hatena-citation"><a href="http://agtn.hatenablog.com/entry/2016/05/30/234647">agtn.hatenablog.com</a></cite></p>

<h2>アウトライン</h2>

<ul>
<li>テンプレートの種類と構文

<ul>
<li>定義する</li>
<li>使用する(<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%A4%A5%F3%A5%B9%A5%BF%A5%F3%A5%B9">インスタンス</a>化)</li>
</ul>
</li>
<li>クラステンプレート</li>
<li>関数テンプレート</li>
<li>メンバテンプレート</li>
<li><a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%A8%A5%A4%A5%EA%A5%A2%A5%B9">エイリアス</a>テンプレート(<a class="keyword" href="http://d.hatena.ne.jp/keyword/C%2B%2B">C++</a>11〜)</li>
<li>変数テンプレート(<a class="keyword" href="http://d.hatena.ne.jp/keyword/C%2B%2B">C++</a>14〜)</li>
<li>まとめと今後</li>
</ul>


<h2>テンプレートの種類と構文</h2>

<p><a class="keyword" href="http://d.hatena.ne.jp/keyword/C%2B%2B">C++</a> のテンプレートは，大きく 5 種類に分類することが出来ます。</p>

<ol>
<li>クラステンプレート</li>
<li>関数テンプレート</li>
<li><a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%E1%A5%F3%A5%D0%B4%D8%BF%F4">メンバ関数</a>テンプレート</li>
<li><a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%A8%A5%A4%A5%EA%A5%A2%A5%B9">エイリアス</a>テンプレート</li>
<li>変数テンプレート</li>
</ol>


<p>これらについて、以降で詳しくまとめていきます。</p>

<p>まずざっくり共通する<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B7%A5%F3%A5%BF%A5%C3%A5%AF%A5%B9">シンタックス</a>を示しておきます。</p>

<h3>定義する</h3>

<p>何かのテンプレートを定義したい場合は、通常の定義の前に <code>template</code> 宣言を記述します。</p>

<pre class="code lang-cpp" data-lang="cpp" data-unlink><span class="synType">template</span> &lt;<span class="synType">typename</span> T&gt;
<span class="synType">class</span> stack {
  ...
};
</pre>


<p>複数のテンプレート引数を取りたい場合や、型以外のテンプレート引数を取りたい場合には、以下のようにします。</p>

<pre class="code lang-cpp" data-lang="cpp" data-unlink><span class="synType">template</span> &lt;<span class="synType">typename</span> T, <span class="synType">int</span> N&gt;
<span class="synType">class</span> my_array {
  ...
};
</pre>


<p>上の例はクラステンプレートでしたが、関数でも<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%A8%A5%A4%A5%EA%A5%A2%A5%B9">エイリアス</a>でも、<code>template</code> を宣言する部分は共通です。</p>

<h3>使用する(<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%A4%A5%F3%A5%B9%A5%BF%A5%F3%A5%B9">インスタンス</a>化)</h3>

<p>次にテンプレートを使用する場合です。<br/>
テンプレートはあくまでテンプレートなので、使用する際には、具体的なテンプレート引数を与えて実体化する必要があります。これを <strong><a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%A4%A5%F3%A5%B9%A5%BF%A5%F3%A5%B9">インスタンス</a>化</strong> といいます。</p>

<p><a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%A4%A5%F3%A5%B9%A5%BF%A5%F3%A5%B9">インスタンス</a>化の構文も、テンプレートの種類によらず基本的には共通しています。</p>

<pre class="code lang-cpp" data-lang="cpp" data-unlink>stack&lt;<span class="synType">int</span>&gt; int_stack;
my_array&lt;std::string, <span class="synConstant">5</span>&gt; five_strings;
</pre>


<p><code>テンプレート名 &lt; 引数 &gt;</code> という感じです。<br/>
一応注意書きをしておきますと、<code>stack&lt;stack&lt;int&gt;&gt;</code> が<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B7%A5%F3%A5%BF%A5%C3%A5%AF%A5%B9">シンタックス</a>エラーになる場合があります。
意味としては <code>stack&lt;int&gt;</code> のスタックです。<br/>
<code>stack&lt;stack&lt;int&gt;&gt;</code> が<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D1%A5%A4%A5%EB">コンパイル</a>エラーになる場合は、使っている<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D1%A5%A4%A5%E9">コンパイラ</a>が古いかもしれません。
<a class="keyword" href="http://d.hatena.ne.jp/keyword/C%2B%2B">C++</a>03 では、<code>&gt;&gt;</code> の部分がシフト<a class="keyword" href="http://d.hatena.ne.jp/keyword/%B1%E9%BB%BB%BB%D2">演算子</a>として解釈されてしまうためです。<br/>
<code>g++ -std=c++11</code> とか <code>clang++ -std=c++11</code> とか <code>g++ -std=c++14</code> とか <code>clang++ -std=c++14</code> とか、<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D1%A5%A4%A5%E9">コンパイラ</a>が新しい規格を参照するようにオプションを渡してあげれば動きます。<br/>
(もし動かない場合は<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D1%A5%A4%A5%E9">コンパイラ</a>が古すぎます。もう 2016 年ですから、最低でも <a class="keyword" href="http://d.hatena.ne.jp/keyword/C%2B%2B">C++</a>11 以降を使いましょう。別物です。)</p>

<p>では、以降、それぞれのテンプレートについて詳しく見ていきます。</p>

<h2>クラステンプレート</h2>

<p>クラステンプレートは、クラスのテンプレートです。前回の <code>stack</code> がこれにあたります。<br/>
一番わかり易い<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%E6%A1%BC%A5%B9%A5%B1%A1%BC%A5%B9">ユースケース</a>は、コンテナ型の定義でしょう。<br/>
スタックや単方向リスト、ハッシュマップなど、内部に保持する型に依存しないデータ構造を定義するために使えます。</p>

<p>前回の <code>stack</code> を再掲しておきます。</p>

<pre class="code lang-cpp" data-lang="cpp" data-unlink><span class="synType">template</span> &lt;<span class="synType">typename</span> T&gt;
<span class="synType">class</span> stack {
<span class="synStatement">public</span>:
  stack() : data(), n() {}

  <span class="synType">void</span> push(T x) {
    <span class="synStatement">if</span> (n &gt;= MAX_ELEM) {
      <span class="synStatement">throw</span> <span class="synConstant">&quot;stack is full!!&quot;</span>;
    }
    data[n++] = x;
  }

  T pop() {
    <span class="synStatement">if</span> (n &lt; <span class="synConstant">0</span>) {
      <span class="synStatement">throw</span> <span class="synConstant">&quot;stack is empty!!&quot;</span>;
    }
    <span class="synStatement">return</span> data[--n];
  }
<span class="synStatement">private</span>:
  T data[MAX_ELEM];
  <span class="synType">int</span> n;
};
</pre>


<h2>関数テンプレート</h2>

<p>関数テンプレートは以下の様なものです。</p>

<pre class="code lang-cpp" data-lang="cpp" data-unlink><span class="synType">template</span> &lt;<span class="synType">typename</span> T&gt;
T max(T left, T right) {
  <span class="synStatement">if</span> (left &gt; right) {
    <span class="synStatement">return</span> left;
  } <span class="synStatement">else</span> {
    <span class="synStatement">return</span> right;
  }
}

<span class="synType">int</span> x = max&lt;<span class="synType">int</span>&gt;(<span class="synConstant">1</span>, <span class="synConstant">0</span>); <span class="synComment">// =&gt; x == 1</span>
<span class="synType">double</span> y = max&lt;<span class="synType">double</span>&gt;(<span class="synConstant">0.5</span>, <span class="synConstant">100.0</span>); <span class="synComment">// =&gt; y == 100.0</span>
</pre>


<p><code>max</code> 関数は、「ある型 T について、２つの引数のうち、大きい方を返す」関数です。<br/>
明示的に <code>max&lt;int&gt;</code> や <code>max&lt;double&gt;</code> とすることで、<code>int</code> や <code>double</code> についての「大きい方を返す」関数を得ています。</p>

<h3>テンプレート引数の推論</h3>

<p>実は、関数テンプレートの場合、明示的にテンプレートを引数を渡す必要がない場合があります。<br/>
今回の <code>max</code> 関数はまさにそのケースです。</p>

<pre class="code lang-cpp" data-lang="cpp" data-unlink><span class="synType">int</span> x = max(<span class="synConstant">1</span>, <span class="synConstant">0</span>);
</pre>


<p>これは、テンプレート引数の<a class="keyword" href="http://d.hatena.ne.jp/keyword/%B7%BF%BF%E4%CF%C0">型推論</a>の結果です。<br/>
<code>max</code> 関数の第一引数、第二引数はそれぞれ <code>T</code> です。そして、実際に渡されている <code>1</code> や <code>0</code> は <code>int</code> 型です。<br/>
これらの情報から、<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D1%A5%A4%A5%E9">コンパイラ</a>は <code>T == int</code> であることを推論します。<br/>
したがって暗黙に <code>max&lt;int&gt;</code> と指定されることになります。<br/>
この推論は色々複雑だったりしますが、はじめは引数から単純に推論できれば推論されると思っておけば良いんじゃないかなと思います。</p>

<p>一方、クラステンプレートなど、関数テンプレート(と<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%E1%A5%F3%A5%D0%B4%D8%BF%F4">メンバ関数</a>テンプレート)以外のテンプレートの場合には、この推論は行われません。<br/>
勘違いしやすいので気をつけましょう。特にクラステンプレートは間違いやすいです。</p>

<h3><a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%E1%A5%F3%A5%D0%B4%D8%BF%F4">メンバ関数</a>テンプレート</h3>

<pre class="code lang-cpp" data-lang="cpp" data-unlink><span class="synType">class</span> printer {
<span class="synStatement">public</span>:
  <span class="synType">explicit</span> printer(std::ostream&amp; os) : os(os) {}

  <span class="synType">template</span> &lt;<span class="synType">typename</span> T&gt;
  <span class="synType">void</span> print(T <span class="synType">const</span>&amp; v) {
    os &lt;&lt; v;
  }
<span class="synStatement">private</span>:
  std::ostream&amp; os;
};
</pre>


<p><a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%E1%A5%F3%A5%D0%B4%D8%BF%F4">メンバ関数</a>テンプレートは、関数テンプレートとほとんど同じです。違いは、<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%E1%A5%F3%A5%D0%B4%D8%BF%F4">メンバ関数</a>として定義されていることだけです。</p>

<pre class="code lang-cpp" data-lang="cpp" data-unlink>printer p(std::cout);
</pre>


<p>ここまではテンプレートでもなんでもないただのクラスです。</p>

<pre class="code lang-cpp" data-lang="cpp" data-unlink>p.print(<span class="synConstant">0</span>);
p.print(<span class="synConstant">&quot;abc&quot;</span>);
p.print&lt;<span class="synType">double</span>&gt;(<span class="synConstant">0.1</span>);
</pre>


<p>こんな感じで使います。関数テンプレートとほぼ同じですね。</p>

<h3><a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%A8%A5%A4%A5%EA%A5%A2%A5%B9">エイリアス</a>テンプレート</h3>

<p><a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%A8%A5%A4%A5%EA%A5%A2%A5%B9">エイリアス</a>テンプレートは、<a class="keyword" href="http://d.hatena.ne.jp/keyword/C%2B%2B">C++</a>11 から使用できるテンプレートです。</p>

<pre class="code lang-cpp" data-lang="cpp" data-unlink><span class="synType">template</span> &lt;<span class="synType">typename</span> T, <span class="synType">typename</span> U&gt;
<span class="synType">struct</span> pair;

<span class="synType">template</span> &lt;<span class="synType">typename</span> T&gt;
<span class="synStatement">using</span> with_int_t = pair&lt;T, <span class="synType">int</span>&gt;;
</pre>


<p><code>using</code> で型名の<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%A8%A5%A4%A5%EA%A5%A2%A5%B9">エイリアス</a>を作ることが出来ますが、それをテンプレートにすることが出来ます。</p>

<pre class="code lang-cpp" data-lang="cpp" data-unlink>with_int_t&lt;<span class="synType">bool</span>&gt; p(<span class="synConstant">true</span>, <span class="synConstant">0</span>);  <span class="synComment">// pair&lt;bool, int&gt; p(true, 0);</span>
with_int_t&lt;std::string&gt; s(<span class="synConstant">&quot;abc&quot;</span>, <span class="synConstant">100</span>);  <span class="synComment">// pair&lt;std::string, int&gt; s(&quot;abc&quot;, 100);</span>
</pre>


<p>ちなみに <a class="keyword" href="http://d.hatena.ne.jp/keyword/C%2B%2B">C++</a>11 より前は、<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%A8%A5%A4%A5%EA%A5%A2%A5%B9">エイリアス</a>テンプレートの代替として、</p>

<pre class="code lang-cpp" data-lang="cpp" data-unlink><span class="synType">template</span> &lt;<span class="synType">typename</span> T&gt;
<span class="synType">struct</span> with_int {
  <span class="synType">typedef</span> pair&lt;T, <span class="synType">int</span>&gt; type;
}

with_int&lt;<span class="synType">bool</span>&gt;::type p(<span class="synConstant">true</span>, <span class="synConstant">0</span>); <span class="synComment">// pair&lt;bool, int&gt; p(true, 0);</span>
</pre>


<p>という記述をしていました。(今でもライブラリなどで現役の表現ですので覚えておきましょう)</p>

<h3>変数テンプレート</h3>

<p>最後の変数テンプレートは、<a class="keyword" href="http://d.hatena.ne.jp/keyword/C%2B%2B">C++</a>14 から使用できるテンプレートです。</p>

<pre class="code lang-cpp" data-lang="cpp" data-unlink><span class="synType">template</span> &lt;<span class="synType">typename</span> T&gt;
constexpr T pi = <span class="synStatement">static_cast</span>&lt;T&gt;(<span class="synConstant">3.1415926</span>);

<span class="synType">int</span> x = pi&lt;<span class="synType">int</span>&gt;;
<span class="synType">double</span> y = pi&lt;<span class="synType">double</span>&gt;;
</pre>


<p><code>constexpr</code> は定数式というやつです。</p>

<p>ちなみに <a class="keyword" href="http://d.hatena.ne.jp/keyword/C%2B%2B">C++</a>14 より前は、代替として</p>

<pre class="code lang-cpp" data-lang="cpp" data-unlink><span class="synType">template</span> &lt;<span class="synType">typename</span> T&gt;
<span class="synType">struct</span> pi {
  <span class="synType">static</span> <span class="synType">const</span> T value = <span class="synStatement">static_cast</span>&lt;T&gt;(<span class="synConstant">3.1415926</span>);
};

<span class="synType">int</span> x = pi&lt;<span class="synType">int</span>&gt;::value;
<span class="synType">double</span> y = pi&lt;<span class="synType">double</span>&gt;::value;
</pre>


<p>という記述をしていました。(こちらも現役の表現です)</p>

<h2>まとめと今後</h2>

<p>というわけで今回はテンプレートの種類とそれぞれの<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B7%A5%F3%A5%BF%A5%C3%A5%AF%A5%B9">シンタックス</a>についてまとめてみました。<br/>
今後、特殊化や部分特殊化などのお話をするときに種類によって微妙に違いがあったりするので、しっかり区別しておいたほうが良さそうです。</p>

-----
--------