---
title: "Boost.Spirit.X3 の練習 2"
date: 2015-12-17T14:20:03.000Z
tags: ["C++", "Boost"]
---

<p><a href="http://agtn.hatenablog.com/entry/2015/12/17/190505">Boost.Spirit.X3 の練習1 - プログラミングのメモ帳➚</a>に引き続き，<code>Boost.Spirit.X3</code> のお勉強メモです．</p>

<h2>セマンティックアクション</h2>

<p><a class="keyword" href="http://d.hatena.ne.jp/keyword/%B9%BD%CA%B8%B2%F2%C0%CF">構文解析</a>にはセマンティックアクションというのがつきものです．<br/>
<code>yacc</code> や <code>parsec</code> など有名な<a class="keyword" href="http://d.hatena.ne.jp/keyword/%B9%BD%CA%B8%B2%F2%C0%CF">構文解析</a>のための<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%C4%A1%BC%A5%EB">ツール</a>/ライブラリにもありますね．</p>

<p>セマンティックアクションとは，定義したパーサのルールにマッチした時に実行するプログラムのことです．<br/>
といってもわかりにくいと思うので，コードを出してしまいます．</p>

<p>以下のコードは，<a class="keyword" href="http://d.hatena.ne.jp/keyword/%C9%E2%C6%B0%BE%AE%BF%F4%C5%C0%BF%F4">浮動小数点数</a>にマッチしてその値を標準出力に出力するパーサの定義です．</p>

<pre class="code lang-cpp" data-lang="cpp" data-unlink><span class="synPreProc">#include </span><span class="synConstant">&lt;boost/spirit/home/x3.hpp&gt;</span>

<span class="synPreProc">#include </span><span class="synConstant">&lt;iostream&gt;</span>
<span class="synPreProc">#include </span><span class="synConstant">&lt;string&gt;</span>


<span class="synType">namespace</span> x3 = boost::spirit::x3;

<span class="synType">auto</span> <span class="synType">const</span> print_action =
  [](<span class="synType">auto</span>&amp; ctx) { std::cout &lt;&lt; _attr(ctx) &lt;&lt; std::endl; };

<span class="synType">int</span> main()
{

  std::string src{ <span class="synConstant">&quot;( 123.4 )&quot;</span> };

  <span class="synType">auto</span> first = std::cbegin(src);
  <span class="synType">auto</span> <span class="synType">const</span> last = std::cend(src);

  <span class="synType">auto</span> parser = (<span class="synConstant">&quot;(&quot;</span> &gt;&gt; x3::double_ &gt;&gt; <span class="synConstant">&quot;)&quot;</span>)[print_action];

  <span class="synType">bool</span> success = x3::phrase_parse(first, last, parser, x3::ascii::space);

  <span class="synStatement">if</span> (!success || first != last) {
    <span class="synComment">// parse failed</span>
    std::cerr &lt;&lt; <span class="synConstant">&quot;Parse failed.&quot;</span> &lt;&lt; std::endl;
  }
}
</pre>

<pre class="code" data-lang="" data-unlink>$ clang++ -std=c++14 main.cpp
$ ./a.out
123.4</pre>

<p>パーサの結果を受け取らないようにしています．<code>parser</code> は <code>double</code> を返しますが，その結果は無視しています．<br/>
そして，セマンティックアクション部分で，出力を行っています．</p>

<p><code>auto const print_action</code> の定義が，セマンティックアクションです．<br/>
<code>C++14</code> の<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B8%A5%A7%A5%CD%A5%EA%A5%C3%A5%AF">ジェネリック</a>ラムダの機能をつかっています．<br/>
セマンティックアクションは，<code>X3</code> パーサのコンテキストを第一引数に取ります．<br/>
その具体的な型を気にしてはいけません．<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B8%A5%A7%A5%CD%A5%EA%A5%C3%A5%AF">ジェネリック</a>ラムダで受け取ります．<br/>
<code>_attr(ctx)</code> で，現在マッチしているパーサの attribute にアクセス出来ます．
<code>"(" &gt;&gt; x3::double_ &gt;&gt; ")"</code> の attribute は <code>double</code> 型なので，<code>_attr(ctx)</code> はマッチした<a class="keyword" href="http://d.hatena.ne.jp/keyword/%C9%E2%C6%B0%BE%AE%BF%F4%C5%C0%BF%F4">浮動小数点数</a>になります．</p>

<p><code>Qi</code> のセマンティックアクションは，関数オブジェクトなどを単純に使用することが出来ませんでした(?) が，<code>X3</code> ではセマンティックアクションはただの関数オブジェクトです．<br/>
パーサのコンテキストを引数に受け，結果や attribute への参照をそのまま関数内で扱えるようになったため，セマンティックアクションの記述がより <em>普通</em> の関数っぽくかけるようになったと感じました．</p>

<h2>名前付きパーサの定義</h2>

<p>今までのサンプルでは，シンプルなパーサを１つ定義しているだけでした．<br/>
一方現実には，パーサが<a class="keyword" href="http://d.hatena.ne.jp/keyword/%BA%C6%B5%A2">再帰</a>的になることは珍しくありません．</p>

<p>そこで，パーサの名前を前方宣言し，あとから定義を記述するようなパーサの書き方を使用します．</p>

<pre class="code lang-cpp" data-lang="cpp" data-unlink><span class="synPreProc">#include </span><span class="synConstant">&lt;boost/spirit/home/x3.hpp&gt;</span>

<span class="synPreProc">#include </span><span class="synConstant">&lt;iostream&gt;</span>
<span class="synPreProc">#include </span><span class="synConstant">&lt;string&gt;</span>


<span class="synType">namespace</span> x3 = boost::spirit::x3;

<span class="synType">namespace</span> detail {

  x3::rule&lt;<span class="synType">class</span> constant, <span class="synType">int</span>&gt; <span class="synType">const</span> constant;

  <span class="synType">auto</span> <span class="synType">const</span> constant_def = x3::int_;

  BOOST_SPIRIT_DEFINE(constant);

} <span class="synComment">// namespace detail</span>

<span class="synStatement">using</span> detail::constant;

<span class="synType">int</span> main()
{

  std::string src{ <span class="synConstant">&quot;123&quot;</span> };

  <span class="synType">auto</span> first = std::cbegin(src);
  <span class="synType">auto</span> <span class="synType">const</span> last = std::cend(src);


  <span class="synType">int</span> result;
  <span class="synType">bool</span> success = x3::phrase_parse(first, last, constant, x3::ascii::space, result);

  <span class="synStatement">if</span> (!success || first != last) {
    <span class="synComment">// parse failed</span>
    std::cerr &lt;&lt; <span class="synConstant">&quot;Parse failed.&quot;</span> &lt;&lt; std::endl;
  } <span class="synStatement">else</span> {
    std::cout &lt;&lt; <span class="synConstant">&quot;Parse: &quot;</span> &lt;&lt; result &lt;&lt; std::endl;
  }
}
</pre>

<p><code>namespace detail</code> の中身がパーサの定義になっています．
(<code>namespace</code> を分けたのは，<code>constant</code> そのものの名前以外を隠すためです．)
<code>X3</code> では，<code>x3::rule&lt;class, result type&gt;</code> というテンプレートクラスがパーサルールになります．<code>class</code> 部分は今は前方宣言だけで構わないようです．(後で <code>on_error</code> とかの属性を付与する際に必要になる？)<br/>
<code>result type</code> はそのパーサが返すべき値になります．</p>

<p>つまり，<code>x3::rule&lt;class constant, int&gt; const constant;</code> は，<code>int</code> 型の値を返す，特別な属性を持たない <code>constant</code> という名前のパーサを宣言したことになります．</p>

<p>そして，その実装は <code>constant_def</code> という名前で与えられます．<br/>
<code>hogehoge_def</code> という名前にする規約のようです．(<code>BOOST_SPIRIT_DEFINE</code> 部分を書き換えれば違う名前にしても大丈夫のようだが，素直に従っておけば良さそう)<br/>
今回はシンプルに <code>x3::int_</code> そのものとしています．</p>

<p>最後に <code>BOOST_SPIRIT_DEFINE</code> することで，<code>constant</code> というパーサの宣言と，<code>constant_def</code> という実装をひも付けます．</p>

<p>使い方は今までと全く同じです．</p>

<h2>使ってみる</h2>

<p>セマンティックアクションと名前付きパーサの両方を使って，整数を受け取って2倍にした値を返すパーサを書いてみます．</p>

<pre class="code lang-cpp" data-lang="cpp" data-unlink><span class="synPreProc">#include </span><span class="synConstant">&lt;boost/spirit/home/x3.hpp&gt;</span>

<span class="synPreProc">#include </span><span class="synConstant">&lt;iostream&gt;</span>
<span class="synPreProc">#include </span><span class="synConstant">&lt;string&gt;</span>


<span class="synType">namespace</span> x3 = boost::spirit::x3;

<span class="synType">namespace</span> detail {

  <span class="synType">auto</span> <span class="synType">const</span> twice = [](<span class="synType">auto</span>&amp; ctx) { _val(ctx) = _attr(ctx) * <span class="synConstant">2</span>; };

  x3::rule&lt;<span class="synType">class</span> constant, <span class="synType">int</span>&gt; <span class="synType">const</span> constant;

  <span class="synType">auto</span> <span class="synType">const</span> constant_def = x3::int_[twice];

  BOOST_SPIRIT_DEFINE(constant);

} <span class="synComment">// namespace detail</span>

<span class="synStatement">using</span> detail::constant;

<span class="synType">int</span> main()
{

  std::string src{ <span class="synConstant">&quot;123&quot;</span> };

  <span class="synType">auto</span> first = std::cbegin(src);
  <span class="synType">auto</span> <span class="synType">const</span> last = std::cend(src);


  <span class="synType">int</span> result;
  <span class="synType">bool</span> success = x3::phrase_parse(first, last, constant, x3::ascii::space, result);

  <span class="synStatement">if</span> (!success || first != last) {
    <span class="synComment">// parse failed</span>
    std::cerr &lt;&lt; <span class="synConstant">&quot;Parse failed.&quot;</span> &lt;&lt; std::endl;
  } <span class="synStatement">else</span> {
    std::cout &lt;&lt; <span class="synConstant">&quot;Parse: &quot;</span> &lt;&lt; result &lt;&lt; std::endl;
  }
}
</pre>

<p>セマンティックアクション内では，パーサの <code>result type</code> に <code>_val(ctx)</code> でアクセス出来ます．
<code>_val(ctx)</code> は参照を返すので，ここに適当な値を代入してやれば，パーサの返り値にすることが出来ます．<br/>
<code>_attr(ctx)</code> は先程と同じです．<code>x3::int_</code> の attribute は <code>int</code> です．</p>

<p>実行してみると，<code>Parse: 246</code> が返るはずです．</p>

<h2>一旦まとめ</h2>

<p>セマンティックアクションと名前付きパーサの宣言と定義をまとめました．<br/>
<code>Qi</code> のころより，セマンティックアクションはかなり書きやすくなっている気がします．<br/>
今回の例では，<code>_attr(ctx)</code> が単純な値だったのでわかりやすいですが，<code>x3::int_ &gt;&gt; x3::double_</code> の <code>_attr(ctx)</code> は <code>Boost.Fusion</code> が登場したりしてちょっとややこしそうです．</p>

<p>また，<a class="keyword" href="http://d.hatena.ne.jp/keyword/%BA%C6%B5%A2">再帰</a>的パーサを定義できるようにパーサの宣言をまとめたのに，<a class="keyword" href="http://d.hatena.ne.jp/keyword/%BA%C6%B5%A2">再帰</a>的パーサを書いていませんが，これは別記事に電卓でも作ってまとめたいと思います．</p>

---

---
