---
title: "Boost.Spirit.X3 で簡易電卓を実装 1"
date: 2015-12-18T14:27:05.000Z
tags: []
---

<p><iframe src="http://agtn.hatenablog.com/embed/2015/12/17/190505" title="Boost.Spirit.X3 の練習1 - プログラミングのメモ帳➚" class="embed-card embed-blogcard" scrolling="no" frameborder="0" style="display: block; width: 100%; height: 190px; max-width: 500px; margin: 10px 0px;"></iframe><cite class="hatena-citation"><a href="http://agtn.hatenablog.com/entry/2015/12/17/190505">agtn.hatenablog.com</a></cite>
<iframe src="http://agtn.hatenablog.com/embed/2015/12/17/232003" title="Boost.Spirit.X3 の練習 2 - プログラミングのメモ帳➚" class="embed-card embed-blogcard" scrolling="no" frameborder="0" style="display: block; width: 100%; height: 190px; max-width: 500px; margin: 10px 0px;"></iframe><cite class="hatena-citation"><a href="http://agtn.hatenablog.com/entry/2015/12/17/232003">agtn.hatenablog.com</a></cite></p>

<p>引き続き，<code>Boost.Spirit.X3</code> です．<br/>
今回は，前回までの知識をつかって，簡易電卓を実装してみます．</p>

<h2>仕様</h2>

<p>今回定義する電卓は，</p>

<ul>
<li><code>+</code></li>
<li><code>-</code></li>
<li><code>*</code></li>
<li><code>/</code></li>
</ul>

<p>の 4 つの演算と単項の <code>-</code> をサポートします．<br/>
また，整数型のみを扱うものとします．<br/>
<code>(</code>, <code>)</code> でくくることで，<a class="keyword" href="http://d.hatena.ne.jp/keyword/%B1%E9%BB%BB%BB%D2">演算子</a>の結合優先順位を書き換えられ，<code>*</code> と <code>/</code> は <code>+</code> と <code>-</code> より優先されるとします．</p>

<p>要するに整数の四則演算のみをサポートする電卓です．</p>

<p>このような電卓を実装するサンプルは <code>Boost.Spirit.X3</code> 以外のライブラリ/<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%C4%A1%BC%A5%EB">ツール</a>でも大量に出てくると思います．<br/>
今回は，<a class="keyword" href="http://d.hatena.ne.jp/keyword/%B9%BD%CA%B8%B2%F2%C0%CF">構文解析</a>そのものというよりは <code>Boost.Spirit.X3</code> の使い方についてメモしたいので，<a class="keyword" href="http://d.hatena.ne.jp/keyword/%B9%BD%CA%B8%B2%F2%C0%CF">構文解析</a>そのものの話はぐぐってみてください．</p>

<h2>パーサの骨格</h2>

<p><a class="keyword" href="http://d.hatena.ne.jp/keyword/%B1%E9%BB%BB%BB%D2">演算子</a>の結合規則をサポートするために，<code>primary</code>(定数と <code>()</code> で囲まれた式), <code>neg_expr</code>(単項 <code>-</code>), <code>mul_expr</code>(<code>*</code>, <code>/</code>), <code>add_expr</code>(<code>+</code>, <code>-</code>), <code>expression</code> というパーサをそれぞれ定義します．<br/>
先頭から順に結合強度が強くなっています．(<code>expression</code> が最弱, <code>primary</code> が最強)</p>

<p><code>primary</code> は <code>()</code> で囲まれた式，つまり <code>"(" &gt; expression &gt; ")"</code> を受け付ける必要があり，また，<code>primary</code> 自体も <code>expression</code> の一部です．<br/>
したがって，この規則を定義するためには，<a class="keyword" href="http://d.hatena.ne.jp/keyword/%BA%C6%B5%A2">再帰</a>的なパーサを記述する必要があります．</p>

<p><code>X3</code> で<a class="keyword" href="http://d.hatena.ne.jp/keyword/%BA%C6%B5%A2">再帰</a>的なパーサを記述する方法は<a href="http://agtn.hatenablog.com/entry/2015/12/17/232003">前回の記事</a>にまとめました．</p>

<pre class="code lang-cpp" data-lang="cpp" data-unlink>  <span class="synType">struct</span> primary;
  <span class="synType">struct</span> neg_expr;
  <span class="synType">struct</span> mul_expr;
  <span class="synType">struct</span> add_expr;
  <span class="synType">struct</span> expression;

  x3::rule&lt;primary, <span class="synType">int</span>&gt; <span class="synType">const</span> primary;
  x3::rule&lt;neg_expr, <span class="synType">int</span>&gt; <span class="synType">const</span> neg_expr;
  x3::rule&lt;mul_expr, <span class="synType">int</span>&gt; <span class="synType">const</span> mul_expr;
  x3::rule&lt;add_expr, <span class="synType">int</span>&gt; <span class="synType">const</span> add_expr;
  x3::rule&lt;expression, <span class="synType">int</span>&gt; <span class="synType">const</span> expression;
</pre>

<p>それぞれのパーサは attribute として整数型を持ちます．ここに演算結果が格納されることになります．<br/>
<code>struct primary</code> などは，今は前方宣言のみで十分です．<code>on_error</code> などを実装したくなった時に定義します．</p>

<h2>primary</h2>

<p>まずは <code>primary</code> を定義します.<br/>
<code>primary</code> は整数定数か， <code>()</code> で囲まれた <code>expression</code> を受理します．</p>

<pre class="code lang-cpp" data-lang="cpp" data-unlink><span class="synType">auto</span> <span class="synType">const</span> primary_def =
    x3::int_
  | <span class="synConstant">&quot;(&quot;</span> &gt; expression &gt; <span class="synConstant">&quot;)&quot;</span>
  ;
</pre>

<p>attribute を考慮しなければこんな感じでしょうか．<code>expression</code> は既に宣言されているので使用可能です．(<code>expression</code> の実装がこの時点で見えていなくても使用できます.)</p>

<p>単純に attribute を結果として返すセマンティックアクションはこの後もよく出てくるので，ヘルパとして定義しておきます．</p>

<pre class="code lang-cpp" data-lang="cpp" data-unlink><span class="synType">namespace</span> detail {

  decltype(<span class="synType">auto</span>) assign()
  {
    <span class="synStatement">using</span> x3::_attr;
    <span class="synStatement">using</span> x3::_val;
    <span class="synStatement">return</span> [](<span class="synType">auto</span>&amp;&amp; ctx) { _val(ctx) = _attr(ctx); };
  }

} <span class="synComment">// namespace detail</span>
</pre>

<p><code>assign</code> は attribute を結果に代入する関数オブジェクトを返します．<br/>
関数にする必要が特にありませんが，この後出てくるヘルパと見た目を合わせたいので関数にしました．</p>

<p>これを使うと，</p>

<pre class="code lang-cpp" data-lang="cpp" data-unlink><span class="synType">auto</span> primary_def =
    x3::int_[detail::assign()]
  | (<span class="synConstant">&quot;(&quot;</span> &gt; expression &gt; <span class="synConstant">&quot;)&quot;</span>)[detail::assign()]
  ;
</pre>

<p>こんな感じで <code>primary</code> が定義できます．</p>

<h2>単項マイナス</h2>

<p>次に <code>neg_expr</code> を定義します．
セマンティックアクションを考えなければ，</p>

<pre class="code lang-cpp" data-lang="cpp" data-unlink><span class="synType">auto</span> <span class="synType">const</span> neg_expr_def =
    primary
  | <span class="synConstant">&quot;-&quot;</span> &gt; primary
  ;
</pre>

<p>となります．<br/>
<code>"-" &gt; primary</code> のセマンティックアクションとしては，attribute を符号反転して結果に格納するというアクションが求められます．<br/>
ここはちょっと汎用的に，attribute に関数オブジェクトを適用して結果に格納するアクションを返すような関数を定義して解決してみます．</p>

<pre class="code lang-cpp" data-lang="cpp" data-unlink><span class="synType">namespace</span> detail {
  <span class="synType">template</span> &lt;<span class="synType">typename</span> F&gt;
  decltype(<span class="synType">auto</span>) assign_f(F&amp;&amp; func)
  {
    <span class="synStatement">return</span> [func](<span class="synType">auto</span>&amp;&amp; ctx) { _val(ctx) = func(_attr(ctx)); };
  }
} <span class="synComment">// namespace detail</span>
</pre>

<p><code>assign_f</code> は <code>assign</code> と異なり，関数オブジェクトを１つ引数に取ります．<br/>
そして，その関数オブジェクトを <code>_attr(ctx)</code> に適用し結果に格納します．</p>

<p>これを使って，<code>neg_expr</code> は</p>

<pre class="code lang-cpp" data-lang="cpp" data-unlink><span class="synType">auto</span> <span class="synType">const</span> neg_expr_def =
    primary[detail::assign()]
  | (<span class="synConstant">&quot;-&quot;</span> &gt; primary)[detail::assign(std::negate&lt;<span class="synType">int</span>&gt;<span class="synError">{</span>})]
  ;
</pre>

<p>となります．<code>std::negate</code> は <code>&lt;functional&gt;</code> で定義された型で，ここでは <code>int</code> 型の値を符号反転する関数オブジェクトとして使用しています．</p>

<h2>乗除</h2>

<p>次に結合強度が強いのは <code>*</code> と <code>/</code> です．<br/>
ちょっとわかりにくいですが，セマンティックアクションを無視すれば，<code>mul_expr</code> は</p>

<pre class="code lang-cpp" data-lang="cpp" data-unlink><span class="synType">auto</span> <span class="synType">const</span> mul_expr_def =
    neg_expr
    &gt;&gt; *(
        (<span class="synConstant">&quot;*&quot;</span> &gt;&gt; neg_expr)
      | (<span class="synConstant">&quot;/&quot;</span> &gt;&gt; neg_expr)
    )
  ;
</pre>

<p>と定義できます．<code>mul_expr</code> は <code>1</code> や <code>(1 + 2)</code>, <code>-1</code> の後に，<code>* 1</code> とか <code>/ -3</code> とか <code>* (1 - 2)</code> とかが 0 回以上現れるような式です．<br/>
<code>1 * -2</code> はちょっと気持ち悪い気もしますが… 今気がついたので許してください．</p>

<p>セマンティックアクションとしては，<code>("*" &gt;&gt; neg_expr)</code> が現れる度に，<code>_val(ctx)</code> を <code>_val(ctx) * _attr(ctx)</code> に更新すれば良いです．<br/>
始めの <code>neg_expr</code> の結果を <code>_val(ctx)</code> に格納すれば，<code>_val(ctx)</code> は常に現在の計算結果を表すことになります．<code>("*" &gt;&gt; neg_expr)</code> は現在の計算結果に，今処理した式(<code>*</code> の後に続く式のこと) を処理した結果をかければ良いということです．</p>

<p>というわけで分かりにくいとは思いますが，ほしいアクションは，</p>

<pre class="code lang-cpp" data-lang="cpp" data-unlink>[](<span class="synType">auto</span>&amp;&amp; ctx) { _val(ctx) = _val(ctx) * _attr(ctx); }
</pre>

<p>です．</p>

<p>さて，では <code>/</code> の場合を考えます．<br/>
<code>/</code> の場合であってもほとんどは <code>*</code> と同じであることがわかります．<br/>
ほしいアクションは</p>

<pre class="code lang-cpp" data-lang="cpp" data-unlink>[](<span class="synType">auto</span>&amp;&amp; ctx) { _val(ctx) = _val(ctx) / _attr(ctx); }
</pre>

<p>であり，<code>*</code> と <code>/</code> の違いしか有りません．</p>

<p>そこでこれも関数にまとめてしまいます．</p>

<pre class="code lang-cpp" data-lang="cpp" data-unlink><span class="synType">namespace</span> detail {

  <span class="synType">template</span> &lt;<span class="synType">typename</span> Op&gt;
  decltype(<span class="synType">auto</span>) calc_op(Op&amp;&amp; op)
  {
    <span class="synStatement">return</span> [op](<span class="synType">auto</span>&amp;&amp; ctx) { _val(ctx) = op(_val(ctx), _attr(ctx)); };
  }

} <span class="synComment">// namespace detail</span>
</pre>

<p>こんな関数を定義して，</p>

<pre class="code lang-cpp" data-lang="cpp" data-unlink><span class="synType">auto</span> <span class="synType">const</span> mul_expr_def =
    neg_expr[detail::assign()]
    &gt;&gt; *(
        (<span class="synConstant">&quot;*&quot;</span> &gt;&gt; neg_expr)[detail::calc_op(std::multiplies&lt;<span class="synType">int</span>&gt;<span class="synError">{</span>})]
      | (<span class="synConstant">&quot;/&quot;</span> &gt;&gt; neg_expr)[detail::calc_op(std::divides&lt;<span class="synType">int</span>&gt;<span class="synError">{</span>})]
    )
  ;
</pre>

<p>と使います．<br/>
<code>calc_op</code> は関数オブジェクトを引数に取り，<code>_val(ctx)</code> と <code>_attr(ctx)</code> に適用した結果を格納するアクションを返します．</p>

<p><code>add_expr</code> は <code>mul_expr</code> とほぼおなじなので詳細はスキップします．</p>

<h2>expression</h2>

<p>最後に <code>expression</code> です．これは単純に <code>add_expr</code> と一致します．<br/>
命名のわかりやすさと，今後拡張していく際に便利そうということで分けてあるだけです．</p>

<pre class="code lang-cpp" data-lang="cpp" data-unlink><span class="synType">auto</span> <span class="synType">const</span> expression_def =
    add_expr[detail::assign()]
  ;
</pre>

<h2>確認</h2>

<p>コード全体を掲載します．</p>

<pre class="code lang-cpp" data-lang="cpp" data-unlink><span class="synPreProc">#include </span><span class="synConstant">&lt;boost/spirit/home/x3.hpp&gt;</span>

<span class="synPreProc">#include </span><span class="synConstant">&lt;iostream&gt;</span>
<span class="synPreProc">#include </span><span class="synConstant">&lt;string&gt;</span>
<span class="synPreProc">#include </span><span class="synConstant">&lt;functional&gt;</span>

<span class="synType">namespace</span> x3 = boost::spirit::x3;

<span class="synType">namespace</span> grammar {

  <span class="synType">namespace</span> detail {

    decltype(<span class="synType">auto</span>) assign()
    {
      <span class="synStatement">using</span> x3::_attr;
      <span class="synStatement">using</span> x3::_val;
      <span class="synStatement">return</span> [](<span class="synType">auto</span>&amp;&amp; ctx) { _val(ctx) = _attr(ctx); };
    }

    <span class="synType">template</span> &lt;<span class="synType">typename</span> F&gt;
    decltype(<span class="synType">auto</span>) assign_f(F&amp;&amp; func)
    {
      <span class="synStatement">return</span> [func](<span class="synType">auto</span>&amp;&amp; ctx) { _val(ctx) = func(_attr(ctx)); };
    }

    <span class="synType">template</span> &lt;<span class="synType">typename</span> Op&gt;
    decltype(<span class="synType">auto</span>) calc_op(Op&amp;&amp; op)
    {
      <span class="synStatement">return</span> [op](<span class="synType">auto</span>&amp;&amp; ctx) { x3::_val(ctx) = op(x3::_val(ctx), x3::_attr(ctx)); };
    }

  } <span class="synComment">// namespace detail</span>

  <span class="synType">struct</span> primary;
  <span class="synType">struct</span> neg_expr;
  <span class="synType">struct</span> mul_expr;
  <span class="synType">struct</span> add_expr;
  <span class="synType">struct</span> expression;

  x3::rule&lt;primary, <span class="synType">int</span>&gt; <span class="synType">const</span> primary;
  x3::rule&lt;neg_expr, <span class="synType">int</span>&gt; <span class="synType">const</span> neg_expr;
  x3::rule&lt;mul_expr, <span class="synType">int</span>&gt; <span class="synType">const</span> mul_expr;
  x3::rule&lt;add_expr, <span class="synType">int</span>&gt; <span class="synType">const</span> add_expr;
  x3::rule&lt;expression, <span class="synType">int</span>&gt; <span class="synType">const</span> expression;

  <span class="synType">auto</span> <span class="synType">const</span> primary_def =
      x3::int_[detail::assign()]
    | (<span class="synConstant">&quot;(&quot;</span> &gt; expression &gt; <span class="synConstant">&quot;)&quot;</span>)[detail::assign()]
    ;

  <span class="synType">auto</span> <span class="synType">const</span> neg_expr_def =
      primary[detail::assign()]
    | (<span class="synConstant">&quot;-&quot;</span> &gt; primary)[detail::assign_f(std::negate&lt;<span class="synType">int</span>&gt;<span class="synError">{</span>})]
    ;

  <span class="synType">auto</span> <span class="synType">const</span> mul_expr_def =
      neg_expr[detail::assign()]
      &gt;&gt; *(
          (<span class="synConstant">&quot;*&quot;</span> &gt;&gt; neg_expr)[detail::calc_op(std::multiplies&lt;<span class="synType">int</span>&gt;<span class="synError">{</span>})]
        | (<span class="synConstant">&quot;/&quot;</span> &gt;&gt; neg_expr)[detail::calc_op(std::divides&lt;<span class="synType">int</span>&gt;<span class="synError">{</span>})]
      )
    ;

  <span class="synType">auto</span> <span class="synType">const</span> add_expr_def =
      mul_expr[detail::assign()]
      &gt;&gt; *(
          (<span class="synConstant">&quot;+&quot;</span> &gt; mul_expr)[detail::calc_op(std::plus&lt;<span class="synType">int</span>&gt;<span class="synError">{</span>})]
        | (<span class="synConstant">&quot;-&quot;</span> &gt; mul_expr)[detail::calc_op(std::minus&lt;<span class="synType">int</span>&gt;<span class="synError">{</span>})]
      )
    ;

  <span class="synType">auto</span> <span class="synType">const</span> expression_def =
      add_expr[detail::assign()]
    ;

  BOOST_SPIRIT_DEFINE(
      primary,
      neg_expr,
      mul_expr,
      add_expr,
      expression
      );

} <span class="synComment">// namespace grammar</span>
<span class="synStatement">using</span> grammar::expression;

<span class="synType">int</span> main()
{
  std::string str;
  std::getline(std::cin, str);

  <span class="synType">auto</span> first(std::cbegin(str));
  <span class="synType">auto</span> <span class="synType">const</span> last(std::cend(str));

  <span class="synType">int</span> result;
  <span class="synType">bool</span> success = x3::phrase_parse(first, last, expression, x3::ascii::space, result);

  <span class="synStatement">if</span> (!success || first != last) {
    std::cerr &lt;&lt; <span class="synConstant">&quot;Parse failed.&quot;</span> &lt;&lt; std::endl;
    <span class="synStatement">return</span> <span class="synConstant">1</span>;
  }

  std::cout &lt;&lt; <span class="synConstant">&quot;Parsed: &quot;</span> &lt;&lt; result &lt;&lt; std::endl;
  <span class="synStatement">return</span> <span class="synConstant">0</span>;
}
</pre>

<p>実行してみます．</p>

<pre class="code" data-lang="" data-unlink>$ clang++ -std=c++14 main.cpp
$ ./a.out
1 + 2 * 3
Parsed: 7
$ ./a.out
(1 + 2) * 3
Parsed: 9</pre>

<p><a class="keyword" href="http://d.hatena.ne.jp/keyword/%B1%E9%BB%BB%BB%D2">演算子</a>の優先順位が正しく解決できていることが確認出来ます．</p>

<h2>まとめ</h2>

<p>今回は，セマンティックアクションで計算自体を行ってしまいましたが，普通は抽象<a class="keyword" href="http://d.hatena.ne.jp/keyword/%B9%BD%CA%B8%CC%DA">構文木</a>(AST) に変換するためにセマンティックアクションを使うのが正道だと思います．<br/>
<code>X3</code> は AST のための色々を提供してくれていますが，自前で作った AST でもちょっと苦労はするかもしれませんが変換できるはずなので，時間があれば，自前 AST に変換してから実行する電卓も作ってみたいと思います．</p>

<p>また，AST に変換して計算する場合，AST に位置情報を付与することで，エラーレポートが便利になったりするはずです( 0 除算のエラーを通知する際，どの部分でのエラーなのかを吐いてくれればうれしいですよね).<br/>
パース失敗時にもどこで失敗したのかをレポートしてくれたほうが便利です．<br/>
<code>X3</code> で <code>on_error</code>, <code>on_success</code> を使ってこれらを実装してみようと考えています．</p>

<p>今回のコードでは <code>decltype(auto)</code> など，<a class="keyword" href="http://d.hatena.ne.jp/keyword/C%2B%2B">C++</a>14 の機能を使っています．<code>X3</code> は <a class="keyword" href="http://d.hatena.ne.jp/keyword/C%2B%2B">C++</a>14 前提のライブラリなので，迷いなくこういった機能を使用できて幸せデスね．</p>

---

---
