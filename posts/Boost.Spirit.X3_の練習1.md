---
title: "Boost.Spirit.X3 の練習1"
date: 2015-12-17T10:05:05.000Z
tags: []
---

<h1>Boost.Spirit.X3 の練習1</h1>

<p><a href="http://ciere.com/cppnow15/x3_docs/">Boost.Spirit.X3</a> という <a class="keyword" href="http://d.hatena.ne.jp/keyword/C%2B%2B">C++</a> のための パーサ<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D3%A5%CD%A1%BC%A5%BF">コンビネータ</a>ライブラリを使ってみています．<br/>
<code>Boost.Spirit</code> というと， <a class="keyword" href="http://d.hatena.ne.jp/keyword/C%2B%2B">C++</a> の黒魔術の塊みたいなイメージがあります． ちなみに <code>Boost.Spirit.Qi</code> が安定版のパーサ<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D3%A5%CD%A1%BC%A5%BF">コンビネータ</a>ライブラリで， <code>X3</code> はまだ開発中のようなので注意してください．</p>

<p><code>X3</code> は <code>Qi</code> と異なり，<a class="keyword" href="http://d.hatena.ne.jp/keyword/C%2B%2B">C++</a>14 以降の規格を前提にしています．そのため，<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B8%A5%A7%A5%CD%A5%EA%A5%C3%A5%AF">ジェネリック</a>ラムダなどを用いてよりわかりやすいプログラムが書けるようになっているようです．<br/>
<code>Qi</code> は<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D1%A5%A4%A5%EB">コンパイル</a>時間が爆発していたのですが， <code>X3</code> だと多少マシのようです．</p>

<h2>第一歩</h2>

<p>まずは猛烈にシンプルなパーサを使ってみましょう．
<code>X3</code> も <code>Qi</code> 同様に定義済みのパーサがあるので，それを単純に使用するだけのサンプルです．</p>

<pre class="code lang-cpp" data-lang="cpp" data-unlink><span class="synPreProc">#include </span><span class="synConstant">&lt;boost/spirit/home/x3.hpp&gt;</span>

<span class="synPreProc">#include </span><span class="synConstant">&lt;iostream&gt;</span>
<span class="synPreProc">#include </span><span class="synConstant">&lt;string&gt;</span>


<span class="synType">int</span> main()
{
  <span class="synType">namespace</span> x3 = boost::spirit::x3;

  <span class="synType">int</span> result;
  std::string src{ <span class="synConstant">&quot;123&quot;</span> };

  <span class="synType">auto</span> first = std::cbegin(src);
  <span class="synType">auto</span> <span class="synType">const</span> last = std::cend(src);
  <span class="synType">bool</span> success = x3::parse(first, last, x3::int_, result);

  <span class="synStatement">if</span> (!success || first != last) {
    <span class="synComment">// parse failed</span>
    std::cerr &lt;&lt; <span class="synConstant">&quot;Parse failed.&quot;</span> &lt;&lt; std::endl;
  } <span class="synStatement">else</span> {
    std::cout &lt;&lt; <span class="synConstant">&quot;Parse: &quot;</span> &lt;&lt; result &lt;&lt; std::endl;
  }
}
</pre>

<h3><a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D1%A5%A4%A5%EB">コンパイル</a> &amp; 実行</h3>

<pre class="code" data-lang="" data-unlink>$ clang++ -std=c++14 int_parser.cpp
$ ./a.out
Parse: 123</pre>

<p><code>Boost.Spirit.X3</code> を使用する場合は, <code>#include &lt;boost/spirit/home/x3.hpp&gt;</code> とすればオッケーです．(これは使用していない機能のヘッダも読み込んでしまっているので，<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D1%A5%A4%A5%EB">コンパイル</a>は重くなります… が，これ以降使う機能を増やす度にヘッダを書き換えるのは面倒ですし，<code>X3</code> の機能の多くを使用するプログラムの場合は，大差ないと思います．)</p>

<p><a class="keyword" href="http://d.hatena.ne.jp/keyword/%CC%BE%C1%B0%B6%F5%B4%D6">名前空間</a>は <code>boost::spirit::x3</code> です．頻繁に出てくるので <code>namespace x3</code> で<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%A8%A5%A4%A5%EA%A5%A2%A5%B9">エイリアス</a>しました．</p>

<p>実際にパースしている部分は,</p>

<pre class="code lang-cpp" data-lang="cpp" data-unlink><span class="synType">bool</span> success = x3::parse(first, last, x3::int_, result);
</pre>

<p>の部分です．<br/>
<code>x3::parse</code> は，第一引数にソース文字列の先頭<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%A4%A5%C6%A5%EC%A1%BC%A5%BF">イテレータ</a>，第二引数にソース文字列の終端<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%A4%A5%C6%A5%EC%A1%BC%A5%BF">イテレータ</a>をとります．<br/>
第三引数が，パーサの定義です．ここでは， <code>x3::int_</code> という定義済みパーサを使用しました． これは, 整数を表す文字列を受けて，それを整数に変換するパーサです． たとえば <code>"1"</code> を <code>1</code> に変換します．整数以外にあたった場合はマッチせず，パースに失敗します．(<code>x3::parse</code> の返り値が <code>false</code> になる)
第四引数は，指定したパーサの返す値です．ここちょっと説明が難しいですね．<br/>
<code>x3::int_</code> は <code>int</code> 型の値を返すパーサです(これを <code>x3::int_</code> の attribute が <code>int</code> 型であると表現している？).<br/>
そこで，<code>x3::int_</code> の返す値を格納する変数として，<code>int result</code> の参照を渡しているという感じです．<br/>
パースが成功していれば，<code>result</code> の中身は <code>x3::int_</code> の返り値になっているはずです．</p>

<p>パースの成否判定は <code>success</code> と <code>first == last</code> で行います．</p>

<pre class="code lang-cpp" data-lang="cpp" data-unlink><span class="synStatement">if</span> (!success || first != last) {
</pre>

<p><code>success</code> はそもそもパーサにソースがマッチしなかった場合, <code>false</code> になります．<br/>
また，<code>"123abc"</code> に <code>x3::int_</code> をマッチさせると，<code>"123"</code> だけが消費され，<code>"abc"</code> が残ります．この時，<code>first</code> は <code>"a"</code> の位置まで進んでいます．
もしソースが先頭から終端まで，パーサにマッチしていたならば，<code>first == last</code> となるはずです．</p>

<p>というわけでこのプログラムは，<code>x3::int_</code> に <code>"123"</code> をパースさせるプログラムでした．結果，きちんと整数の <code>123</code> が取得できていることがわかります．</p>

<h2><a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D3%A5%CD%A1%BC%A5%BF">コンビネータ</a></h2>

<p><code>Boost.Spirit.X3</code> はパーサ<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D3%A5%CD%A1%BC%A5%BF">コンビネータ</a>ライブラリです．個々のパーサを組み合わせてみましょう．</p>

<p><code>" ( 1234.5)"</code> とか <code>"(67.8 )  "</code> にマッチして，<code>double</code> を返すようなパーサを定義してみます．</p>

<pre class="code lang-cpp" data-lang="cpp" data-unlink><span class="synType">auto</span> parser = x3::lit(<span class="synConstant">&quot;(&quot;</span>) &gt;&gt; x3::double_ &gt;&gt; x3::lit(<span class="synConstant">&quot;)&quot;</span>);
<span class="synType">bool</span> success = x3::phrase_parse(first, last, parser, x3::ascii::space, result);
</pre>

<p>まずは <code>parser</code> の定義です．
<code>x3::lit</code> は<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%EA%A5%C6%A5%E9%A5%EB">リテラル</a>を表します．引数にとった文字列にマッチし，何も返さないパーサです．<code>x3::lit("(")</code> は <code>(</code> にマッチし，何も返さないパーサということになります．<br/>
<code>x3::double_</code> は <code>x3::int_</code> と同じく，定義済みパーサで，<a class="keyword" href="http://d.hatena.ne.jp/keyword/%C9%E2%C6%B0%BE%AE%BF%F4%C5%C0%BF%F4">浮動小数点数</a>にマッチしその値を返します．</p>

<p>そして重要なのが，<code>&gt;&gt;</code> です．<br/>
これは，「左辺にマッチした後，右辺にマッチするパーサ」を作り出す<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D3%A5%CD%A1%BC%A5%BF">コンビネータ</a>です．<br/>
ここでは，<code>x3::lit("(") &gt;&gt; x3::double_</code> ですから，<code>(</code> にマッチした後，<a class="keyword" href="http://d.hatena.ne.jp/keyword/%C9%E2%C6%B0%BE%AE%BF%F4%C5%C0%BF%F4">浮動小数点数</a>にマッチするパーサ，ということになります.</p>

<p>通常，<code>&gt;&gt;</code> は左辺の返す値と右辺の返す値のタプルを返します．(<code>x3::int_ &gt;&gt; x3::double_</code> ならば，<code>int</code> と <code>double</code> のタプル)<br/>
しかし，左辺右辺どちらか一方が値を返さない(正確には <code>x3::unused_type</code> を返す) 場合には，もう一方の値だけを返します．<br/>
つまり，<code>x3::lit("(") &gt;&gt; x3::double_</code> は <code>double</code> だけを返し，<code>&gt;&gt; x3::lit(")")</code> と続けても <code>double</code> だけが返ります．</p>

<p>次に，空白の読み飛ばしについてです．</p>

<pre class="code lang-cpp" data-lang="cpp" data-unlink><span class="synType">bool</span> success = x3::phrase_parse(first, last, parser, x3::ascii::space, result);
</pre>

<p>ひとつ目の例と異なり，<code>x3::parse</code> ではなく <code>x3::phrase_parse</code> を使っています．<br/>
こちらは，<code>attribute</code> を示す最後の引数の前に，スキップパーサを取ります．
スキップパーサとは，文字通り，スキップしてほしい文字列にマッチするパーサです．<br/>
<code>x3::ascii::space</code> は定義済みのパーサで，スペース，改行文字，タブにマッチします．したがって，これらの文字はスキップされます．
スキップ判定のタイミングは <code>&gt;&gt;</code> の部分です．つまり，"12    3" は <code>x3::int_</code> でパースすると, <code>12</code> までしかマッチしません．<code>x3::int_ &gt;&gt; x3::int_</code> とすることで，スペースがスキップされます．</p>

<pre class="code lang-cpp" data-lang="cpp" data-unlink><span class="synPreProc">#include </span><span class="synConstant">&lt;boost/spirit/home/x3.hpp&gt;</span>

<span class="synPreProc">#include </span><span class="synConstant">&lt;iostream&gt;</span>
<span class="synPreProc">#include </span><span class="synConstant">&lt;string&gt;</span>


<span class="synType">int</span> main()
{
  <span class="synType">namespace</span> x3 = boost::spirit::x3;

  std::string src{ <span class="synConstant">&quot;( 123.4 )&quot;</span> };

  <span class="synType">auto</span> first = std::cbegin(src);
  <span class="synType">auto</span> <span class="synType">const</span> last = std::cend(src);
  <span class="synType">double</span> result;
  <span class="synType">auto</span> parser = x3::lit(<span class="synConstant">&quot;(&quot;</span>) &gt;&gt; x3::double_ &gt;&gt; x3::lit(<span class="synConstant">&quot;)&quot;</span>);
  <span class="synType">bool</span> success = x3::phrase_parse(first, last, parser, x3::ascii::space, result);

  <span class="synStatement">if</span> (!success || first != last) {
    <span class="synComment">// parse failed</span>
    std::cerr &lt;&lt; <span class="synConstant">&quot;Parse failed.&quot;</span> &lt;&lt; std::endl;
  } <span class="synStatement">else</span> {
    std::cout &lt;&lt; <span class="synConstant">&quot;Parse: &quot;</span> &lt;&lt; result &lt;&lt; std::endl;
  }
}
</pre>

<p>ちなみに，<code>x3::lit("(") &gt;&gt; x3::double_ &gt;&gt; x3::lit(")")</code> の部分ですが，<code>operator&lt;&lt;</code> の引数の内，片方がパーサであれば，<code>char const*</code> から暗黙変換が働くので， <code>"(" &gt;&gt; x3::double_ &gt;&gt; ")"</code> と書くことが出来ます．</p>

<p><a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D3%A5%CD%A1%BC%A5%BF">コンビネータ</a>は他にもあります．</p>

<ul>
<li><code>|</code>

<ul>
<li><code>a | b</code> で，<code>a</code> にマッチするか <code>b</code> にマッチするか</li>
<li><code>a</code> を先に試すので，両方にマッチする場合でも <code>a</code> にマッチしたことになる (PEG の特徴ですね)</li>
<li>返り値は <code>boost::variant&lt;a, b&gt;</code> です．<code>a</code> と <code>b</code> が同じ型を返す場合はその型を返します</li>
</ul>
</li>
<li><code>&gt;</code>

<ul>
<li><code>&gt;&gt;</code> とほぼ同じです．</li>
<li><code>a &gt;&gt; b</code> は <code>a</code> にマッチして <code>b</code> にマッチしなかった場合，<code>a</code> の前にバックトラックします.</li>
<li><code>a &gt; b</code> はその場合，即座にパース失敗を通知します．</li>
</ul>
</li>
<li><code>*</code>

<ul>
<li><code>*a</code> という形で使う</li>
<li><code>a</code> の 0 回以上の繰り返し</li>
<li>返り値は <code>std::vector&lt;a&gt;</code></li>
</ul>
</li>
<li><code>+</code>

<ul>
<li><code>+a</code> という形で使う</li>
<li><code>*</code> の一回以上繰り返し版</li>
</ul>
</li>
<li><code>-</code>

<ul>
<li><code>-a</code> という形で使う</li>
<li><code>a</code> が来ても来なくても良いというパーサ</li>
<li>返り値は <code>boost::optional&lt;a&gt;</code></li>
</ul>
</li>
</ul>

<p>...</p>

<p><code>&gt;</code> と <code>&gt;&gt;</code> の違いはわかりにくいですね．<br/>
<code>(x3::lit("(") &gt;&gt; ")") | ("(" &gt;&gt; x3::int_ &gt;&gt; ")")</code> に <code>"(123)"</code> を食わせると，<code>|</code> の後半部分にマッチしてくれます．
一方, <code>&gt;&gt;</code> を <code>&gt;</code> に置換えた場合，ソース先頭の<code>"("</code> が<code>x3::lit("(")</code> にマッチするにもかかわらず，その直後に<code>")"</code> が来ていないため，その時点でエラーを通知してしまいます．</p>

<h2>一旦むすび</h2>

<p>とりあえず最も基本的な部分をまとめてみました．<br/>
ここまでは <code>Qi</code> と同じなんですよね．</p>

<p>セマンティックアクションや <code>on_error</code> などの扱いががっつり変わっているようなので，一旦ここで切って，それぞれ調べてからまとめたいと思います．
何か間違い等あればぜひご指摘お願いします．</p>

---

---
