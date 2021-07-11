---
title: "C++ でパーサコンビネータを書きました"
date: 2016-04-29T15:30:09.000Z
tags: []
---

<p><a class="keyword" href="http://d.hatena.ne.jp/keyword/C%2B%2B">C++</a> で<a class="keyword" href="http://d.hatena.ne.jp/keyword/%B9%BD%CA%B8%B2%F2%C0%CF">構文解析</a>といえば，Boost.Spirit や <a class="keyword" href="http://d.hatena.ne.jp/keyword/yacc">yacc</a>系などが有名ですが，どうにも使うの辛かったので作りました．</p>

<h3>2016/05/01 追記　</h3>

<p>いろいろ更新しました．肯定先読み以外はプリミティブも実装し終わっているかと思います．<br/>
ドキュメントはまだ無いのですが，すべての機能についてテストは書いてあるので，それを見てもらえればなんとか使い方もわかるかと思います．</p>

<p><a href="https://github.com/agatan/coco">agatan/coco</a></p>

<p><code>coco::combix</code> がパーサ<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D3%A5%CD%A1%BC%A5%BF">コンビネータ</a>ライブラリの namespace です．</p>

<p>Boost.Spirit は高機能かつ高性能なんですが，かなり変態的な構文で記述する必要があり(まぁ <a class="keyword" href="http://d.hatena.ne.jp/keyword/C%2B%2B">C++</a> なんですけど)，さらにその性能や便利さ，構文のために異常なまでにテンプレートを多用しています．私は<a class="keyword" href="http://d.hatena.ne.jp/keyword/%B9%BD%CA%B8%B2%F2%C0%CF">構文解析</a>後の<a class="keyword" href="http://d.hatena.ne.jp/keyword/%B9%BD%CA%B8%CC%DA">構文木</a>の構築に Boost.Variant を使ってみているのですが，Boost.Spirit と Boost.Variant の両面から，<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B8%A5%A7%A5%CD%A5%EA%A5%C3%A5%AF">ジェネリック</a>すぎるがゆえの<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D1%A5%A4%A5%EB">コンパイル</a>エラー爆発攻撃を食らって本当に辛いです．</p>

<p>そこで，<a class="keyword" href="http://d.hatena.ne.jp/keyword/Haskell">Haskell</a> の <a href="https://hackage.haskell.org/package/parsec">parsec</a> や Rust の <a href="https://github.com/Marwes/combine">combine</a> を参考にしつつ，<a class="keyword" href="http://d.hatena.ne.jp/keyword/C%2B%2B">C++</a> でパーサ<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D3%A5%CD%A1%BC%A5%BF">コンビネータ</a>を書いてみました．(実際これを使っても<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D1%A5%A4%A5%EB">コンパイル</a>エラーは割りと発狂しますが)</p>

<h2>例</h2>

<p>例となるコードは <a href="https://github.com/agatan/coco-combix-demo">agatan/coco-combix-demo</a> においてあります．<br/>
ドキュメントもないので，なんとなく雰囲気だけコードから読み取る必要があります．(例に出ていない機能もちょいちょい実装されてしまっています．)</p>

<p>以下にちょっと簡略版のコードを載せてみます．ありがちな電卓です．AST を作らず直接計算しています．</p>

<pre class="code lang-cpp" data-lang="cpp" data-unlink><span class="synPreProc">#include </span><span class="synConstant">&lt;string&gt;</span>
<span class="synPreProc">#include </span><span class="synConstant">&lt;iostream&gt;</span>
<span class="synPreProc">#include </span><span class="synConstant">&lt;functional&gt;</span>

<span class="synPreProc">#include </span><span class="synConstant">&lt;coco/combix.hpp&gt;</span>

<span class="synType">namespace</span> cbx = coco::combix;

<span class="synStatement">using</span> stream_type = cbx::iterator_stream&lt;std::string::const_iterator&gt;;

cbx::parser&lt;<span class="synType">int</span>, stream_type&gt; expression();

cbx::parser&lt;<span class="synType">int</span>, stream_type&gt; number() {
  <span class="synStatement">return</span> cbx::expected(cbx::map(cbx::many1(cbx::digit()),
                                [](<span class="synType">auto</span>&amp;&amp; is) <span class="synError">{</span>
                                  <span class="synType">int</span> acc = <span class="synConstant">0</span>;
                                  <span class="synStatement">for</span> (<span class="synType">auto</span> i : is) <span class="synError">{</span>
                                    acc = acc * <span class="synConstant">10</span> + i;
                                  <span class="synError">}</span>
                                  <span class="synStatement">return</span> acc;
                                }<span class="synError">)</span>,
                       <span class="synConstant">&quot;integer number&quot;</span><span class="synError">)</span>;
}

cbx::parser&lt;<span class="synType">int</span>, stream_type&gt; factor() {
  <span class="synStatement">return</span> cbx::choice(
      number(),
      cbx::between(cbx::skip(cbx::token(<span class="synConstant">'('</span>), cbx::spaces()),
                   cbx::skip(cbx::token(<span class="synConstant">')'</span>), cbx::spaces()),
                   cbx::skip(cbx::lazy_fun(expression), cbx::spaces())));
}

cbx::parser&lt;<span class="synType">int</span>, stream_type&gt; term() {
  <span class="synType">auto</span> op = cbx::map(
      cbx::skip(cbx::choice(cbx::token(<span class="synConstant">'*'</span>), cbx::token(<span class="synConstant">'/'</span>)), cbx::spaces()),
      [](<span class="synType">auto</span> c) -&gt; std::function&lt;<span class="synType">int</span>(<span class="synType">int</span>, <span class="synType">int</span>)&gt; <span class="synError">{</span>
        <span class="synStatement">if</span> (c == <span class="synConstant">'*'</span>) <span class="synError">{</span>
          <span class="synStatement">return</span> std::multiplies&lt;<span class="synType">int</span>&gt;();
        <span class="synError">}</span> <span class="synStatement">else</span> <span class="synError">{</span>
          <span class="synStatement">return</span> std::divides&lt;<span class="synType">int</span>&gt;();
        <span class="synError">}</span>
      }<span class="synError">)</span>;
  <span class="synStatement">return</span> cbx::chainl1(cbx::skip(factor(), cbx::spaces()), op);
}

cbx::parser&lt;<span class="synType">int</span>, stream_type&gt; expression() {
  <span class="synType">auto</span> op = cbx::map(
      cbx::skip(cbx::choice(cbx::token(<span class="synConstant">'+'</span>), cbx::token(<span class="synConstant">'-'</span>)), cbx::spaces()),
      [](<span class="synType">auto</span> c) -&gt; std::function&lt;<span class="synType">int</span>(<span class="synType">int</span>, <span class="synType">int</span>)&gt; <span class="synError">{</span>
        <span class="synStatement">if</span> (c == <span class="synConstant">'+'</span>) <span class="synError">{</span>
          <span class="synStatement">return</span> std::plus&lt;<span class="synType">int</span>&gt;();
        <span class="synError">}</span> <span class="synStatement">else</span> <span class="synError">{</span>
          <span class="synStatement">return</span> std::minus&lt;<span class="synType">int</span>&gt;();
        <span class="synError">}</span>
      }<span class="synError">)</span>;
  <span class="synStatement">return</span> cbx::chainl1(cbx::skip(term(), cbx::spaces()), op);
}

<span class="synType">int</span> main() {
  std::string src;
  std::getline(std::cin, src);
  <span class="synType">auto</span> n = number();
  <span class="synType">auto</span> stream = cbx::range_stream(src);
  <span class="synType">auto</span> <span class="synType">const</span> parser = expression();
  <span class="synStatement">if</span> (<span class="synType">auto</span> res = cbx::parse(parser, stream)) {
    std::cout &lt;&lt; res.unwrap() &lt;&lt; std::endl;
  } <span class="synStatement">else</span> {
    std::cout &lt;&lt; cbx::to_string(res.unwrap_error()) &lt;&lt; std::endl;
  }
}
</pre>

<h2>特徴</h2>

<p>parsec を知っている方であれば読めるはずです...<br/>
特徴としては，多くのパーサは入力ストリームの型に依存せずに作れるようになっていることです．例えば，あらゆる入力一つを受け付け消費する <code>any</code> というパーサは，入力が <code>char</code> のストリームであろうと <code>int</code> のストリームであろうとパースを実行できるようになっています．<br/>
本来はエラーメッセージの爆発や読みづらさを防ぐために，すべてのパーサ自体にストリームの型をひも付けたかったのですが，そうすると，<code>any</code> を使うたびに，<code>any&lt;cbx::iterator_stream&lt;typename std::vector&lt;int&gt;::const_iterator&gt;&gt;()</code> とか <code>any&lt;cbx::iterator_stream&lt;std::string::const_iterator&gt;&gt;()</code> とかしなくてはなりません．これは <a class="keyword" href="http://d.hatena.ne.jp/keyword/Haskell">Haskell</a> や Rust と違って <a class="keyword" href="http://d.hatena.ne.jp/keyword/C%2B%2B">C++</a> の<a class="keyword" href="http://d.hatena.ne.jp/keyword/%B7%BF%BF%E4%CF%C0">型推論</a>が限定的であるためです．(<a class="keyword" href="http://d.hatena.ne.jp/keyword/Haskell">Haskell</a> や Rust では後でその値がどう使われているかも推論の根拠として使われます．)<br/>
そこで，パーサ自体には入力ストリームの型を指定させずに，実際にパースする部分で初めて入力ストリームの型を検査することにしました．</p>

<p>で，<code>cbx::parser&lt;int, stream_type&gt;</code> はパーサを type erasure を使ってラップします．普通に使っていると簡単に <code>cbx::expected&lt;cbx::map_parser&lt;cbx::many1_parser&lt;cbx::digit_parser&gt;, (lambda at ...)&gt;&gt;</code> 型とかが出てきます(<code>cbx::expected(cbx::map(cbx::many1(cbx::digit()), [](auto&amp;&amp;) {...}), "integer number")</code> の型です)<br/>
これを関数定義のたびに書くとか発狂してしまうので，type erasure を使って型をラップし短絡します．<br/>
ただしパフォーマンスの観点から行くとおそらく型をラップするために仮想関数を使ってしまうので，インライン展開等がきかなくなると思われます．まぁ仕方ないです．<br/>
ただ，型を膨らませすぎずに適度にラップしてやると，<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D1%A5%A4%A5%EB">コンパイル</a>エラーの内容がかなり読みやすくなるはずです．なのでなんかわからんけどエラーになるっていうときは細かくパーサを分割してラップしてやると良いかもしれません．</p>

<h2>まとめ</h2>

<p>あまりにもドキュメントやコメント書かなすぎてひどいですが，ちょっと<a class="keyword" href="http://d.hatena.ne.jp/keyword/%B9%BD%CA%B8%B2%F2%C0%CF">構文解析</a>したいとかっていうときに便利だと思います．<br/>
Boost.Spirit と違って普通に <a class="keyword" href="http://d.hatena.ne.jp/keyword/C%2B%2B">C++</a> のプログラムとして書けます．(Boost.Spirit も <a class="keyword" href="http://d.hatena.ne.jp/keyword/C%2B%2B">C++</a> プログラムとして書けてはいるんですが，なんかあれはあれで別の言語を覚えているような気分になってしまったので...)</p>

<p>あと PEG のプリミティブをまだ完全に実装していないと思います．先読みや否定先読みが出来ません．(実装します…)</p>

---

---
