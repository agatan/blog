---
title: "C++テンプレートイディオム CRTP"
date: 2016-06-16T10:27:08.000Z
tags: []
---

<p><a class="keyword" href="http://d.hatena.ne.jp/keyword/C%2B%2B">C++</a>テンプレートの有名なイディオムとして、CRTPというものがあります。
今回はそれについて。
複雑な部分特殊化みたいな話もないですし、メリットもわかりやすい良いイディオムだと思うので、ちょっとまとめておきます。
(Control キーのことをよく CTRL と書くので、CTRP とタイポしがち）</p>

<p>詳細はこちらを参照してください。
<a href="https://ja.wikibooks.org/wiki/More_C%2B%2B_Idioms/%E5%A5%87%E5%A6%99%E3%81%AB%E5%86%8D%E5%B8%B0%E3%81%97%E3%81%9F%E3%83%86%E3%83%B3%E3%83%97%E3%83%AC%E3%83%BC%E3%83%88%E3%83%91%E3%82%BF%E3%83%BC%E3%83%B3(Curiously_Recurring_Template_Pattern">More C++ Idioms/奇妙に再帰したテンプレートパターン(Curiously Recurring Template Pattern) - Wikibooks</a>)</p>

<h2>CRTPの利点</h2>

<p>細かい実装の話の前に、CRTPを使うと何がうれしいのかを簡単に。</p>

<p><strong>静的 Template Method パターンの実現</strong>_ 。これがCRTPの利点です。</p>

<p>Template Method パターンについてはここでは説明しませんが、大枠の<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%A2%A5%EB%A5%B4%A5%EA%A5%BA%A5%E0">アルゴリズム</a>を共有しつつその内部で使用する実装の詳細をクラスごとに切り替えるといった目的に使われる<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%C7%A5%B6%A5%A4%A5%F3%A5%D1%A5%BF%A1%BC%A5%F3">デザインパターン</a>です。</p>

<p>通常 Template Method パターンを <a class="keyword" href="http://d.hatena.ne.jp/keyword/C%2B%2B">C++</a> で実現しようと思うとどうしても仮想関数を使うことになると思います。これによって実行時のオーバヘッドがかかってしまいます。
Template Method パターンは、いわゆるクラスベースの動的な<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%DD%A5%EA%A5%E2%A1%BC%A5%D5%A5%A3%A5%BA%A5%E0">ポリモーフィズム</a>を必要としないクラスであっても、<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%A2%A5%EB%A5%B4%A5%EA%A5%BA%A5%E0">アルゴリズム</a>の共有やボイラープレートコードの削減に非常に有用なパターンです。
これを静的に実現するのが CRTP の目的なのです。</p>

<h2>実装</h2>

<p>CRTP とは、その名の通り、奇妙に<a class="keyword" href="http://d.hatena.ne.jp/keyword/%BA%C6%B5%A2">再帰</a>したテンプレートのパターンのことです。
情報量０の文章ですね。実際のコードも見たほうがわかりやすいと思います。</p>

<p>以下に、<code>compare</code> というメソッドから比較<a class="keyword" href="http://d.hatena.ne.jp/keyword/%B1%E9%BB%BB%BB%D2">演算子</a>を <em>derive</em> (自動導出) する例をのせます。</p>

<pre class="code lang-cpp" data-lang="cpp" data-unlink><span class="synPreProc">#include </span><span class="synConstant">&lt;iostream&gt;</span>

<span class="synType">template</span> &lt;<span class="synType">typename</span> T&gt;
<span class="synType">struct</span> comparable {
  <span class="synType">template</span> &lt;<span class="synType">typename</span> U&gt;
  <span class="synStatement">friend</span> <span class="synType">bool</span> <span class="synStatement">operator</span>==(comparable <span class="synType">const</span>&amp; lhs, U <span class="synType">const</span>&amp; rhs) {
    <span class="synStatement">return</span> <span class="synStatement">static_cast</span>&lt;T <span class="synType">const</span>&amp;&gt;(lhs).compare(rhs) == <span class="synConstant">0</span>;
  }

  <span class="synType">template</span> &lt;<span class="synType">typename</span> U&gt;
  <span class="synStatement">friend</span> <span class="synType">bool</span> <span class="synStatement">operator</span>&gt;(comparable <span class="synType">const</span>&amp; lhs, U <span class="synType">const</span>&amp; rhs) {
    <span class="synStatement">return</span> <span class="synStatement">static_cast</span>&lt;T <span class="synType">const</span>&amp;&gt;(lhs).compare(rhs) &gt; <span class="synConstant">0</span>;
  }

  <span class="synType">template</span> &lt;<span class="synType">typename</span> U&gt;
  <span class="synStatement">friend</span> <span class="synType">bool</span> <span class="synStatement">operator</span>&lt;(comparable <span class="synType">const</span>&amp; lhs, U <span class="synType">const</span>&amp; rhs) {
    <span class="synStatement">return</span> <span class="synStatement">static_cast</span>&lt;T <span class="synType">const</span>&amp;&gt;(lhs).compare(rhs) &lt; <span class="synConstant">0</span>;
  }
};

<span class="synType">struct</span> person : comparable&lt;person&gt; {
  <span class="synType">int</span> age;

  <span class="synType">int</span> compare(person <span class="synType">const</span>&amp; rhs) <span class="synType">const</span> {
    <span class="synStatement">return</span> age - rhs.age;
  }
};

<span class="synType">int</span> main() {
  person p1, p2;
  p1.age = <span class="synConstant">10</span>;
  p2.age = <span class="synConstant">100</span>;

  std::cout &lt;&lt; std::boolalpha &lt;&lt; (p1 == p2) &lt;&lt; std::endl;
  std::cout &lt;&lt; std::boolalpha &lt;&lt; (p1 &lt; p2) &lt;&lt; std::endl;
  std::cout &lt;&lt; std::boolalpha &lt;&lt; (p1 &gt; p2) &lt;&lt; std::endl;
}
</pre>

<p><code>person</code> クラスには <code>operator==</code> など定義していないにもかかわらず、<code>person</code> を比較することが出来ています。</p>

<p>CRTPの中心となるのは <code>struct person : comparable&lt;person&gt;</code> という部分です。
クラスを定義する際に、自分をテンプレート引数にとるクラスを継承するというコード、これこそが「奇妙な<a class="keyword" href="http://d.hatena.ne.jp/keyword/%BA%C6%B5%A2">再帰</a>」なのです。
実際、<code>struct person : comparable&lt;person&gt;</code> の部分ではまだ <code>person</code> がどんな実装になるかはわかっていません。奇妙ですね。</p>

<p>さて、まずは <code>person</code> の中身を見てみます。
<code>person</code> では、<code>person const&amp;</code> を引数にとり、それが自分より大きければ正の値を、小さければ負の値を、等しければ0を返すような、<code>compare</code> というメソッドを定義しています。
<code>person</code> の仕事はこれだけです。</p>

<p><code>comparable</code> は、テンプレート引数にひとつの型をとります。<br/>
<code>comparable</code> はその型が <code>compare</code> というメソッドをもつことを期待しています。(暗黙のインターフェース)<br/>
<code>comparable</code> の仕事は <code>compare</code> というひとつのメソッドから、<code>operator==</code>, <code>operator&lt;</code>, <code>operator&gt;</code> を自動的に導くことです。<br/>
Template Methodパターンをご存じの方ならすんなり理解できるかと思います。</p>

<p>CRTP のすごいところは、仮想関数をまったく使わないことです。つまり、実行時のテーブルルックアップは発生しません。すべてが静的に決定されるのです。</p>

<h2>おまけ</h2>

<p>先に挙げた <code>compare</code> から <code>operator==</code> を自動導出する例ですが、<a class="keyword" href="http://d.hatena.ne.jp/keyword/Haskell">Haskell</a> の <code>Ord</code> 型クラスを意識しています。</p>

<pre class="code lang-haskell" data-lang="haskell" data-unlink><span class="synType">class</span> Eq a <span class="synStatement">=&gt;</span> Ord a <span class="synType">where</span>
  compare <span class="synStatement">::</span> a <span class="synStatement">-&gt;</span> a <span class="synStatement">-&gt;</span> Ordering
  (<span class="synStatement">&lt;</span>), (<span class="synStatement">&lt;=</span>), (<span class="synStatement">&gt;</span>), (<span class="synStatement">&lt;=</span>) <span class="synStatement">::</span> a <span class="synStatement">-&gt;</span> a <span class="synStatement">-&gt;</span> Bool
  max, min <span class="synStatement">::</span> a <span class="synStatement">-&gt;</span> a <span class="synStatement">-&gt;</span> a
</pre>

<p><code>Ord</code> 型クラスの<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%A4%A5%F3%A5%B9%A5%BF%A5%F3%A5%B9">インスタンス</a>になるためには、最低でも <code>compare</code> を実装している必要があります。
逆にいえば、<code>compare</code> だけ実装すれば、他の関数は自動的に実装されます。</p>

<p><a class="keyword" href="http://d.hatena.ne.jp/keyword/Haskell">Haskell</a> では、<code>Ord</code> になるためには <code>Eq</code> の<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%A4%A5%F3%A5%B9%A5%BF%A5%F3%A5%B9">インスタンス</a>である必要があります。
これを <a class="keyword" href="http://d.hatena.ne.jp/keyword/C%2B%2B">C++</a> で表現するためには、</p>

<pre class="code lang-cpp" data-lang="cpp" data-unlink><span class="synType">template</span> &lt;<span class="synType">typename</span> T&gt;
<span class="synType">struct</span> ord : eq&lt;T&gt; {
  ...
};
</pre>

<p>こんな感じでしょうか。もちろん型クラスの代替にはなりえないんですけどね。
<a class="keyword" href="http://d.hatena.ne.jp/keyword/Haskell">Haskell</a> の型クラスの利点のひとつである、最小限のインターフェース実装による関数の自動導出っぽいこともできるよというお話でした。</p>

<p>実際にテンプレートライブラリを書いてみて改めて有用性がわかったテクニックでした。
拙作の coco にも導入したい... すべてのパーサにユーティリティ<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%E1%A5%F3%A5%D0%B4%D8%BF%F4">メンバ関数</a>を追加するみたいなことが出来るはず... いつかやります。</p>

---

---
