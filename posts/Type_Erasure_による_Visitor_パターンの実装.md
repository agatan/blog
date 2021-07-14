---
title: "Type Erasure による Visitor パターンの実装"
date: 2016-01-25T11:07:38.000Z
tags: ["C++"]
---

<p>プログラミングしていて，<a class="keyword" href="http://d.hatena.ne.jp/keyword/%CC%DA%B9%BD%C2%A4">木構造</a>をうまく扱いたいという状況は結構良くあると思います．<br/>
代数的データ型とパターンマッチを持つ言語であればとても美しく完結に表現できる<a class="keyword" href="http://d.hatena.ne.jp/keyword/%CC%DA%B9%BD%C2%A4">木構造</a>ですが，<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%AA%A5%D6%A5%B8%A5%A7%A5%AF%A5%C8%BB%D8%B8%FE%B8%C0%B8%EC">オブジェクト指向言語</a>でやろうと思うと結構たいへんです．<br/>
典型的には Visitor パターンというやつを用います．<a href="http://qiita.com/lyrical_logical/items/bc6126f34a571a2c4f97">デザインパターン - Visitor パターン再考 - Qiita</a>が非常にわかりやすく，理解の助けになりました．ありがとうございます．</p>

<p>一方，<a class="keyword" href="http://d.hatena.ne.jp/keyword/C%2B%2B">C++</a> の有名なライブラリ，Boost には Boost.Variant というモジュールがあり，これまたとても美しく Visitor っぽいことが出来ます．</p>

<pre class="code lang-cpp" data-lang="cpp" data-unlink><span class="synPreProc">#include </span><span class="synConstant">&lt;boost/variant.hpp&gt;</span>
<span class="synPreProc">#include </span><span class="synConstant">&lt;string&gt;</span>
<span class="synPreProc">#include </span><span class="synConstant">&lt;iostream&gt;</span>

<span class="synStatement">using</span> sample = boost::variant&lt;<span class="synType">int</span>, <span class="synType">double</span>, std::string&gt;;

sample s1 = <span class="synConstant">1</span>;
sample s2 = <span class="synConstant">2.0</span>;
sample s3 = <span class="synConstant">&quot;sample3&quot;</span>;

boost::apply_visitor([](<span class="synType">auto</span> <span class="synType">const</span>&amp; v) <span class="synError">{</span> std::cout &lt;&lt; v &lt;&lt; std::endl; }, s1); <span class="synComment">// =&gt; 1</span>
boost::apply_visitor([](<span class="synType">auto</span> <span class="synType">const</span>&amp; v) <span class="synError">{</span> std::cout &lt;&lt; v &lt;&lt; std::endl; }, s2); <span class="synComment">// =&gt; 2.0</span>
boost::apply_visitor([](<span class="synType">auto</span> <span class="synType">const</span>&amp; v) <span class="synError">{</span> std::cout &lt;&lt; v &lt;&lt; std::endl; }, s3); <span class="synComment">// =&gt; sample3</span>
</pre>

<p>しかし，Boost.Variant は非常に高機能ですが，テンプレートをガンガン使っていたりするので，<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D1%A5%A4%A5%EB">コンパイル</a>コストが大きいという問題があります．</p>

<p>そこで，Type Erasure を使って visitor パターンをうまく表せれば，<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D1%A5%A4%A5%EB">コンパイル</a>コストを下げられるのでは？というお話です．<br/>
Type Erasure は「型消去」とかで<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B0%A5%B0%A4%EB">ググる</a>と色々解説してくださっている記事などが出てくると思います．（ありがとうございます）</p>

<p>この話，私が考えたわけではなくて，どこかの<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%BD%A1%BC%A5%B9%A5%B3%A1%BC%A5%C9">ソースコード</a>で見たようなきがするんですが，当時は Type Erasure とか意味不明だったのでスルーしていました．<br/>
今ならなんとなくやりたいことは出来るような気がするので（＆ちょうど必要になったので）記事にしてみていますが，もしオリジナルっぽいものや同じようなことを提案している<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%BD%A1%BC%A5%B9%A5%B3%A1%BC%A5%C9">ソースコード</a>・記事を見かけた方は是非ご連絡いただけると嬉しいです．</p>

<h2>1st step</h2>

<h3>Visitor</h3>

<p>今回表現したいデータ構造をまず定めます．簡単のために，足し算・掛け算・整数定数の 3 種類のノードを持つ<a class="keyword" href="http://d.hatena.ne.jp/keyword/%CC%DA%B9%BD%C2%A4">木構造</a>を考えます．<br/>
<code>(1 + 2) * 3</code> なら， <code>mul( add(1, 2), 3 )</code> みたいな感じです．</p>

<p>この構造を visit する Visitor クラスから先に考えます．<br/>
Visitor クラスは，<code>visit</code> という<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%E1%A5%F3%A5%D0%B4%D8%BF%F4">メンバ関数</a>をもつ型の値を，型を消去して保持させます．</p>

<pre class="code lang-cpp" data-lang="cpp" data-unlink><span class="synType">class</span> visitor {
<span class="synStatement">private</span>:
  <span class="synType">class</span> visitor_base_holder {
  <span class="synStatement">public</span>:
    <span class="synType">virtual</span> <span class="synType">void</span> visit(add &amp;) = <span class="synConstant">0</span>;
    <span class="synType">virtual</span> <span class="synType">void</span> visit(mul &amp;) = <span class="synConstant">0</span>;
    <span class="synType">virtual</span> <span class="synType">void</span> visit(constant &amp;) = <span class="synConstant">0</span>;

    <span class="synType">virtual</span> ~visitor_base_holder() = <span class="synStatement">default</span>;
  };

  <span class="synType">template</span> &lt;<span class="synType">typename</span> V&gt; <span class="synType">class</span> visitor_holder : <span class="synStatement">public</span> visitor_base_holder {
  <span class="synStatement">private</span>:
    V &amp;v;

  <span class="synStatement">public</span>:
    visitor_holder(V &amp;v) : v(v) {}

    <span class="synType">void</span> visit(add &amp;a) override { v(a); }
    <span class="synType">void</span> visit(mul &amp;a) override { v(a); }
    <span class="synType">void</span> visit(constant &amp;a) override { v(a); }

    <span class="synType">virtual</span> ~visitor_holder() = <span class="synStatement">default</span>;
  };

  std::unique_ptr&lt;visitor_base_holder&gt; holder;

<span class="synStatement">public</span>:
  <span class="synType">template</span> &lt;<span class="synType">typename</span> V&gt;
  visitor(V &amp;v)
      : holder(std::make_unique&lt;visitor_holder&lt;V&gt;&gt;(v)) {}

  <span class="synType">template</span> &lt;<span class="synType">typename</span> Visitable&gt; <span class="synType">void</span> visit(Visitable &amp;v) { holder-&gt;visit(v); }
};
</pre>

<p>今回は <code>const</code> 修飾についてすべて無視しています．( <code>const</code> を考慮するならば，各 <code>visit</code> について，visitor の <code>const</code> 性と node の <code>const</code> 性を考える必要があります．つまり 4 種類の<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%E1%A5%F3%A5%D0%B4%D8%BF%F4">メンバ関数</a>を定義しなければなりません．）<br/>
visit した対象となるそれぞれのデータについて<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%AA%A1%BC%A5%D0%A1%BC%A5%ED%A1%BC%A5%C9">オーバーロード</a>する形で <code>visit</code> を定義しています．<br/>
<code>visitor</code> の<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%B9%A5%C8%A5%E9%A5%AF%A5%BF">コンストラクタ</a>に，<code>operator()(add&amp;)</code>, <code>operator()(mul&amp;)</code>, <code>operator()(constant&amp;)</code> を全て持つオブジェクト（<a class="keyword" href="http://d.hatena.ne.jp/keyword/C%2B%2B">C++</a>14 の<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B8%A5%A7%A5%CD%A5%EA%A5%C3%A5%AF">ジェネリック</a>ラムダでもOK）を渡すことで，型消去された visitor が出来上がります．<br/>
<code>visitor</code> の<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%B9%A5%C8%A5%E9%A5%AF%A5%BF">コンストラクタ</a>にどんな型の値を渡しても，出来上がる <code>visitor</code> にはその型情報は含まれないので，様々な visitor を統一して扱う（ <code>vector</code> に突っ込むとか）事ができるようになります．</p>

<h3>Node</h3>

<p>次にノードの方について考えます． 通常，Visitor パターンでは， visit される側のクラスに <code>accept</code> を実装します．<br/>
visit される側のデータを統一的に扱う（ <code>vector</code> に突っ込むとか）ためには，継承やインターフェースを用いるのが普通です．<br/>
<a class="keyword" href="http://d.hatena.ne.jp/keyword/C%2B%2B">C++</a> では，Visitor 側に使った Type Erasure のテクニックが使えます．<br/>
<code>std::vector&lt;node&gt;</code> などのように，統一的にノードを扱いつつも，visit される際には，<code>visit(add&amp;)</code> や <code>visit(mul&amp;)</code> のような適切な<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%AA%A1%BC%A5%D0%A1%BC%A5%ED%A1%BC%A5%C9">オーバーロード</a>関数を呼び出すようにしてやればオッケーです．</p>

<pre class="code lang-cpp" data-lang="cpp" data-unlink><span class="synType">class</span> node {
<span class="synStatement">private</span>:
  <span class="synType">class</span> node_base_holder {
  <span class="synStatement">public</span>:
    <span class="synType">virtual</span> <span class="synType">void</span> accept(visitor &amp;v) = <span class="synConstant">0</span>;

    <span class="synType">virtual</span> ~node_base_holder() = <span class="synStatement">default</span>;
  };

  <span class="synType">template</span> &lt;<span class="synType">typename</span> T&gt; <span class="synType">class</span> node_holder : <span class="synStatement">public</span> node_base_holder {
  <span class="synStatement">public</span>:
    node_holder(T <span class="synType">const</span> &amp;n) : node(n) {}
    node_holder(T &amp;&amp;n) : node(n) {}

    <span class="synType">void</span> accept(visitor &amp;v) override { v.visit(node); }

    ~node_holder() = <span class="synStatement">default</span>;

  <span class="synStatement">private</span>:
    T node;
  };

  std::shared_ptr&lt;node_base_holder&gt; holder;

<span class="synStatement">public</span>:
  <span class="synType">template</span> &lt;<span class="synType">typename</span> Node&gt;
  node(Node <span class="synType">const</span> &amp;n)
      : holder(std::make_shared&lt;node_holder&lt;Node&gt;&gt;(n)) {}

  <span class="synType">template</span> &lt;<span class="synType">typename</span> Node&gt;
  node(Node &amp;&amp;n)
      : holder(std::make_shared&lt;node_holder&lt;Node&gt;&gt;(n)) {}

  <span class="synType">void</span> accept(visitor &amp;v) { holder-&gt;accept(v); }

  <span class="synType">template</span> &lt;<span class="synType">typename</span> Visitor&gt; <span class="synType">void</span> accept(Visitor &amp;v) {
    visitor visit(v);
    holder-&gt;accept(visit);
  }
};
</pre>

<p>これ結構わかりにくと思うのですが，自分でも<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D1%A5%A4%A5%E9">コンパイラ</a>に怒られながら書いたのでいまいちよく分かってません．<br/>
先ほどの <code>visitor</code> の場合と異なり，<code>node</code> には特別満たすべきインターフェースは有りません．<br/>
Type Erasure を使う理由は，適切な <code>visit</code> 関数へのディスパッチのためです．</p>

<h3>使う</h3>

<p><code>visitor</code> と <code>node</code> が出来たので，使ってみます．<br/>
その前にデータ構造を定義しておきます．</p>

<pre class="code lang-cpp" data-lang="cpp" data-unlink><span class="synType">struct</span> constant {
  <span class="synType">int</span> value;
};

<span class="synType">struct</span> add {
  node lhs;
  node rhs;
};

<span class="synType">struct</span> mul {
  node lhs;
  node rhs;
};
</pre>

<p><code>add</code> や <code>mul</code> のフィールドに，<code>node</code> が使用されている点が大事です．<br/>
<code>add.lhs</code> や <code>mul.rhs</code> には，<code>constant</code> が来るか <code>add</code> が来るか <code>mul</code> が来るか分かりません．<br/>
そこで，visit 可能な型なら何でもOKという意味で，<code>node</code> 型の値をフィールドとします．</p>

<pre class="code lang-cpp" data-lang="cpp" data-unlink>node n = mul{add{constant{<span class="synConstant">1</span>}, constant{<span class="synConstant">2</span>}}, constant{<span class="synConstant">3</span>}};
</pre>

<p>これで，<code>(1 + 2) * 3</code> が表現できています．
<code>add</code> や <code>constant</code> から <code>node</code> へと暗黙変換が行われていることに注意してください．</p>

<p>次に visitor を定義します．これは，<code>operator()</code> を<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%AA%A1%BC%A5%D0%A1%BC%A5%ED%A1%BC%A5%C9">オーバーロード</a>した関数オブジェクトです．<br/>
式を出力する <code>printer</code> と 式を計算する <code>calculator</code> を定義します．</p>

<pre class="code lang-cpp" data-lang="cpp" data-unlink><span class="synType">struct</span> printer {
  <span class="synType">void</span> <span class="synStatement">operator</span>()(add &amp;a) {
    std::cout &lt;&lt; <span class="synConstant">&quot;(&quot;</span>;
    a.lhs.accept(*<span class="synStatement">this</span>);
    std::cout &lt;&lt; <span class="synConstant">&quot;)&quot;</span>;
    std::cout &lt;&lt; <span class="synConstant">&quot;+&quot;</span>;
    std::cout &lt;&lt; <span class="synConstant">&quot;(&quot;</span>;
    a.rhs.accept(*<span class="synStatement">this</span>);
    std::cout &lt;&lt; <span class="synConstant">&quot;)&quot;</span>;
  }

  <span class="synType">void</span> <span class="synStatement">operator</span>()(mul &amp;a) {
    std::cout &lt;&lt; <span class="synConstant">&quot;(&quot;</span>;
    a.lhs.accept(*<span class="synStatement">this</span>);
    std::cout &lt;&lt; <span class="synConstant">&quot;)&quot;</span>;
    std::cout &lt;&lt; <span class="synConstant">&quot;*&quot;</span>;
    std::cout &lt;&lt; <span class="synConstant">&quot;(&quot;</span>;
    a.rhs.accept(*<span class="synStatement">this</span>);
    std::cout &lt;&lt; <span class="synConstant">&quot;)&quot;</span>;
  }

  <span class="synType">void</span> <span class="synStatement">operator</span>()(constant &amp;c) { std::cout &lt;&lt; c.value; }
};

<span class="synType">struct</span> calculator {
  <span class="synType">int</span> result;
  <span class="synType">void</span> <span class="synStatement">operator</span>()(add &amp;a) {
    calculator l, r;
    a.lhs.accept(l);
    a.rhs.accept(r);
    result = l.result + r.result;
  }

  <span class="synType">void</span> <span class="synStatement">operator</span>()(mul &amp;m) {
    calculator l, r;
    m.lhs.accept(l);
    m.rhs.accept(r);
    result = l.result * r.result;
  }

  <span class="synType">void</span> <span class="synStatement">operator</span>()(constant &amp;c) { result = c.value; }
};
</pre>

<p>こんな感じです．<br/>
<code>visit</code> や <code>accept</code> を <code>void</code> を返す関数として定義したので，<code>calculator</code> は自前のフィールドに結果を保持する必要があります．
(あとで改善します)</p>

<p>使い方は</p>

<pre class="code lang-cpp" data-lang="cpp" data-unlink>  node n = mul{add{constant{<span class="synConstant">1</span>}, constant{<span class="synConstant">2</span>}}, constant{<span class="synConstant">3</span>}};
  printer p;
  n.accept(p);
  calculator calc;
  n.accept(calc);
  std::cout &lt;&lt; std::endl;
  std::cout &lt;&lt; calc.result &lt;&lt; std::endl;
  <span class="synStatement">return</span> <span class="synConstant">0</span>;
</pre>

<p>です．</p>

<h1>まとめ</h1>

<p>この方法の利点としては，データの定義そのものに Visitor パターンのためのノイズが入らないことが挙げられます．<br/>
普通の Visitor パターンでは継承必須ですし．</p>

<p><code>const</code> つけてないせいで一時オブジェクトが使えないので <code>printer p;</code> という行が必要になってしまっています．これは<code>const</code>をがんばってつけるだけなのでまぁ問題有りません．<br/>
一方，<code>calculator</code> の方はダサいですね．値を返す visitor も定義できるようにしたい．<br/>
<code>visitor</code> の定義もツライです．<code>const</code> を考慮した場合，同じような内容の<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%E1%A5%F3%A5%D0%B4%D8%BF%F4">メンバ関数</a>を 4 回ずつ書く必要がある．</p>

<p>このへんの問題点は解決可能な気がするので出来たら後で記事にするつもりです．</p>

<p>難しすぎて普通の visitor パターンで良くね？感出てきた</p>

---

---
