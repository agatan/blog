---
title: "C++ : なぜテンプレートが必要なのか"
date: 2016-05-30T14:46:47.000Z
tags: ["C++"]
---

<p>こんにちは。<br/>
ちょっと <a class="keyword" href="http://d.hatena.ne.jp/keyword/C%2B%2B">C++</a> への熱を冷まさないために、<a class="keyword" href="http://d.hatena.ne.jp/keyword/C%2B%2B">C++</a> のテンプレートについてまとめてみたいと思います。</p>

<h2>対象</h2>

<ul>
<li><a class="keyword" href="http://d.hatena.ne.jp/keyword/C%2B%2B">C++</a> のテンプレートが怖い人</li>
<li><a class="keyword" href="http://d.hatena.ne.jp/keyword/C%2B%2B">C++</a> の<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D1%A5%A4%A5%EB">コンパイル</a>エラーメッセージが怖い人</li>
<li><a class="keyword" href="http://d.hatena.ne.jp/keyword/C%2B%2B">C++</a> の規格とブログポストを比較して誤りを探したい人(もし誤っていたら教えて下さい...)</li>
</ul>

<h2>テンプレートとは</h2>

<blockquote><p>プログラミングにおけるテンプレートは、静的型付けの<a class="keyword" href="http://d.hatena.ne.jp/keyword/C%2B%2B">C++</a>でデータ型にとらわれずにコードを書くことを可能にする機能であり、<a class="keyword" href="http://d.hatena.ne.jp/keyword/C%2B%2B">C++</a>においては<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B8%A5%A7%A5%CD%A5%EA%A5%C3%A5%AF">ジェネリック</a>プログラミングに用いられる。<br/>
<a href="https://ja.wikipedia.org/wiki/%E3%83%86%E3%83%B3%E3%83%97%E3%83%AC%E3%83%BC%E3%83%88_(%E3%83%97%E3%83%AD%E3%82%B0%E3%83%A9%E3%83%9F%E3%83%B3%E3%82%B0">テンプレート (プログラミング) - Wikipedia</a>)より</p></blockquote>

<p>他の静的型付きな<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%D7%A5%ED%A5%B0%A5%E9%A5%DF%A5%F3%A5%B0%B8%C0%B8%EC">プログラミング言語</a>をすでに知っている場合は，すんなり入りやすいかもしれません。<br/>
<a class="keyword" href="http://d.hatena.ne.jp/keyword/Java">Java</a> や <a class="keyword" href="http://d.hatena.ne.jp/keyword/Scala">Scala</a>, <a class="keyword" href="http://d.hatena.ne.jp/keyword/C%23">C#</a> でいうところの<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B8%A5%A7%A5%CD%A5%EA%A5%AF%A5%B9">ジェネリクス</a>に近い存在です。<br/>
<a class="keyword" href="http://d.hatena.ne.jp/keyword/OCaml">OCaml</a> や <a class="keyword" href="http://d.hatena.ne.jp/keyword/Haskell">Haskell</a> だと多相とか。</p>

<p>雑に表現するならば、リストとか<a class="keyword" href="http://d.hatena.ne.jp/keyword/%CF%A2%C1%DB%C7%DB%CE%F3">連想配列</a>のように内部のデータ型に依らないデータ構造を、静的型のもとにどうやったらうまく表現できるかな、に対する解の一つです。</p>

<h3>例</h3>

<p>では一つの例として、スタックというデータ構造をプログラムに落としこむことを考えます。<br/>
まずは <code>int</code> 型のスタックを定義してみます。</p>

<pre class="code lang-cpp" data-lang="cpp" data-unlink><span class="synPreProc">#define MAX_ELEM </span><span class="synConstant">10</span>

<span class="synType">class</span> int_stack {
<span class="synStatement">public</span>:
  int_stack() : data(), n() {}

  <span class="synType">void</span> push(<span class="synType">int</span> x) {
    <span class="synStatement">if</span> (n &gt;= MAX_ELEM) {
      <span class="synStatement">throw</span> <span class="synConstant">&quot;stack is full!!&quot;</span>;
    }
    data[n++] = x;
  }

  <span class="synType">int</span> pop() {
    <span class="synStatement">if</span> (n &lt; <span class="synConstant">0</span>) {
      <span class="synStatement">throw</span> <span class="synConstant">&quot;stack is empty!!&quot;</span>;
    }
    <span class="synStatement">return</span> data[--n];
  }
<span class="synStatement">private</span>:
  <span class="synType">int</span> data[MAX_ELEM];
  <span class="synType">int</span> n;
};
</pre>

<p>簡単のため、かなりお粗末なスタックですが、最低限のスタックとしての見た目はしていると思います。</p>

<p>では次に、<code>std::string</code> 型のスタックや <code>double</code> 型のスタックを作りたいとなったらどうすればよいでしょうか。<br/>
コピーして <code>int</code> を置換しますか？あまり褒められた方法ではなさそうです。</p>

<h4>C でのアプローチの一つ</h4>

<p>C 言語の場合、このような問題に対しては <code>void*</code> というアプローチがあります。<br/>
<code>void*</code> は <a class="keyword" href="http://d.hatena.ne.jp/keyword/java">java</a> でいう <code>Object</code> のように扱われます。</p>

<pre class="code lang-c" data-lang="c" data-unlink><span class="synPreProc">#define MAX_ELEM </span><span class="synConstant">10</span>

<span class="synType">struct</span> stack {
  <span class="synType">void</span> *data[MAX_ELEM];
  <span class="synType">int</span> n;
};

<span class="synType">void</span> push(stack *s, <span class="synType">void</span> *elem) {
  ....
}

<span class="synType">void</span> *pop(stack *s) {
  ....
}

<span class="synComment">/* Usage */</span>
stack *s = new_stack();
<span class="synType">int</span> *x = (<span class="synType">int</span>*)malloc(<span class="synStatement">sizeof</span>(<span class="synType">int</span>));
*x = <span class="synConstant">1</span>;
push(s, (<span class="synType">void</span>*)x);
<span class="synType">int</span> *y = (<span class="synType">int</span>*)pop(s);
printf(<span class="synConstant">&quot;</span><span class="synSpecial">%d\n</span><span class="synConstant">&quot;</span> *y); <span class="synComment">/* =&gt; 1 */</span>
</pre>

<p>こんな感じでしょうか。実装の細かい部分は省略しています。<br/>
<code>push</code> の際にはあらゆるポインタを <code>void*</code> にキャストし、逆に <code>pop</code> する際には <code>void*</code> を求める型にキャストしています。<br/>
<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B8%A5%A7%A5%CD%A5%EA%A5%AF%A5%B9">ジェネリクス</a>のなかった頃の <a class="keyword" href="http://d.hatena.ne.jp/keyword/java">java</a> は、これを <code>Object</code> へのキャスト・<code>Object</code> からのキャストとして表現していました。</p>

<h5>void* のデメリット</h5>

<p><code>void*</code> を使う場合のデメリットは、型システムを台無しにしている点です。(<code>malloc</code> や <code>free</code> が必要であることは C 言語特有の問題なのでスルー)<br/>
つまり、<code>int</code> のスタックから <code>pop</code> してきたとき、<code>int*</code> に正しくキャストを行う責任は<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%D7%A5%ED%A5%B0%A5%E9%A5%DE">プログラマ</a>にあり、<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D1%A5%A4%A5%E9">コンパイラ</a>は何も手助けをしてくれないということです。<br/>
したがって、 <code>int</code> スタックに <code>double</code> の値を <code>push</code> したり、 <code>double</code> スタックから <code>char*</code> を <code>pop</code> したりというミスが簡単に引き起こされてしまうということです。</p>

<h4>そこでテンプレート</h4>

<p>では <a class="keyword" href="http://d.hatena.ne.jp/keyword/C%2B%2B">C++</a> ではどのようなアプローチを取るかというと、テンプレートを使います。<br/>
今回は型に関するテンプレートの話しかしないので、<a class="keyword" href="http://d.hatena.ne.jp/keyword/java">java</a> の<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B8%A5%A7%A5%CD%A5%EA%A5%AF%A5%B9">ジェネリクス</a>も大体同じ話だと思って構わないと思います。(実行時の表現や<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D1%A5%A4%A5%E9">コンパイラ</a>の動きなどの違いはあるが、対象としている問題は同じ)</p>

<p>さきほどの <code>int_stack</code> の実装では、要素型が <code>int</code> に固定化されてしまっているのが問題でした。<br/>
そこで、テンプレートでは、型を抽象化し、ある種の引数のように扱っています。</p>

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

<p>先頭の <code>template &lt;typename T&gt;</code> (<code>template &lt;class T&gt;</code> でも可)は、型引数の導入の役割を果たしています。<br/>
<code>stack</code> クラスの定義内に登場する <code>T</code> は型引数として導入された型を表します。</p>

<p>利用する際には、<code>stack&lt;int&gt;</code> とか <code>stack&lt;std::string&gt;</code> とか、型を <code>stack</code> に渡してあげればOKです。</p>

<pre class="code lang-cpp" data-lang="cpp" data-unlink>stack&lt;<span class="synType">int</span>&gt; int_stack;
int_stack.push(<span class="synConstant">1</span>);
int_stack.push(<span class="synConstant">2</span>);
<span class="synType">int</span> x = int_stack.pop();
int_stack.push(<span class="synConstant">&quot;abc&quot;</span>); <span class="synComment">// =&gt; Compile error!</span>

stack&lt;std::string&gt; str_stack;
str_stack.push(<span class="synConstant">&quot;abc&quot;</span>);
str_stack.push(<span class="synConstant">1</span>); <span class="synComment">// =&gt; Compile error!</span>
</pre>

<p>このように、同じコードをコピペすることなく、複数の型に対応したスタックという汎用的なデータ構造を表現することが出来ています。<br/>
さらに、この方法では、<code>void*</code> や <code>Object</code> と異なり、型的に誤った使用方法をしようとすると<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D1%A5%A4%A5%EB">コンパイル</a>エラーになるというメリットがあります。<br/>
ランタイムエラーより<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D1%A5%A4%A5%EB">コンパイル</a>エラーのほうが<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%C7%A5%D0%A5%C3%A5%B0">デバッグ</a>しやすいし発見しやすいですよね。</p>

<h2>一旦まとめ</h2>

<p>というわけで今回はテンプレートがなぜ便利かという話のほんのさわりの部分について書いてみました。<br/>
次はテンプレートや<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B8%A5%A7%A5%CD%A5%EA%A5%AF%A5%B9">ジェネリクス</a>の実現方法、ランタイムにおける表現方法などについて書いてみます。<br/>
そこからはテンプレート引数として値をとる話や、TMP についても触れていければと思っています。</p>

---

---
