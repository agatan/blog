---
title: "C++ のテンプレートの実装"
date: 2016-06-13T07:59:27.000Z
tags: ["C++"]
---

<p><a class="keyword" href="http://d.hatena.ne.jp/keyword/C%2B%2B">C++</a> のテンプレートがなぜ必要で，どんな構文・種類のものがあるかについては前回までにまとめました。<br/>
というわけで次は <a class="keyword" href="http://d.hatena.ne.jp/keyword/C%2B%2B">C++</a> ではテンプレートという機能を使用するとどんな<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%D0%A5%A4">バイ</a>ナリが生成されるのかについて見ていきます。<br/>
<a class="keyword" href="http://d.hatena.ne.jp/keyword/C%2B%2B">C++</a> のテンプレートの強力さとか勘所みたいなものを把握するために非常に重要な部分なので、覚えておくとよいと思います。</p>

<h2><a class="keyword" href="http://d.hatena.ne.jp/keyword/C%2B%2B">C++</a> でテンプレートを<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D1%A5%A4%A5%EB">コンパイル</a>してみる</h2>

<p>早速ですが、実際に <a class="keyword" href="http://d.hatena.ne.jp/keyword/C%2B%2B">C++</a> でテンプレートを使っているコードを<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D1%A5%A4%A5%EB">コンパイル</a>してみます。<br/>
<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%A2%A5%BB%A5%F3%A5%D6%A5%EA">アセンブリ</a>よりも <a class="keyword" href="http://d.hatena.ne.jp/keyword/LLVM">LLVM</a> IR のほうがわかりやすいかな？と思うので、<a class="keyword" href="http://d.hatena.ne.jp/keyword/LLVM">LLVM</a> IR を clang++ で生成させてみます。
（<a class="keyword" href="http://d.hatena.ne.jp/keyword/LLVM">LLVM</a> IR については <a href="http://postd.cc/llvm-for-grad-students/">大学院生のためのLLVM | インフラ・ミドルウェア | POSTD</a> あたりを読んでおくとなんとなく概念がつかめると思います。公式は<a href="http://llvm.org/docs/LangRef.html">LLVM Language Reference Manual — LLVM 3.9 documentation</a>）</p>

<p>対象となるコードはこちら。</p>

<pre class="code lang-cpp" data-lang="cpp" data-unlink><span class="synComment">// main.cpp</span>
<span class="synType">template</span> &lt;<span class="synType">typename</span> T&gt;
T identity(T x) {
  <span class="synStatement">return</span> x;
}

<span class="synType">int</span> main() {
  <span class="synType">float</span> f = <span class="synConstant">0.0f</span>;
  identity(f);

  <span class="synType">int</span> d = <span class="synConstant">0</span>;
  identity(d);
  <span class="synStatement">return</span> <span class="synConstant">0</span>;
}
</pre>

<p>clang++ で <a class="keyword" href="http://d.hatena.ne.jp/keyword/LLVM">LLVM</a> IR を生成させるには，<code>-S -emit-llvm</code> をオプションに指定します。また、今回のコードは最適化されてしまうとほとんどコードが残らないので、最適化を抑制するよう、<code>-O0</code> を付けます。</p>

<pre class="code" data-lang="" data-unlink>$ clang++ -O0 -S -emit-llvm main.cpp</pre>

<p>すると <code>main.ll</code> というファイルが出来ています。これが <a class="keyword" href="http://d.hatena.ne.jp/keyword/LLVM">LLVM</a> IR です。</p>

<h2>IR を読む</h2>

<p><a class="keyword" href="http://d.hatena.ne.jp/keyword/LLVM">LLVM</a> IR は，<a class="keyword" href="http://d.hatena.ne.jp/keyword/LLVM">LLVM</a> という<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D1%A5%A4%A5%E9">コンパイラ</a>基盤技術における中間表現 (Intermediate Representation) です。<br/>
ざっくり言うと、<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%A2%A1%BC%A5%AD%A5%C6%A5%AF%A5%C1%A5%E3">アーキテクチャ</a>に依存しない、読みやすい<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%A2%A5%BB%A5%F3%A5%D6%A5%EA">アセンブリ</a>です。</p>

<p><code>main.ll</code> はそこまで長くないですが、エッセンスだけ抜粋します。</p>

<pre class="code" data-lang="" data-unlink>define i32 @main() #0 { ;; main 関数
  %1 = alloca i32, align 4
  %f = alloca float, align 4
  %d = alloca i32, align 4
  store i32 0, i32* %1
  store float 0.000000e+00, float* %f, align 4
  %2 = load float* %f, align 4
  %3 = call float @_Z8identityIfET_S0_(float %2)
  store i32 0, i32* %d, align 4
  %4 = load i32* %d, align 4
  %5 = call i32 @_Z8identityIiET_S0_(i32 %4)
  ret i32 0
}

define linkonce_odr float @_Z8identityIfET_S0_(float %x) #1 { ;; identity&lt;float&gt; の実体
  %1 = alloca float, align 4
  store float %x, float* %1, align 4
  %2 = load float* %1, align 4
  ret float %2
}

define linkonce_odr i32 @_Z8identityIiET_S0_(i32 %x) #1 { ;; identity&lt;int&gt; の実体
  %1 = alloca i32, align 4
  store i32 %x, i32* %1, align 4
  %2 = load i32* %1, align 4
  ret i32 %2
}</pre>

<p>コメントでも書きましたが、3 つの関数が定義されていることがわかると思います。
ここで重要なのは、 <strong>identity&lt;int>とidentity&lt;float>がそれぞれ別の関数として定義されている</strong> ことです。</p>

<p><code>identity&lt;int&gt;</code> と <code>identity&lt;float&gt;</code> は <code>main</code> の中で使われています。<br/>
つまり、テンプレートは、「使った分だけ実体が作られる。かつその処理は<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D1%A5%A4%A5%EB">コンパイル</a>時に終わる。」ということがわかります。<br/>
たとえば<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%BD%A1%BC%A5%B9%A5%B3%A1%BC%A5%C9">ソースコード</a>中に <code>identity&lt;bool&gt;</code> の実体化を要求するコードがあれば、その時はじめて <code>identity&lt;bool&gt;</code> が作られます。
独自定義でも構いません。<code>identity&lt;MyClass&gt;</code> の実体化を要求するコードがあれば、その時はじめて <code>identity&lt;MyClass&gt;</code> が作られます。</p>

<p>もちろん、一度実体化されたテンプレートは再利用されます。つまり、<code>identity&lt;int&gt;</code> を要求するコードが、一つの<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%BD%A1%BC%A5%B9%A5%B3%A1%BC%A5%C9">ソースコード</a>に何度現れても、ただひとつの <code>identity&lt;int&gt;</code> が生成されます。</p>

<h2><a class="keyword" href="http://d.hatena.ne.jp/keyword/Java">Java</a> との比較</h2>

<p><a class="keyword" href="http://d.hatena.ne.jp/keyword/Java">Java</a> にも<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B8%A5%A7%A5%CD%A5%EA%A5%AF%A5%B9">ジェネリクス</a>という仕組みがあります。<br/>
概念的にはテンプレートに似たものなので、比較してみます (テンプレートの方がより強力ですが、型を汎用化したいという目的であれば、両者とも同様に使用できます。)</p>

<p>同じようなコードを<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D1%A5%A4%A5%EB">コンパイル</a>して<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%D0%A5%A4%A5%C8%A5%B3%A1%BC%A5%C9">バイトコード</a>を見てみます。</p>

<pre class="code lang-java" data-lang="java" data-unlink><span class="synType">class</span> Main {
  <span class="synType">static</span> &lt;T&gt; T identity(T x) {
    <span class="synStatement">return</span> x;
  }

  <span class="synType">public</span> <span class="synType">static</span> <span class="synType">void</span> main(String[] args) {
    Integer d = <span class="synConstant">1</span>;
    Float f = <span class="synConstant">0.0f</span>;
    Main.identity(f);
    Main.identity(d);
  }
}
</pre>

<p><code>javac Main.java</code> してから、<code>javap -v Main</code> します。これで<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%D0%A5%A4%A5%C8%A5%B3%A1%BC%A5%C9">バイトコード</a>が出力されます。</p>

<pre class="code" data-lang="" data-unlink>class Main
/* 中略 */
{
  Main();
    descriptor: ()V
    flags:
    Code:
      stack=1, locals=1, args_size=1
         0: aload_0
         1: invokespecial #1                  // Method java/lang/Object.&#34;&lt;init&gt;&#34;:()V
         4: return
      LineNumberTable:
        line 1: 0

  static &lt;T extends java.lang.Object&gt; T identity(T);
    descriptor: (Ljava/lang/Object;)Ljava/lang/Object;
    flags: ACC_STATIC
    Code:
      stack=1, locals=1, args_size=1
         0: aload_0
         1: areturn
      LineNumberTable:
        line 3: 0
    Signature: #14                          // &lt;T:Ljava/lang/Object;&gt;(TT;)TT;

  public static void main(java.lang.String[]);
    descriptor: ([Ljava/lang/String;)V
    flags: ACC_PUBLIC, ACC_STATIC
    Code:
      stack=1, locals=3, args_size=1
         0: iconst_1
         1: invokestatic  #2                  // Method java/lang/Integer.valueOf:(I)Ljava/lang/Integer;
         4: astore_1
         5: fconst_0
         6: invokestatic  #3                  // Method java/lang/Float.valueOf:(F)Ljava/lang/Float;
         9: astore_2
        10: aload_2
        11: invokestatic  #4                  // Method identity:(Ljava/lang/Object;)Ljava/lang/Object;
        14: pop
        15: aload_1
        16: invokestatic  #4                  // Method identity:(Ljava/lang/Object;)Ljava/lang/Object;
        19: pop
        20: return
      LineNumberTable:
        line 7: 0
        line 8: 5
        line 9: 10
        line 10: 15
        line 11: 20
}
SourceFile: &#34;Main.java&#34;</pre>

<p>注目すべきは <code>// Method identity:(Ljava/lang/Object;)Ljava/lang/Object;</code> というコメントのついた行です。<br/>
2 行ありますが、それぞれ <code>identity(d)</code> と <code>identity(f)</code> に相当します。</p>

<p><code>Integer.valueOf</code> や <code>Float.valueOf</code> を含むコメントを見ていただければわかると思いますが、このコメント部分には呼び出しているメソッドの型が記されています。<br/>
つまり、<code>identity</code> は <code>Integer</code> で呼んでも <code>Float</code> で呼んでも <code>Object identity(Object)</code> を呼んでいるということです。</p>

<p>これは <a class="keyword" href="http://d.hatena.ne.jp/keyword/Java">Java</a> の<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B8%A5%A7%A5%CD%A5%EA%A5%AF%A5%B9">ジェネリクス</a>の大きな特徴で型消去などと呼ばれる性質です。<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B8%A5%A7%A5%CD%A5%EA%A5%AF%A5%B9">ジェネリクス</a>による型はすべて<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D1%A5%A4%A5%EB">コンパイル</a>時にのみ利用され、実行時にはすべて <code>Object</code> として表現しつつ適切にキャストを挟むような構造になっています。<br/>
キャストはキャストでも、正しいことが<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D1%A5%A4%A5%E9">コンパイラ</a>によって保証されたキャストになるので、<code>List</code> よりも <code>List&lt;String&gt;</code> のほうが安全というわけです。</p>

<h2>それぞれの利点と欠点</h2>

<p>テンプレートや<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B8%A5%A7%A5%CD%A5%EA%A5%AF%A5%B9">ジェネリクス</a>を実現する方法として、2つの例を上げました。<br/>
一つは <a class="keyword" href="http://d.hatena.ne.jp/keyword/C%2B%2B">C++</a> の採用している方式で、テンプレート引数ごとに新しく実体を作ってしまう方式です。<br/>
もう一つは <a class="keyword" href="http://d.hatena.ne.jp/keyword/Java">Java</a> の採用している方式で、<code>Object</code> のようにすべての型を受け取れる基底クラスのようなものを用いて、実行時には型情報を残さない方式です。</p>

<p>今回はたまたま <a class="keyword" href="http://d.hatena.ne.jp/keyword/C%2B%2B">C++</a> と <a class="keyword" href="http://d.hatena.ne.jp/keyword/Java">Java</a> を例にあげましたが、他の言語でもこのような方式を使っている言語は多いです。（みんなだいすき D 言語は <a class="keyword" href="http://d.hatena.ne.jp/keyword/C%2B%2B">C++</a> の方式を採用しています）</p>

<p>さてそれぞれの利点と欠点についてです。</p>

<h3><a class="keyword" href="http://d.hatena.ne.jp/keyword/Java">Java</a> 方式</h3>

<ul>
<li>利点

<ul>
<li>分割<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D1%A5%A4%A5%EB">コンパイル</a>が容易（<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B8%A5%A7%A5%CD%A5%EA%A5%C3%A5%AF">ジェネリック</a>な関数を提供する側は <code>Object</code> を入れた<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%D0%A5%A4">バイ</a>ナリや<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%D0%A5%A4%A5%C8%A5%B3%A1%BC%A5%C9">バイトコード</a>と、型情報だけを生成すればよい。使用する側は、型情報から型検査を行った後、<code>Object</code> へのキャストなどを含めた<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%D0%A5%A4">バイ</a>ナリや<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%D0%A5%A4%A5%C8%A5%B3%A1%BC%A5%C9">バイトコード</a>をはけば良い）</li>
</ul>
</li>
<li>欠点

<ul>
<li>実行時にやることが増えるのでオーバヘッドがある</li>
</ul>
</li>
</ul>

<h3><a class="keyword" href="http://d.hatena.ne.jp/keyword/C%2B%2B">C++</a> 方式</h3>

<ul>
<li>利点

<ul>
<li>実行時オーバヘッドなし（全て<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D1%A5%A4%A5%EB">コンパイル</a>時に解決される）</li>
</ul>
</li>
<li>欠点

<ul>
<li><a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%D0%A5%A4">バイ</a>ナリサイズの増加</li>
<li>分割<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D1%A5%A4%A5%EB">コンパイル</a>が困難 (テンプレートを実体化しようと思うと、型情報だけでは足りない。使用者側が定義そのものをまるまる知っている必要がある。)</li>
</ul>
</li>
</ul>

<p>こんな感じでしょうか。<br/>
この比較はあくまで型を汎用化したいという目的に関しての比較です。<a class="keyword" href="http://d.hatena.ne.jp/keyword/C%2B%2B">C++</a> のテンプレートにできて <a class="keyword" href="http://d.hatena.ne.jp/keyword/Java">Java</a> の<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B8%A5%A7%A5%CD%A5%EA%A5%AF%A5%B9">ジェネリクス</a>に出来ないことはたくさんあります。</p>

<h2>まとめ</h2>

<p><a class="keyword" href="http://d.hatena.ne.jp/keyword/C%2B%2B">C++</a>er はみんな実行時のオーバヘッドが嫌いです。テンプレートは、今までに紹介してきた使用法からは想像も出来ないほど豊富な計算を、<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D1%A5%A4%A5%EB">コンパイル</a>時にすべて行うことが出来ます。実行時のオーバヘッドなしで。<br/>
<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D1%A5%A4%A5%EB">コンパイル</a>時にテンプレートの解決が終わるということは、強力な最適化が望めるということでもあります。つまり、実行時のキャストといったわかりやすいオーバヘッド以上に、実行速度には差が生まれるでしょう。</p>

<p>というわけで今回はテンプレートの実現方法について、<a class="keyword" href="http://d.hatena.ne.jp/keyword/Java">Java</a> と比較しながら説明してみました。</p>

---

---
