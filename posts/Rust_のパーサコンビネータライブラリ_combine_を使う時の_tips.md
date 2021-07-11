---
title: "Rust のパーサコンビネータライブラリ combine を使う時の tips"
date: 2016-05-14T16:03:19.000Z
tags: []
---

<p>Rust のパーサ<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D3%A5%CD%A1%BC%A5%BF">コンビネータ</a>ライブラリの一つである <a href="https://github.com/Marwes/combine">Marwes/combine: A parser combinator library for Rust</a> を使ってみています．<br/>
詳しい使い方はきちんとしたドキュメントがあるのでそちらを参照してください．<br/>
ざっくりいうと <a class="keyword" href="http://d.hatena.ne.jp/keyword/Haskell">Haskell</a> の <a href="https://hackage.haskell.org/package/parsec">parsec: Monadic parser combinators | Hackage</a> の Rust 版という感じです．</p>

<p>(ちなみに私も combine を参考に <a class="keyword" href="http://d.hatena.ne.jp/keyword/C%2B%2B">C++</a> でパーサ<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D3%A5%CD%A1%BC%A5%BF">コンビネータ</a>を作ってみたりしました. )
<iframe src="http://agtn.hatenablog.com/embed/2016/04/30/003009" title="C++ でパーサコンビネータを書きました - 右上➚" class="embed-card embed-blogcard" scrolling="no" frameborder="0" style="display: block; width: 100%; height: 190px; max-width: 500px; margin: 10px 0px;"></iframe><cite class="hatena-citation"><a href="http://agtn.hatenablog.com/entry/2016/04/30/003009">agtn.hatenablog.com</a></cite></p>

<p>で、このライブラリ、とても<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B8%A5%A7%A5%CD%A5%EA%A5%C3%A5%AF">ジェネリック</a>なコードで書かれているので、かなり<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D1%A5%A4%A5%EB">コンパイル</a>時間が増加します&hellip; (Boost.Spirit 系に近いです． <a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D1%A5%A4%A5%EB">コンパイル</a>エラーなどは遥かに読みやすいのであまり困ることはないですが)  <br/>
パーサを書いている時にはテストは頻繁に行いたいので，ちょっと<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D1%A5%A4%A5%EB">コンパイル</a>がおそいのはつらい．</p>

<p>なにか解決策はないかなぁと思っていたら本家に issue がたっていました．<br/>
<a href="https://github.com/Marwes/combine/issues/21">Extremely long compile times · Issue #21 · Marwes/combine</a></p>

<p>今回この issue にかかれていた内容を検証してみたので，ここでまとめておこうと思います．</p>

<h2>結論</h2>

<p>パーサの定義を，<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B8%A5%A7%A5%CD%A5%EA%A5%C3%A5%AF">ジェネリック</a>な構造体のメソッドとして定義すると<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D1%A5%A4%A5%EB">コンパイル</a>時間が大幅に短くなる</p>

<h2>方法</h2>

<p>まずはじめに言われているのは，入力ストリーム型を<code>I: Stream&lt;Item=char&gt;</code> から <code>&amp;str</code> にしてしまうという方法です．</p>

<blockquote><p>(It might be possible to specialize the parsers directly as well, say
<code>
fn expr(input: State&lt;&amp;str&gt;) -&gt; ParseResult&lt;Expr, &amp;str&gt;
</code>
instead of
<code>
fn expr&lt;I: Stream&gt;(input: State&lt;I&gt;) -&gt; ParseResult&lt;I, &amp;str&gt;
</code>
)</p></blockquote>

<p>これは作ったパーサを<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B8%A5%A7%A5%CD%A5%EA%A5%C3%A5%AF">ジェネリック</a>な入力に対して適用することができなくなりますが，ライブラリの利用者側としては，<code>char</code> のストリームといったらだいたい <code>&amp;str</code> だと思うので，ぶっちゃけ<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B8%A5%A7%A5%CD%A5%EA%A5%C3%A5%AF">ジェネリック</a>じゃなくてもいいじゃんという感じです．</p>

<p>そしてもう一つが,  <a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B8%A5%A7%A5%CD%A5%EA%A5%C3%A5%AF">ジェネリック</a>な構造体を作って，パーサの定義をその中に閉じ込めるという方法です．<br/>
ちょっとこちらはコード例を実際に見たほうがわかりやすいと思うので後で説明します．</p>

<h2>実験コード</h2>

<p>というわけで，</p>

<ol>
<li><a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B8%A5%A7%A5%CD%A5%EA%A5%C3%A5%AF">ジェネリック</a>なパーサ</li>
<li>&amp;str のみを受理するパーサ</li>
<li><a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B8%A5%A7%A5%CD%A5%EA%A5%C3%A5%AF">ジェネリック</a>な構造体の中に定義された<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B8%A5%A7%A5%CD%A5%EA%A5%C3%A5%AF">ジェネリック</a>でないパーサ</li>
</ol>

<p>の三種類について，<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D1%A5%A4%A5%EB">コンパイル</a>時間をはかってみます．</p>

<p>パーサ界のハローワールド，計算機で実験してみます．
まずは<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B8%A5%A7%A5%CD%A5%EA%A5%C3%A5%AF">ジェネリック</a>なパーサです．</p>

<pre class="code lang-rust" data-lang="rust" data-unlink><span class="synStatement">use</span> <span class="synPreProc">combine</span><span class="synSpecial">::</span><span class="synType">*</span>;
<span class="synStatement">use</span> <span class="synPreProc">combine</span><span class="synSpecial">::</span><span class="synPreProc">primitives</span><span class="synSpecial">::</span>Stream;

<span class="synStatement">pub</span> <span class="synStatement">fn</span> <span class="synIdentifier">integer</span><span class="synStatement">&lt;</span>I<span class="synStatement">&gt;</span>(input: State<span class="synStatement">&lt;</span>I<span class="synStatement">&gt;</span>) <span class="synStatement">-&gt;</span> ParseResult<span class="synStatement">&lt;</span><span class="synType">i64</span>, I<span class="synStatement">&gt;</span>
    <span class="synStatement">where</span> I: Stream<span class="synStatement">&lt;</span>Item <span class="synStatement">=</span> <span class="synType">char</span><span class="synStatement">&gt;</span>
{
    <span class="synIdentifier">many1</span><span class="synSpecial">::</span><span class="synStatement">&lt;</span><span class="synType">Vec</span><span class="synStatement">&lt;</span>_<span class="synStatement">&gt;</span>, _<span class="synStatement">&gt;</span>(<span class="synIdentifier">digit</span>())
        .<span class="synIdentifier">map</span>(<span class="synStatement">|</span>is<span class="synStatement">|</span> is.<span class="synIdentifier">into_iter</span>().<span class="synIdentifier">fold</span>(<span class="synConstant">0</span>, <span class="synStatement">|</span>lhs, rhs<span class="synStatement">|</span> lhs <span class="synStatement">+</span> (rhs <span class="synStatement">as</span> <span class="synType">i64</span> <span class="synStatement">-</span> <span class="synConstant">'0'</span> <span class="synStatement">as</span> <span class="synType">i64</span>)))
        .<span class="synIdentifier">parse_state</span>(input)
}

<span class="synStatement">pub</span> <span class="synStatement">fn</span> <span class="synIdentifier">factor</span><span class="synStatement">&lt;</span>I<span class="synStatement">&gt;</span>(input: State<span class="synStatement">&lt;</span>I<span class="synStatement">&gt;</span>) <span class="synStatement">-&gt;</span> ParseResult<span class="synStatement">&lt;</span><span class="synType">i64</span>, I<span class="synStatement">&gt;</span>
    <span class="synStatement">where</span> I: Stream<span class="synStatement">&lt;</span>Item <span class="synStatement">=</span> <span class="synType">char</span><span class="synStatement">&gt;</span>
{
    <span class="synIdentifier">between</span>(<span class="synType">char</span>(<span class="synConstant">'('</span>), <span class="synType">char</span>(<span class="synConstant">')'</span>), <span class="synIdentifier">parser</span>(expr)).<span class="synIdentifier">or</span>(<span class="synIdentifier">parser</span>(integer)).<span class="synIdentifier">parse_state</span>(input)
}

<span class="synStatement">pub</span> <span class="synStatement">fn</span> <span class="synIdentifier">term</span><span class="synStatement">&lt;</span>I<span class="synStatement">&gt;</span>(input: State<span class="synStatement">&lt;</span>I<span class="synStatement">&gt;</span>) <span class="synStatement">-&gt;</span> ParseResult<span class="synStatement">&lt;</span><span class="synType">i64</span>, I<span class="synStatement">&gt;</span>
    <span class="synStatement">where</span> I: Stream<span class="synStatement">&lt;</span>Item <span class="synStatement">=</span> <span class="synType">char</span><span class="synStatement">&gt;</span>
{
    <span class="synStatement">let</span> op <span class="synStatement">=</span> <span class="synType">char</span>(<span class="synConstant">'*'</span>).<span class="synIdentifier">or</span>(<span class="synType">char</span>(<span class="synConstant">'/'</span>)).<span class="synIdentifier">map</span>(<span class="synStatement">|</span>c<span class="synStatement">|</span> {
        <span class="synType">move</span> <span class="synStatement">|</span>lhs, rhs<span class="synStatement">|</span> <span class="synStatement">match</span> c {
            <span class="synConstant">'*'</span> <span class="synStatement">=&gt;</span> lhs <span class="synStatement">*</span> rhs,
            <span class="synConstant">'/'</span> <span class="synStatement">=&gt;</span> lhs <span class="synStatement">/</span> rhs,
            _ <span class="synStatement">=&gt;</span> <span class="synPreProc">unreachable!</span>(),
        }
    });
    <span class="synIdentifier">chainl1</span>(<span class="synIdentifier">parser</span>(factor), op).<span class="synIdentifier">parse_state</span>(input)
}

<span class="synStatement">pub</span> <span class="synStatement">fn</span> <span class="synIdentifier">expr</span><span class="synStatement">&lt;</span>I<span class="synStatement">&gt;</span>(input: State<span class="synStatement">&lt;</span>I<span class="synStatement">&gt;</span>) <span class="synStatement">-&gt;</span> ParseResult<span class="synStatement">&lt;</span><span class="synType">i64</span>, I<span class="synStatement">&gt;</span>
    <span class="synStatement">where</span> I: Stream<span class="synStatement">&lt;</span>Item <span class="synStatement">=</span> <span class="synType">char</span><span class="synStatement">&gt;</span>
{
    <span class="synStatement">let</span> op <span class="synStatement">=</span> <span class="synType">char</span>(<span class="synConstant">'+'</span>).<span class="synIdentifier">or</span>(<span class="synType">char</span>(<span class="synConstant">'-'</span>)).<span class="synIdentifier">map</span>(<span class="synStatement">|</span>c<span class="synStatement">|</span> {
        <span class="synType">move</span> <span class="synStatement">|</span>lhs, rhs<span class="synStatement">|</span> <span class="synStatement">match</span> c {
            <span class="synConstant">'+'</span> <span class="synStatement">=&gt;</span> lhs <span class="synStatement">+</span> rhs,
            <span class="synConstant">'-'</span> <span class="synStatement">=&gt;</span> lhs <span class="synStatement">-</span> rhs,
            _ <span class="synStatement">=&gt;</span> <span class="synPreProc">unreachable!</span>(),
        }
    });
    <span class="synIdentifier">chainl1</span>(<span class="synIdentifier">parser</span>(term), op).<span class="synIdentifier">parse_state</span>(input)
}

<span class="synStatement">pub</span> <span class="synStatement">fn</span> <span class="synIdentifier">parse</span><span class="synStatement">&lt;</span>I<span class="synStatement">&gt;</span>(input: I) <span class="synStatement">-&gt;</span> <span class="synType">Result</span><span class="synStatement">&lt;</span>(<span class="synType">i64</span>, I), ParseError<span class="synStatement">&lt;</span>I<span class="synStatement">&gt;&gt;</span>
    <span class="synStatement">where</span> I: Stream<span class="synStatement">&lt;</span>Item <span class="synStatement">=</span> <span class="synType">char</span><span class="synStatement">&gt;</span>
{
    <span class="synIdentifier">parser</span>(expr).<span class="synIdentifier">parse</span>(input)
}
</pre>

<p>それぞれの関数が一つのパーサの役割を担います．それぞれのパーサが独立していて，それぞれ別々に型変数を導入しています．</p>

<p>次に <code>&amp;str</code> だけを受け取るパーサです．これは上記の<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B8%A5%A7%A5%CD%A5%EA%A5%C3%A5%AF">ジェネリック</a>なパーサの，型変数を <code>&amp;str</code> に置き換えるだけなのでとても簡単です．<br/>
一部だけ掲載します．</p>

<pre class="code lang-rust" data-lang="rust" data-unlink><span class="synStatement">pub</span> <span class="synStatement">fn</span> <span class="synIdentifier">expr</span>(input: State<span class="synStatement">&lt;</span><span class="synType">&amp;str</span><span class="synStatement">&gt;</span>) <span class="synStatement">-&gt;</span> ParseResult<span class="synStatement">&lt;</span><span class="synType">i64</span>, <span class="synType">&amp;str</span><span class="synStatement">&gt;</span> {
     <span class="synStatement">let</span> op <span class="synStatement">=</span> <span class="synType">char</span>(<span class="synConstant">'+'</span>).<span class="synIdentifier">or</span>(<span class="synType">char</span>(<span class="synConstant">'-'</span>)).<span class="synIdentifier">map</span>(<span class="synStatement">|</span>c<span class="synStatement">|</span> {
        <span class="synType">move</span> <span class="synStatement">|</span>lhs, rhs<span class="synStatement">|</span> <span class="synStatement">match</span> c {
            <span class="synConstant">'+'</span> <span class="synStatement">=&gt;</span> lhs <span class="synStatement">+</span> rhs,
            <span class="synConstant">'-'</span> <span class="synStatement">=&gt;</span> lhs <span class="synStatement">-</span> rhs,
            _ <span class="synStatement">=&gt;</span> <span class="synPreProc">unreachable!</span>(),
        }
    });
    <span class="synIdentifier">chainl1</span>(<span class="synIdentifier">parser</span>(term), op).<span class="synIdentifier">parse_state</span>(input)
}

<span class="synStatement">pub</span> <span class="synStatement">fn</span> <span class="synIdentifier">parse</span>(input: <span class="synType">&amp;str</span>) <span class="synStatement">-&gt;</span> <span class="synType">Result</span><span class="synStatement">&lt;</span>(<span class="synType">i64</span>, <span class="synType">&amp;str</span>), ParseError<span class="synStatement">&lt;</span><span class="synType">&amp;str</span><span class="synStatement">&gt;&gt;</span> {
    <span class="synIdentifier">parser</span>(expr).<span class="synIdentifier">parse</span>(input)
}
</pre>

<p>最後が，<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B8%A5%A7%A5%CD%A5%EA%A5%C3%A5%AF">ジェネリック</a>な構造体のメソッド中に，<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B8%A5%A7%A5%CD%A5%EA%A5%C3%A5%AF">ジェネリック</a>でないパーサを定義して閉じ込める方法です．</p>

<pre class="code lang-rust" data-lang="rust" data-unlink><span class="synStatement">use</span> <span class="synPreProc">combine</span><span class="synSpecial">::</span><span class="synType">*</span>;
<span class="synStatement">use</span> <span class="synPreProc">combine</span><span class="synSpecial">::</span><span class="synPreProc">primitives</span><span class="synSpecial">::</span>Stream;
<span class="synStatement">use</span> <span class="synPreProc">std</span><span class="synSpecial">::</span><span class="synPreProc">marker</span><span class="synSpecial">::</span>PhantomData;

 truct P<span class="synStatement">&lt;</span>I<span class="synStatement">&gt;</span>(PhantomData<span class="synStatement">&lt;fn</span>(I) <span class="synStatement">-&gt;</span> I<span class="synStatement">&gt;</span>);

<span class="synStatement">impl&lt;</span>I<span class="synStatement">&gt;</span> P<span class="synStatement">&lt;</span>I<span class="synStatement">&gt;</span> <span class="synStatement">where</span> I: Stream<span class="synStatement">&lt;</span>Item <span class="synStatement">=</span> <span class="synType">char</span><span class="synStatement">&gt;</span> {
    <span class="synStatement">pub</span> <span class="synStatement">fn</span> <span class="synIdentifier">integer</span>(input: State<span class="synStatement">&lt;</span>I<span class="synStatement">&gt;</span>) <span class="synStatement">-&gt;</span> ParseResult<span class="synStatement">&lt;</span><span class="synType">i64</span>, I<span class="synStatement">&gt;</span> {
        <span class="synIdentifier">many1</span><span class="synSpecial">::</span><span class="synStatement">&lt;</span><span class="synType">Vec</span><span class="synStatement">&lt;</span>_<span class="synStatement">&gt;</span>, _<span class="synStatement">&gt;</span>(<span class="synIdentifier">digit</span>())
            .<span class="synIdentifier">map</span>(<span class="synStatement">|</span>is<span class="synStatement">|</span> is.<span class="synIdentifier">into_iter</span>().<span class="synIdentifier">fold</span>(<span class="synConstant">0</span>, <span class="synStatement">|</span>lhs, rhs<span class="synStatement">|</span> lhs <span class="synStatement">+</span> (rhs <span class="synStatement">as</span> <span class="synType">i64</span> <span class="synStatement">-</span> <span class="synConstant">'0'</span> <span class="synStatement">as</span> <span class="synType">i64</span>)))
            .<span class="synIdentifier">parse_state</span>(input)
    }

    <span class="synStatement">pub</span> <span class="synStatement">fn</span> <span class="synIdentifier">factor</span>(input: State<span class="synStatement">&lt;</span>I<span class="synStatement">&gt;</span>) <span class="synStatement">-&gt;</span> ParseResult<span class="synStatement">&lt;</span><span class="synType">i64</span>, I<span class="synStatement">&gt;</span> {
        <span class="synIdentifier">between</span>(<span class="synType">char</span>(<span class="synConstant">'('</span>), <span class="synType">char</span>(<span class="synConstant">')'</span>), <span class="synIdentifier">parser</span>(<span class="synIdentifier">P</span><span class="synSpecial">::</span><span class="synStatement">&lt;</span>I<span class="synStatement">&gt;</span><span class="synSpecial">::</span>expr))
            .<span class="synIdentifier">or</span>(<span class="synIdentifier">parser</span>(<span class="synIdentifier">P</span><span class="synSpecial">::</span><span class="synStatement">&lt;</span>I<span class="synStatement">&gt;</span><span class="synSpecial">::</span>integer))
            .<span class="synIdentifier">parse_state</span>(input)
    }

    <span class="synStatement">pub</span> <span class="synStatement">fn</span> <span class="synIdentifier">term</span>(input: State<span class="synStatement">&lt;</span>I<span class="synStatement">&gt;</span>) <span class="synStatement">-&gt;</span> ParseResult<span class="synStatement">&lt;</span><span class="synType">i64</span>, I<span class="synStatement">&gt;</span> {
        <span class="synStatement">let</span> op <span class="synStatement">=</span> <span class="synType">char</span>(<span class="synConstant">'*'</span>).<span class="synIdentifier">or</span>(<span class="synType">char</span>(<span class="synConstant">'/'</span>)).<span class="synIdentifier">map</span>(<span class="synStatement">|</span>c<span class="synStatement">|</span> {
            <span class="synType">move</span> <span class="synStatement">|</span>lhs, rhs<span class="synStatement">|</span> <span class="synStatement">match</span> c {
                <span class="synConstant">'*'</span> <span class="synStatement">=&gt;</span> lhs <span class="synStatement">*</span> rhs,
                <span class="synConstant">'/'</span> <span class="synStatement">=&gt;</span> lhs <span class="synStatement">/</span> rhs,
                _ <span class="synStatement">=&gt;</span> <span class="synPreProc">unreachable!</span>(),
            }
        });
        <span class="synIdentifier">chainl1</span>(<span class="synIdentifier">parser</span>(<span class="synIdentifier">P</span><span class="synSpecial">::</span><span class="synStatement">&lt;</span>I<span class="synStatement">&gt;</span><span class="synSpecial">::</span>factor), op).<span class="synIdentifier">parse_state</span>(input)
    }

    <span class="synStatement">pub</span> <span class="synStatement">fn</span> <span class="synIdentifier">expr</span>(input: State<span class="synStatement">&lt;</span>I<span class="synStatement">&gt;</span>) <span class="synStatement">-&gt;</span> ParseResult<span class="synStatement">&lt;</span><span class="synType">i64</span>, I<span class="synStatement">&gt;</span> {
        <span class="synStatement">let</span> op <span class="synStatement">=</span> <span class="synType">char</span>(<span class="synConstant">'+'</span>).<span class="synIdentifier">or</span>(<span class="synType">char</span>(<span class="synConstant">'-'</span>)).<span class="synIdentifier">map</span>(<span class="synStatement">|</span>c<span class="synStatement">|</span> {
            <span class="synType">move</span> <span class="synStatement">|</span>lhs, rhs<span class="synStatement">|</span> <span class="synStatement">match</span> c {
                <span class="synConstant">'+'</span> <span class="synStatement">=&gt;</span> lhs <span class="synStatement">+</span> rhs,
                <span class="synConstant">'-'</span> <span class="synStatement">=&gt;</span> lhs <span class="synStatement">-</span> rhs,
                _ <span class="synStatement">=&gt;</span> <span class="synPreProc">unreachable!</span>(),
            }
        });
        <span class="synIdentifier">chainl1</span>(<span class="synIdentifier">parser</span>(<span class="synIdentifier">P</span><span class="synSpecial">::</span><span class="synStatement">&lt;</span>I<span class="synStatement">&gt;</span><span class="synSpecial">::</span>term), op).<span class="synIdentifier">parse_state</span>(input)
    }
}

<span class="synStatement">pub</span> <span class="synStatement">fn</span> <span class="synIdentifier">parse</span>(input: <span class="synType">&amp;str</span>) <span class="synStatement">-&gt;</span> <span class="synType">Result</span><span class="synStatement">&lt;</span>(<span class="synType">i64</span>, <span class="synType">&amp;str</span>), ParseError<span class="synStatement">&lt;</span><span class="synType">&amp;str</span><span class="synStatement">&gt;&gt;</span> {
    <span class="synIdentifier">parser</span>(<span class="synPreProc">P</span><span class="synSpecial">::</span>expr).<span class="synIdentifier">parse</span>(input)
}
</pre>

<p>言葉で説明すると難しいのですが，型変数を導入する部分を構造体の定義部分だけにしてあげることで，型変数をそれぞれのパーサが別々に導入する必要がなくなっています．<br/>
コードも割りとすっきりしますね．</p>

<h2>結果</h2>

<p>上記をコードを <code>cfg</code> を使って<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D1%A5%A4%A5%EB">コンパイル</a>時に切り替えながら<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D1%A5%A4%A5%EB">コンパイル</a>してみました．<br/>
本当はきちんと繰り返し計測すべきですが，ちょっとサボっています．まぁ何度実行してもだいたい同じくらいになったので許してください．</p>

<table>
<thead>
<tr>
<th style="text-align:left;"> 実装方法 </th>
<th style="text-align:right;"> <a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D1%A5%A4%A5%EB">コンパイル</a>時間 </th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;"> <a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B8%A5%A7%A5%CD%A5%EA%A5%C3%A5%AF">ジェネリック</a> </td>
<td style="text-align:right;"> 2.666s </td>
</tr>
<tr>
<td style="text-align:left;"> <code>&amp;str</code> </td>
<td style="text-align:right;"> 1.70s </td>
</tr>
<tr>
<td style="text-align:left;"> 構造体内で定義 </td>
<td style="text-align:right;"> 1.55s </td>
</tr>
</tbody>
</table>

<p>このような結果になりました．<br/>
つまり，先ほどの issue で述べられている<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D1%A5%A4%A5%EB">コンパイル</a>時間の短縮方法はかなり効き目があるということですね．<br/>
構造体の中に閉じ込める方法が，<code>&amp;str</code> しか受理しないようにする方法よりもはやく<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D1%A5%A4%A5%EB">コンパイル</a>できるのは意外でした&hellip; 参照を引数にとると暗黙に lifetime 変数が導入されたと記憶しているので，その関係なのかな？</p>

<p>構造体内で定義する方法では，<code>&amp;str</code> 以外の入力ストリーム型を受けつけることを可能にしつつも<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D1%A5%A4%A5%EB">コンパイル</a>時間を短縮できるということで，積極的にこの方式でパーサを定義するべきということがわかりました．</p>

<p>注意点として，構造体内で別のパーサを呼ぶときには，必ず <code>P::term</code> という形ではなく，<code>P::&lt;I&gt;::term</code> という形で呼び出すようにする必要があるようです．<br/>
きちんと明示的に指定しないと，結局<a class="keyword" href="http://d.hatena.ne.jp/keyword/%B7%BF%BF%E4%CF%C0">型推論</a>するはめになって意味がないということのようです．</p>

---

COMMENT:
AUTHOR: anevaden
EMAIL: pgbsajrq@gmail.com
URL: http://cialisvipsale.com
IP: 60.173.69.118
DATE: 03/24/2018 13:14:29

Regards. Quite a lot of posts.

tadalafil 20 mg <a href="http://cialisvipsale.com">cialis tablets australia</a>
interactions for cialis <a href="http://cialisvipsale.com">http://cialisvipsale.com</a>
cialis 5mg prix <a href="http://cialisvipsale.com">cialis generico</a>
cialis dose 30mg

---

---
