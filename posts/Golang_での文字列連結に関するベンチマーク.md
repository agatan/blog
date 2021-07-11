---
title: "Golang での文字列連結に関するベンチマーク"
date: 2015-09-08T08:09:45.000Z
tags: []
---

<h1>まず結論</h1>

<p><code>append</code> しよう. <code>bytes.Buffer</code> はそんなに速くない.</p>

<h1>きっかけ</h1>

<p>こんな記事を見かけました.<br/>
<a href="http://qiita.com/ruiu/items/2bb83b29baeae2433a79">Goでは文字列連結はコストの高い操作 - Qiita</a></p>

<p><code>buf += "abc"</code> はコストが高いよーっていうお話ですね. これは <a class="keyword" href="http://d.hatena.ne.jp/keyword/golang">golang</a> にかぎらず, <a class="keyword" href="http://d.hatena.ne.jp/keyword/Java">Java</a>とかでもよく話題になる一般的な問題だと思います.<br/>
<a class="keyword" href="http://d.hatena.ne.jp/keyword/Java">Java</a> だと <code>StringBuilder</code> を使うのが良いとされていたと思いますが, <a class="keyword" href="http://d.hatena.ne.jp/keyword/golang">golang</a> だと解法がいくつかあるようです.</p>

<p>そこで, 解法をそれぞれ紹介した後, <a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%D9%A5%F3%A5%C1%A5%DE%A1%BC%A5%AF">ベンチマーク</a>結果を載せてみたいと思います.</p>

<h2>1. 普通に <code>+=</code></h2>

<p>まずは普通に <code>+=</code> で連結していく方法です.</p>

<pre class="code lang-go" data-lang="go" data-unlink><span class="synStatement">func</span> AddString(n <span class="synType">int</span>) <span class="synType">string</span> {
    base := <span class="synConstant">&quot;abc&quot;</span>
    buf := <span class="synConstant">&quot;&quot;</span>
    <span class="synStatement">for</span> i := <span class="synConstant">0</span>; i &lt; n; i++ {
        buf += base
    }
    <span class="synStatement">return</span> buf
}
</pre>

<p>こんな感じですね.<br/>
これだと, 確実に n 回メモリ割り当てが発生するので遅いというのが問題となります.</p>

<h2>2. <code>append</code> する</h2>

<p><a class="keyword" href="http://d.hatena.ne.jp/keyword/golang">golang</a> の <code>string</code> は, メモリ上では <code>[]byte</code> と同等の表現を持ちます.<br/>
そこで, <code>string</code> を <code>[]byte</code> として扱い, <a class="keyword" href="http://d.hatena.ne.jp/keyword/golang">golang</a> のスライスを伸長する <code>append</code> 関数を用いるという方法があります.</p>

<pre class="code lang-go" data-lang="go" data-unlink><span class="synStatement">func</span> AppendString(n <span class="synType">int</span>) <span class="synType">string</span> {
    base := <span class="synConstant">&quot;abc&quot;</span>
    buf := <span class="synStatement">make</span>([]<span class="synType">byte</span>, <span class="synConstant">0</span>)
    <span class="synStatement">for</span> i := <span class="synConstant">0</span>; i &lt; n; i++ {
        buf = append(buf, base...)
    }
    <span class="synStatement">return</span> <span class="synType">string</span>(buf)
}
</pre>

<p><code>make([]byte, 0)</code> によって, 長さ0のスライスを作って, それを伸長していく方法となっています.<br/>
このあたりは <a class="keyword" href="http://d.hatena.ne.jp/keyword/golang">golang</a> のスライスの表現について知っていないとわかりづらいと思うのですが, わかりやすい説明がいろいろなところで読めるので, ここでは説明しません.
<code>append</code> 関数は, スライスの len を必要な分だけ大きくします. また, その結果 len が スライスの cap を超える長さになる場合は, スライスの cap を必要以上に大きくすします.<br/>
これは, <code>append</code> を繰り返し適用するような場合(今回のように)に, メモリ割り当ての回数を最小にするためです. 一度の <code>append</code> で大きめにメモリを確保しておくことで, 次の <code>append</code> ではメモリ割り当てをしなくても済む可能性が生まれます.<br/>
イメージとしては,</p>

<table>
<thead>
<tr>
<th style="text-align:center;"> append の回数 </th>
<th style="text-align:center;"> 0 </th>
<th style="text-align:center;"> 1 </th>
<th style="text-align:center;"> 2 </th>
<th style="text-align:center;"> 3 </th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:center;"> len </td>
<td style="text-align:center;"> 0 </td>
<td style="text-align:center;"> 3 </td>
<td style="text-align:center;"> 6 </td>
<td style="text-align:center;"> 9 </td>
</tr>
<tr>
<td style="text-align:center;"> cap </td>
<td style="text-align:center;"> 0 </td>
<td style="text-align:center;"> 8 </td>
<td style="text-align:center;"> 8 </td>
<td style="text-align:center;"> 16 </td>
</tr>
</tbody>
</table>

<p>こんな感じでしょうか(あくまでイメージですが)<br/>
<code>append</code> は3回呼ばれていますが, メモリ割り当ては2回に抑えられています.<br/>
その分, <code>+=</code> よりも速いだろうということですね.</p>

<h3>2'. <code>cap</code> を十分量確保しておく</h3>

<p><code>make</code> によるスライスの作成の際には, 長さだけでなくキャパシティを指定することが出来ます.<br/>
したがって, はじめから <code>append</code> していった後の最終的なスライスの長さがわかっているのであれば, それをキャパシティに指定することで, メモリ割り当てを最小限に抑えることが可能になります.</p>

<pre class="code lang-go" data-lang="go" data-unlink><span class="synStatement">func</span> AppendStringWithCap(n <span class="synType">int</span>) <span class="synType">string</span> {
    base := <span class="synConstant">&quot;abc&quot;</span>
    buf := <span class="synStatement">make</span>([]<span class="synType">byte</span>, <span class="synConstant">0</span>, <span class="synConstant">3</span>*n)
    <span class="synStatement">for</span> i := <span class="synConstant">0</span>; i &lt; n; i++ {
        buf = append(buf, base...)
    }
    <span class="synStatement">return</span> <span class="synType">string</span>(buf)
}
</pre>

<h2>3. <code>bytes.Buffer</code> を使う</h2>

<p><a class="keyword" href="http://d.hatena.ne.jp/keyword/Java">Java</a> の <code>StringBuilder</code> に近い解法ですね.<br/>
<code>bytes.Buffer</code> は文字通りバイト列のバッファリングを行ってくれます.<br/>
<code>bytes.Buffer</code> に文字列やバイト列を書き込んでいくと, 自動的にメモリ割り当てを減らすように計らってくれます.</p>

<pre class="code lang-go" data-lang="go" data-unlink><span class="synStatement">func</span> BufferString(n <span class="synType">int</span>) <span class="synType">string</span> {
    base := <span class="synConstant">&quot;abc&quot;</span>
    <span class="synStatement">var</span> buf <span class="synType">bytes.Buffer</span>
    <span class="synStatement">for</span> i := <span class="synConstant">0</span>; i &lt; n; i++ {
        buf.WriteString(base)
    }
    <span class="synStatement">return</span> buf.String()
}
</pre>

<p>こんな感じです.</p>

<h1><a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%D9%A5%F3%A5%C1%A5%DE%A1%BC%A5%AF">ベンチマーク</a>結果</h1>

<p><a class="keyword" href="http://d.hatena.ne.jp/keyword/golang">golang</a> には<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%D9%A5%F3%A5%C1%A5%DE%A1%BC%A5%AF">ベンチマーク</a>をとる機能も標準で付いているので, それを利用しました.</p>

<pre class="code lang-go" data-lang="go" data-unlink><span class="synStatement">package</span> main

<span class="synStatement">import</span> <span class="synConstant">&quot;testing&quot;</span>

<span class="synStatement">const</span> N = <span class="synConstant">1000</span>

<span class="synStatement">func</span> BenchmarkAddString(b *testing.B) {
    <span class="synStatement">for</span> i := <span class="synConstant">0</span>; i &lt; b.N; i++ {
        AddString(N)
    }
}

<span class="synStatement">func</span> BenchmarkAppendString(b *testing.B) {
    <span class="synStatement">for</span> i := <span class="synConstant">0</span>; i &lt; b.N; i++ {
        AppendString(N)
    }
}

<span class="synStatement">func</span> BenchmarkAppendStringWithCap(b *testing.B) {
    <span class="synStatement">for</span> i := <span class="synConstant">0</span>; i &lt; b.N; i++ {
        AppendStringWithCap(N)
    }
}

<span class="synStatement">func</span> BenchmarkBufferString(b *testing.B) {
    <span class="synStatement">for</span> i := <span class="synConstant">0</span>; i &lt; b.N; i++ {
        BufferString(N)
    }
}

<span class="synStatement">func</span> TestEquality(t *testing.T) {
    base := AddString(N)
    tests := []<span class="synType">string</span>{
        AppendString(N),
        AppendStringWithCap(N),
        BufferString(N),
    }
    <span class="synStatement">for</span> _, test := <span class="synStatement">range</span> tests {
        <span class="synStatement">if</span> base != test {
            t.Fatal(<span class="synConstant">&quot;not fair&quot;</span>)
        }
    }
}
</pre>

<p><code>TestEquality</code> は, すべての方法で正しく文字列を生成できていることを確認するためのテストです. <a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%D9%A5%F3%A5%C1%A5%DE%A1%BC%A5%AF">ベンチマーク</a>には関係ありません.</p>

<h2>結果</h2>

<p>上記のファイルを用意した後, <code>go test -bench . -benchmem</code> とした結果を以下に示します.</p>

<pre class="code" data-lang="" data-unlink>PASS
BenchmarkAddString-8                5000        348347 ns/op     1592481 B/op        999 allocs/op
BenchmarkAppendString-8           200000          7346 ns/op       13056 B/op         12 allocs/op
BenchmarkAppendStringWithCap-8    300000          5461 ns/op        6144 B/op          2 allocs/op
BenchmarkBufferString-8           100000         16847 ns/op       12256 B/op          8 allocs/op
ok      github.com/agatan/bench 6.881s</pre>

<p>というわけで, <code>make</code> の時点で十分なメモリを確保しておく 2' の方法が最も速く最もメモリを消費しないことがわかりました.<br/>
まぁ当たり前ですねｗｗ</p>

<p>より注目すべきは, 2 と 4 の結果です. 今回の結果だと, 最終的な文字列の長さがわからない場合, <code>bytes.Buffer</code> よりも <code>append</code> を使ったほうが速いという結果になっています (メモリ使用量は若干 <code>bytes.Buffer</code> のほうが小さい)</p>

<p>メモリ割り当ての回数も <code>bytes.Buffer</code> のほうが少なく済んでいるため, <code>[]byte</code> と <code>string</code> の変換など, 文字列連結以外の部分でのオーバーヘッドが大きいため, このような結果になった可能性があります. そこで, <code>N</code> の値を変えて実行してみました.</p>

<h2>N = 10 の場合</h2>

<pre class="code" data-lang="" data-unlink>PASS
BenchmarkAddString-8             2000000           613 ns/op         224 B/op          9 allocs/op
BenchmarkAppendString-8          5000000           270 ns/op          96 B/op          4 allocs/op
BenchmarkAppendStringWithCap-8  10000000           142 ns/op          64 B/op          2 allocs/op
BenchmarkBufferString-8          5000000           251 ns/op         144 B/op          2 allocs/op
ok      github.com/agatan/bench 6.581s</pre>

<h2>N = 10000 の場合</h2>

<pre class="code" data-lang="" data-unlink>PASS
BenchmarkAddString-8                  50      28544042 ns/op    160274378 B/op     10039 allocs/op
BenchmarkAppendString-8            20000         71285 ns/op      160768 B/op         20 allocs/op
BenchmarkAppendStringWithCap-8     30000         55262 ns/op       65536 B/op          2 allocs/op
BenchmarkBufferString-8            10000        151665 ns/op      109280 B/op         11 allocs/op
ok      github.com/agatan/bench 7.393s</pre>

<h1>結論</h1>

<p>連結する文字列の長さや連結の回数にもよるが, おおよそ <code>append</code> のほうが速い！！<br/>
<code>bytes.Buffer</code> はいつ使えばいいの...</p>

---

---
