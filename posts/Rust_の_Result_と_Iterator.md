---
title: "Rust の Result と Iterator"
date: 2016-09-05T14:47:25.000Z
tags: ["Rust"]
---

<p>Rust には失敗するかもしれない値を表す <code>Result&lt;T, E&gt;</code> という型があります。
<a href="https://doc.rust-lang.org/std/result/enum.Result.html">std::result::Result</a></p>

<p>そして iterate できることを表す <code>Iterator</code> という trait があります。
<a href="https://doc.rust-lang.org/std/iter/trait.Iterator.html">std::iter::Iterator</a></p>

<p>また、<code>Iterator</code> trait は要素型を表す関連型を持ちます。<del>例えば <code>String</code> は <code>Iterator&lt;Item=char&gt;</code> を <code>impl</code> しています。これは <code>char</code> 型を要素にもつ <code>Iterator</code> であることを意味します。</del>
ここ間違っていました。<code>String</code> が直接 <code>Iterator</code> を <code>impl</code> しているのではありませんでした。</p>

<p>たまに <code>Iterator&lt;Item=Result&lt;T, E&gt;&gt;</code> のようになっている型を見かけます（T, E にはなにかしら具体的な型が入っていると思ってください）。
例えば、<code>std::io::stdin().bytes()</code> の返り値である <a href="https://doc.rust-lang.org/stable/std/io/struct.Bytes.html">std::io::Bytes</a> は <code>Iterator&lt;Item=Result&lt;u8&gt;&gt;</code> を <code>impl</code> しています。
（ちょっとわかりにくいのですがここでの <code>Result</code> は <code>std::result::Result</code> ではなくて <code>std::io::Result</code> です。<code>std::io::Result</code> は <code>std::result::Result&lt;T, std::io::Error&gt;</code> の<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%A8%A5%A4%A5%EA%A5%A2%A5%B9">エイリアス</a>です。）</p>

<p>さて、このような <code>Iterator</code> からすべての要素が <code>Ok(_)</code> であれば <code>Ok&lt;Vec_&gt;&gt;</code> を、<code>Err(_)</code> があれば <code>Err&lt;_&gt;</code> を返すような処理を書きたいということは割りとよくあります。
で、これを一生懸命実装しようとしていたのですが、標準ライブラリの範囲内ですでに実装されていました。べんり。</p>

<pre class="code lang-rust" data-lang="rust" data-unlink><span class="synStatement">let</span> result <span class="synStatement">=</span> <span class="synPreProc">std</span><span class="synSpecial">::</span><span class="synPreProc">io</span><span class="synSpecial">::</span><span class="synIdentifier">stdin</span>().<span class="synIdentifier">bytes</span>().<span class="synIdentifier">collect</span><span class="synSpecial">::</span><span class="synStatement">&lt;</span><span class="synType">Result</span><span class="synStatement">&lt;</span><span class="synType">Vec</span><span class="synStatement">&lt;</span>_<span class="synStatement">&gt;</span>, _<span class="synStatement">&gt;&gt;</span>();
</pre>

<p>これだけです。これで要件を満たす <code>Result&lt;Vec&lt;_&gt;, _&gt;</code> が返って来ます。すばらしい。</p>

<p>タネは簡単な話で <code>Result</code> が <code>FromIterator</code> trait を <code>impl</code> しているので <code>collect</code> で変換が可能であるというお話でした。
<a href="https://doc.rust-lang.org/stable/std/iter/trait.FromIterator.html">std::iter::FromIterator</a></p>

---

---
