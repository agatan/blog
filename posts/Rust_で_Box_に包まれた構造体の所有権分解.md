---
title: "Rust で Box に包まれた構造体の所有権分解"
date: 2016-12-04T07:45:12.000Z
tags: ["Rust"]
---

<p>ちょっとはまったのでメモ</p>

<pre class="code lang-rust" data-lang="rust" data-unlink><span class="synStatement">struct</span> <span class="synIdentifier">A</span> {
    foo: <span class="synType">Vec</span><span class="synStatement">&lt;</span><span class="synType">i32</span><span class="synStatement">&gt;</span>,
    bar: <span class="synType">Vec</span><span class="synStatement">&lt;</span><span class="synType">bool</span><span class="synStatement">&gt;</span>,
}
</pre>

<p>こんな構造体があったとする。
普通、<code>A</code> の所有権を分解して <code>foo</code> と <code>bar</code> にしたいときは</p>

<pre class="code lang-rust" data-lang="rust" data-unlink><span class="synStatement">fn</span> <span class="synIdentifier">xxx</span>(x: A) <span class="synStatement">-&gt;</span> (<span class="synType">Vec</span><span class="synStatement">&lt;</span><span class="synType">i32</span><span class="synStatement">&gt;</span>, <span class="synType">Vec</span><span class="synStatement">&lt;</span><span class="synType">bool</span><span class="synStatement">&gt;</span>) {
    <span class="synStatement">let</span> A { foo, bar } <span class="synStatement">=</span> x;
    (foo, bar)
}
</pre>

<p>とやれば良い（この例だともっと簡単に書ける気もするけど）</p>

<p>一方、<code>Box&lt;A&gt;</code> から <code>foo</code> と <code>bar</code> に分解したい場合は話が変わってくる。</p>

<pre class="code" data-lang="" data-unlink>fn error1(x: Box&lt;A&gt;) -&gt; (Vec&lt;i32&gt;, Vec&lt;bool&gt;) {
    (x.foo, x.bar)
}

fn error2(x: Box&lt;A&gt;) -&gt; (Vec&lt;i32&gt;, Vec&lt;bool&gt;) {
    let A { foo, bar } = *x;
    (foo, bar)
}</pre>

<p>これらは両方共<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D1%A5%A4%A5%EB">コンパイル</a>できない。
人間から見ると，<code>Box&lt;A&gt;</code> は <code>A</code> の所有権を持っているのだから、<code>A</code> -> <code>foo/bar</code> に分解できるなら <code>Box&lt;A&gt;</code> も同様にできる気がする。</p>

<p>実際にはこのようにすると<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D1%A5%A4%A5%EB">コンパイル</a>が通る。</p>

<pre class="code" data-lang="" data-unlink>fn success(x: Box&lt;A&gt;) -&gt; (Vec&lt;i32&gt;, Vec&lt;bool&gt;) {
    let x = *x;
    let A { foo, bar } = x;
    (foo, bar)
}</pre>

<p>うーん、エラーになるケースだと <code>Deref</code> トレイトの機能を経由している感じになるのかな？
<code>Deref</code> 経由で <code>foo</code> の所有権をとるとその時点で <code>Box&lt;A&gt;</code> の所有権は奪われちゃうから <code>bar</code> の所有権が取れないということなのだと想像した。
<code>success</code> のようなコードが突然出てきたら混乱しそうだ。</p>

---

---
