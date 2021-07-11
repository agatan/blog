---
title: "yukicoder 2015/05/03"
date: 2015-05-04T09:38:47.000Z
tags: []
---

<p>今週は土日共に予定があって<a class="keyword" href="http://d.hatena.ne.jp/keyword/AtCoder">AtCoder</a>さんもyukicoderさんも出場したかったのですが出来ず...</p>

<h1><a class="keyword" href="http://d.hatena.ne.jp/keyword/AtCoder">AtCoder</a> - ARC</h1>

<p>今週の<a class="keyword" href="http://d.hatena.ne.jp/keyword/AtCoder">AtCoder</a>さんはABCはなかった？ぽくて, ARCに挑むにはまだちょっと力不足かなと思うので, 一旦保留します</p>

<h1>yukicoder</h1>

<p>というわけでyukicoderさんの方の最初の2問についてだけコンテスト後ですが挑戦してみましたー</p>

<h2>　<a href="http://yukicoder.me/problems/436">No.201 yukicoderじゃんけん - yukicoder</a></h2>

<p>ゆるふわなじゃんけんですね。じゃんけんといいつつ、手は全く関係ないただの数値比較ですｗｗ<br/>
ただし、注意しなければならないのが数値の範囲です。<code>10 ^ 1000</code> までという非常に大きな数字を扱う必要があるので、単純に実装すると落ちます。<br/>
今まであんまりこういった入力数値の範囲について注視していなかったのですが、今回ばっちりひっかかって落ちたので今度からはちゃんと見ないとダメですねｗｗ</p>

<p>あ、あと今回は<a class="keyword" href="http://d.hatena.ne.jp/keyword/C%2B%2B">C++</a>を使用してみましたー競技プログラミングとは別件で最近<a class="keyword" href="http://d.hatena.ne.jp/keyword/C%2B%2B">C++</a>をよく利用しているので、その流れでDから一旦変えてみましたー</p>

<p>非常に大きな数値を扱う場合、<a class="keyword" href="http://d.hatena.ne.jp/keyword/ruby">ruby</a>や<a class="keyword" href="http://d.hatena.ne.jp/keyword/python">python</a>であれば自動的に<a class="keyword" href="http://d.hatena.ne.jp/keyword/BigDecimal">BigDecimal</a>のようなクラスを使用してくれるので素直に実装すればそのまま通ってしまいますが、今回は<a class="keyword" href="http://d.hatena.ne.jp/keyword/C%2B%2B">C++</a>なので文字列として受け取って桁数を比較する方法で実装しました。</p>

<pre class="code lang-cpp" data-lang="cpp" data-unlink><span class="synPreProc">#include </span><span class="synConstant">&lt;iostream&gt;</span>
<span class="synPreProc">#include </span><span class="synConstant">&lt;string&gt;</span>
<span class="synPreProc">#include </span><span class="synConstant">&lt;vector&gt;</span>
<span class="synPreProc">#include </span><span class="synConstant">&lt;functional&gt;</span>
<span class="synPreProc">#include </span><span class="synConstant">&lt;algorithm&gt;</span>

<span class="synPreProc">#define REP(i,n) </span><span class="synStatement">for</span><span class="synPreProc">(</span><span class="synType">int</span><span class="synPreProc"> i;i&lt;(n);i++)</span>

<span class="synStatement">using</span> <span class="synType">namespace</span> std;

<span class="synType">int</span> main() {
    cin.tie(<span class="synConstant">0</span>);
    ios::sync_with_stdio(<span class="synConstant">false</span>);

    string a, ap, b, bp, g;
    cin &gt;&gt; a &gt;&gt; ap &gt;&gt; g &gt;&gt; b &gt;&gt; bp &gt;&gt; g;
    <span class="synStatement">if</span> (ap.length() &gt; bp.length())
        cout &lt;&lt; a;
    <span class="synStatement">else</span> <span class="synStatement">if</span> (ap.length() &lt; bp.length())
        cout &lt;&lt; b;
    <span class="synStatement">else</span> {
        <span class="synStatement">if</span> (ap &gt; bp)
            cout &lt;&lt; a;
        <span class="synStatement">else</span> <span class="synStatement">if</span> (ap &lt; bp)
            cout &lt;&lt; b;
        <span class="synStatement">else</span>
            cout &lt;&lt; -<span class="synConstant">1</span>;
    }
    cout &lt;&lt; endl;

    <span class="synStatement">return</span> <span class="synConstant">0</span>;
}
</pre>

<p><a class="keyword" href="http://d.hatena.ne.jp/keyword/C%2B%2B">C++</a>にもBigInt的なものはあるんですかね？あんまり<a class="keyword" href="http://d.hatena.ne.jp/keyword/C%2B%2B">C++</a>も詳しくないのでわかりませんが多分あるでしょう。どっちがはやいんだろうなー</p>

<h2><a href="http://yukicoder.me/problems/476">No.202 1円玉投げ - yukicoder</a></h2>

<p>1円玉を1つずつ投げていって重なったら取り除く、を繰り返した時最後に何枚残っているか、という問題ですね。<br/>
最初にとりあえず書いてみた解答がこちら。</p>

<pre class="code lang-cpp" data-lang="cpp" data-unlink><span class="synPreProc">#include </span><span class="synConstant">&lt;iostream&gt;</span>
<span class="synPreProc">#include </span><span class="synConstant">&lt;string&gt;</span>
<span class="synPreProc">#include </span><span class="synConstant">&lt;vector&gt;</span>
<span class="synPreProc">#include </span><span class="synConstant">&lt;functional&gt;</span>
<span class="synPreProc">#include </span><span class="synConstant">&lt;algorithm&gt;</span>
<span class="synPreProc">#include </span><span class="synConstant">&lt;cmath&gt;</span>

<span class="synPreProc">#define REP(i,n) </span><span class="synStatement">for</span><span class="synPreProc">(</span><span class="synType">int</span><span class="synPreProc"> i=</span><span class="synConstant">0</span><span class="synPreProc">;i&lt;(n);i++)</span>

<span class="synStatement">using</span> <span class="synType">namespace</span> std;

<span class="synType">bool</span> is_on(pair&lt;<span class="synType">int</span>,<span class="synType">int</span>&gt; &amp;a, pair&lt;<span class="synType">int</span>, <span class="synType">int</span>&gt; &amp;b)
{
    <span class="synType">double</span> dist = (a.first - b.first) * (a.first - b.first) + (a.second - b.second) * (a.second - b.second);
    dist = sqrt(dist);
    <span class="synStatement">return</span> dist &lt; <span class="synConstant">20</span>;
}

<span class="synType">int</span> main() {
    cin.tie(<span class="synConstant">0</span>);
    ios::sync_with_stdio(<span class="synConstant">false</span>);

    <span class="synType">int</span> N;
    cin &gt;&gt; N;
    <span class="synType">int</span> x, y;
    cin &gt;&gt; x &gt;&gt; y;
    N--;
    vector&lt;pair&lt;<span class="synType">int</span>, <span class="synType">int</span>&gt; &gt; coins;
    coins.emplace_back(x, y);

    REP(i, N) {
        cin &gt;&gt; x &gt;&gt; y;
        <span class="synType">auto</span> p = pair&lt;<span class="synType">int</span>,<span class="synType">int</span>&gt;(x, y);
        <span class="synStatement">if</span> (!any_of(coins.begin(), coins.end(), [&amp;p](pair&lt;<span class="synType">int</span>, <span class="synType">int</span>&gt; &amp;a) <span class="synError">{</span> <span class="synStatement">return</span> is_on(a, p); })) {
            coins.push_back(move(p));
        }
    }
    cout &lt;&lt; coins.size() &lt;&lt; endl;

    <span class="synStatement">return</span> <span class="synConstant">0</span>;
}
</pre>

<p>投げるたびに、今までのコイン達と重なっているかどうかをチェックし、どれとも重なっていなかった場合は追加する、というナイーブな実装です。これだと答えは合うのですが、TLEになってしまいました。<br/>
はじめはx軸方向にソートして、x軸方向で20より離れていればチェックの必要がないので、チェックの必要がある部分を二分探索で求めるという方法を考えたのですが、なんかあんまりうまい方法に思えなくて詰まりまくりました。</p>

<p>0 &lt;= x, y &lt;= 20000 というフィールドを 10 × 10 の細かいフィールドに区切ってチェックするという方法がスタンダードみたいですね！なるほど！ <br/>
というわけで実装してみたのがこちら</p>

<pre class="code lang-cpp" data-lang="cpp" data-unlink><span class="synPreProc">#include </span><span class="synConstant">&lt;iostream&gt;</span>
<span class="synPreProc">#include </span><span class="synConstant">&lt;string&gt;</span>
<span class="synPreProc">#include </span><span class="synConstant">&lt;vector&gt;</span>
<span class="synPreProc">#include </span><span class="synConstant">&lt;functional&gt;</span>
<span class="synPreProc">#include </span><span class="synConstant">&lt;algorithm&gt;</span>
<span class="synPreProc">#include </span><span class="synConstant">&lt;cmath&gt;</span>
<span class="synPreProc">#include </span><span class="synConstant">&lt;array&gt;</span>

<span class="synPreProc">#define REP(i, n) </span><span class="synStatement">for</span><span class="synPreProc">(</span><span class="synType">int</span><span class="synPreProc"> i=</span><span class="synConstant">0</span><span class="synPreProc">;i&lt;(n);i++)</span>

<span class="synStatement">using</span> <span class="synType">namespace</span> std;

<span class="synType">typedef</span> pair&lt;<span class="synType">int</span>, <span class="synType">int</span>&gt; P;

<span class="synType">const</span> <span class="synType">int</span> MAX_N = <span class="synConstant">100000</span>;
array&lt;array&lt;vector&lt;P&gt;, <span class="synConstant">2000</span>&gt;, <span class="synConstant">20000</span> / <span class="synConstant">10</span>&gt; fields;

<span class="synType">bool</span> is_on(pair&lt;<span class="synType">int</span>, <span class="synType">int</span>&gt; &amp;a, pair&lt;<span class="synType">int</span>, <span class="synType">int</span>&gt; &amp;b) {
    <span class="synType">double</span> dist = (a.first - b.first) * (a.first - b.first) + (a.second - b.second) * (a.second - b.second);
    dist = sqrt(dist);
    <span class="synStatement">return</span> dist &lt; <span class="synConstant">20</span>;
}

<span class="synType">int</span> main() {
    cin.tie(<span class="synConstant">0</span>);
    ios::sync_with_stdio(<span class="synConstant">false</span>);

    <span class="synType">int</span> N;
    cin &gt;&gt; N;
    <span class="synType">int</span> x, y;
    cin &gt;&gt; x &gt;&gt; y;
    N--;
    fields[x / <span class="synConstant">20</span>][y / <span class="synConstant">20</span>].emplace_back(x, y);

    <span class="synType">int</span> ans = <span class="synConstant">1</span>;

    REP(k, N) {
        cin &gt;&gt; x &gt;&gt; y;
        P current(x, y);
        <span class="synType">int</span> fx = x / <span class="synConstant">20</span>, fy = y / <span class="synConstant">20</span>;
        <span class="synType">bool</span> flg = <span class="synConstant">true</span>;
        <span class="synStatement">for</span> (<span class="synType">int</span> i = -<span class="synConstant">1</span>; i &lt;= <span class="synConstant">1</span> &amp;&amp; flg; ++i) {
            <span class="synStatement">for</span> (<span class="synType">int</span> j = -<span class="synConstant">1</span>; j &lt;= <span class="synConstant">1</span> &amp;&amp; flg; ++j) {
                <span class="synStatement">if</span> (fx + i &lt; <span class="synConstant">0</span> || fx + i &gt;= <span class="synConstant">2000</span> || fy + j &lt; <span class="synConstant">0</span> || fy + j &gt;= <span class="synConstant">2000</span>) <span class="synStatement">continue</span>;
                <span class="synStatement">for</span> (<span class="synType">auto</span> p: fields[fx + i][fy + j]) {
                    <span class="synStatement">if</span> (is_on(p, current)) {
                        flg = <span class="synConstant">false</span>;
                        <span class="synStatement">break</span>;
                    }
                }
            }
        }
        <span class="synStatement">if</span> (flg) {
            fields[fx][fy].push_back(current);
            ans++;
        }
    }
    cout &lt;&lt; ans &lt;&lt; endl;

    <span class="synStatement">return</span> <span class="synConstant">0</span>;
}
</pre>

<p>10 × 10 のマスに区切って、コインが重なる可能性のある部分(つまり隣接するマス)についてのみチェックをするという実装です。<br/>
今思ったのですが、10 × 10 のマスに区切ったらその中に存在できるコインの数って多分1子だけですよね？そしたら <code>array&lt; array&lt;P, 2000&gt;, 2000&gt;</code> でもよかったかもですね！(あ、でもそれだとコインが存在しないときの値がよくわからなくなるなーoption型とか欲しくなる)</p>

<h1>まとめ</h1>

<p>やっぱり出場したかったなーあのコンテストの感じがないと集中しきれないというかｗｗ<br/>
コンテストだと出来ない時にすごく悔しくて次回への勉強のモチベーションがあがるんですよね！だから今後極力出場していこうと思います！</p>

<p>学びとしては、きちんと問題の対象範囲をよく読むことと、検索の範囲を狭めることで解決できる問題の場合はフィールドをマス目状に区切る方法があるということですね！次回に活かします！</p>

---

---
