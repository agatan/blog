---
title: "Golangでechoサーバ"
date: 2015-09-08T07:14:10.000Z
tags: []
---

<p>最近 <a class="keyword" href="http://d.hatena.ne.jp/keyword/golang">golang</a> が気になります<br/>
<a class="keyword" href="http://d.hatena.ne.jp/keyword/golang">golang</a> の特徴はもはやわざわざここに書くまでも無いことだと思うので書きませんが, 気になっている理由を書いてみます.</p>

<ul>
<li>バイナリ(しかもポータビリティが非常に高いバイナリ)に<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%D1%A5%A4%A5%EB">コンパイル</a>されること</li>
<li><a class="keyword" href="http://d.hatena.ne.jp/keyword/C/C%2B%2B">C/C++</a> には及ばずとも実行が非常に速いこと</li>
<li><a class="keyword" href="http://d.hatena.ne.jp/keyword/C/C%2B%2B">C/C++</a> ほど低レイヤーいじり放題なわけではないが, ある程度低レイヤーまで降りていけること</li>
<li><a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%DE%A5%F3%A5%C9%A5%E9%A5%A4%A5%F3">コマンドライン</a><a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%C4%A1%BC%A5%EB">ツール</a>からWeb アプリケーションのような高レイヤーまで十分得意であること</li>
<li><code>interface</code> による抽象化が, 過度でなく調度良く感じられること</li>
</ul>

<p>こんな感じでしょうか.<br/>
Compiled <a class="keyword" href="http://d.hatena.ne.jp/keyword/Python">Python</a> っていう感じが非常に良さそうだなーと思っています.</p>

<p>というわけで<a class="keyword" href="http://d.hatena.ne.jp/keyword/Haskell">Haskell</a>に引き続き <a class="keyword" href="http://d.hatena.ne.jp/keyword/golang">golang</a> でも echo サーバを書いてみました</p>

<pre class="code lang-go" data-lang="go" data-unlink><span class="synStatement">package</span> main

<span class="synStatement">import</span> (
    <span class="synConstant">&quot;fmt&quot;</span>
    <span class="synConstant">&quot;io&quot;</span>
    <span class="synConstant">&quot;net&quot;</span>
)

<span class="synStatement">func</span> main() {
    listener, err := net.Listen(<span class="synConstant">&quot;tcp&quot;</span>, <span class="synConstant">&quot;:8080&quot;</span>)
    <span class="synStatement">if</span> err != <span class="synStatement">nil</span> {
        <span class="synStatement">panic</span>(err)
    }
    <span class="synStatement">for</span> {
        conn, err := listener.Accept()
        <span class="synStatement">if</span> err != <span class="synStatement">nil</span> {
            <span class="synStatement">panic</span>(err)
        }
        <span class="synStatement">go</span> <span class="synType">func</span>(conn net.Conn) {
            <span class="synStatement">defer</span> conn.Close()
            echo(conn)
        }(conn)
    }
}

<span class="synStatement">func</span> echo(conn net.Conn) {
    buf := <span class="synStatement">make</span>([]<span class="synType">byte</span>, <span class="synConstant">256</span>)
    <span class="synStatement">for</span> {
        n, err := conn.Read(buf)
        <span class="synStatement">if</span> err != <span class="synStatement">nil</span> {
            <span class="synStatement">if</span> err == io.EOF {
                <span class="synStatement">break</span>
            }
            <span class="synStatement">panic</span>(err)
        }
        <span class="synStatement">if</span> n == <span class="synConstant">0</span> {
            <span class="synStatement">break</span>
        }
        wn, err := conn.Write(buf[<span class="synConstant">0</span>:n])
        <span class="synStatement">if</span> err != <span class="synStatement">nil</span> {
            <span class="synStatement">panic</span>(err)
        }
        <span class="synStatement">if</span> wn != n {
            <span class="synStatement">panic</span>(fmt.Errorf(<span class="synConstant">&quot;could not send all data&quot;</span>))
        }
    }
}
</pre>

<p>さすがは <a class="keyword" href="http://d.hatena.ne.jp/keyword/golang">golang</a> というかなんというか. ものすごく普通な空気を感じますね.<br/>
<a class="keyword" href="http://d.hatena.ne.jp/keyword/golang">golang</a>はこういう普通さが売りの１つだと思っています.</p>

---

---
