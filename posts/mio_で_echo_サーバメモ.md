---
title: "mio で echo サーバメモ"
date: 2017-01-07T06:14:55.000Z
tags: []
---
<p>Rust の非同期 IO ライブラリのなかでももっとも低レベルなレイヤーを担っている <a href="https://github.com/carllerche/mio">mio</a> を使ってecho サーバを書いた。
echo サーバばっかり書いているような気がするけど，echo サーバやっておくと簡単な割にライブラリの使い方とかがちゃんと分かる気がするので好きです。</p>

<h3>コード</h3>

<pre class="code lang-rust" data-lang="rust" data-unlink><span class="synStatement">extern</span> <span class="synStatement">crate</span> <span class="synIdentifier">mio</span>;

<span class="synStatement">use</span> <span class="synPreProc">std</span><span class="synSpecial">::</span><span class="synPreProc">io</span><span class="synSpecial">::</span><span class="synPreProc">prelude</span><span class="synSpecial">::</span><span class="synType">*</span>;
<span class="synStatement">use</span> <span class="synPreProc">std</span><span class="synSpecial">::</span><span class="synPreProc">collections</span><span class="synSpecial">::</span>HashMap;

<span class="synStatement">use</span> <span class="synPreProc">mio</span><span class="synSpecial">::</span><span class="synType">*</span>;
<span class="synStatement">use</span> <span class="synPreProc">mio</span><span class="synSpecial">::</span><span class="synPreProc">tcp</span><span class="synSpecial">::</span>{TcpListener, TcpStream};

<span class="synPreProc">#[derive(</span><span class="synType">Debug</span><span class="synPreProc">)]</span>
<span class="synStatement">struct</span> <span class="synIdentifier">ClientsHolder</span> {
    table: HashMap<span class="synStatement">&lt;</span>Token, TcpStream<span class="synStatement">&gt;</span>,
    free_token: <span class="synType">Vec</span><span class="synStatement">&lt;</span>Token<span class="synStatement">&gt;</span>,
    next_max_token: Token,
}

<span class="synStatement">impl</span> ClientsHolder {
    <span class="synStatement">fn</span> <span class="synIdentifier">new_from</span>(start_token: Token) <span class="synStatement">-&gt;</span> <span class="synType">Self</span> {
        ClientsHolder {
            table: <span class="synPreProc">HashMap</span><span class="synSpecial">::</span><span class="synIdentifier">new</span>(),
            free_token: <span class="synType">Vec</span><span class="synSpecial">::</span><span class="synIdentifier">new</span>(),
            next_max_token: start_token,
        }
    }

    <span class="synStatement">fn</span> <span class="synIdentifier">next_token</span>(<span class="synType">&amp;mut</span> <span class="synConstant">self</span>) <span class="synStatement">-&gt;</span> Token {
        <span class="synStatement">if</span> <span class="synStatement">let</span> <span class="synConstant">Some</span>(tok) <span class="synStatement">=</span> <span class="synConstant">self</span>.free_token.<span class="synIdentifier">pop</span>() {
            <span class="synStatement">return</span> tok;
        }
        <span class="synStatement">let</span> tok <span class="synStatement">=</span> <span class="synConstant">self</span>.next_max_token;
        <span class="synConstant">self</span>.next_max_token <span class="synStatement">=</span> <span class="synIdentifier">Token</span>(tok.<span class="synConstant">0</span> <span class="synStatement">+</span> <span class="synConstant">1</span>);
        tok
    }

    <span class="synStatement">fn</span> <span class="synIdentifier">register</span>(<span class="synType">&amp;mut</span> <span class="synConstant">self</span>, tok: Token, client: TcpStream) {
        <span class="synConstant">self</span>.table.<span class="synIdentifier">insert</span>(tok, client);
    }

    <span class="synStatement">fn</span> <span class="synIdentifier">get_mut</span>(<span class="synType">&amp;mut</span> <span class="synConstant">self</span>, tok: Token) <span class="synStatement">-&gt;</span> <span class="synType">Option</span><span class="synStatement">&lt;</span><span class="synType">&amp;mut</span> TcpStream<span class="synStatement">&gt;</span> {
        <span class="synConstant">self</span>.table.<span class="synIdentifier">get_mut</span>(<span class="synType">&amp;</span>tok)
    }

    <span class="synStatement">fn</span> <span class="synIdentifier">remove</span>(<span class="synType">&amp;mut</span> <span class="synConstant">self</span>, tok: Token) <span class="synStatement">-&gt;</span> <span class="synType">Option</span><span class="synStatement">&lt;</span>TcpStream<span class="synStatement">&gt;</span> {
        <span class="synStatement">let</span> result <span class="synStatement">=</span> <span class="synConstant">self</span>.table.<span class="synIdentifier">remove</span>(<span class="synType">&amp;</span>tok);
        <span class="synStatement">if</span> result.<span class="synIdentifier">is_some</span>() {
            <span class="synConstant">self</span>.free_token.<span class="synIdentifier">push</span>(tok);
        }
        result
    }
}

<span class="synComment">// Setup some tokens to allow us to identify which event is</span>
<span class="synComment">// for which socket.</span>
<span class="synType">const</span> SERVER: Token <span class="synStatement">=</span> <span class="synIdentifier">Token</span>(<span class="synConstant">0</span>);

<span class="synStatement">fn</span> <span class="synIdentifier">main</span>() {

    <span class="synStatement">let</span> addr <span class="synStatement">=</span> <span class="synConstant">&quot;127.0.0.1:13265&quot;</span>.<span class="synIdentifier">parse</span>().<span class="synIdentifier">unwrap</span>();

    <span class="synComment">// Setup the server socket</span>
    <span class="synStatement">let</span> server <span class="synStatement">=</span> <span class="synPreProc">TcpListener</span><span class="synSpecial">::</span><span class="synIdentifier">bind</span>(<span class="synType">&amp;</span>addr).<span class="synIdentifier">unwrap</span>();

    <span class="synComment">// Create an poll instance</span>
    <span class="synStatement">let</span> poll <span class="synStatement">=</span> <span class="synPreProc">Poll</span><span class="synSpecial">::</span><span class="synIdentifier">new</span>().<span class="synIdentifier">unwrap</span>();

    <span class="synComment">// Start listening for incoming connections</span>
    poll.<span class="synIdentifier">register</span>(<span class="synType">&amp;</span>server, SERVER, <span class="synPreProc">Ready</span><span class="synSpecial">::</span><span class="synIdentifier">readable</span>(), <span class="synPreProc">PollOpt</span><span class="synSpecial">::</span><span class="synIdentifier">edge</span>())
        .<span class="synIdentifier">unwrap</span>();

    <span class="synComment">// Create storage for events</span>
    <span class="synStatement">let</span> <span class="synType">mut</span> events <span class="synStatement">=</span> <span class="synPreProc">Events</span><span class="synSpecial">::</span><span class="synIdentifier">with_capacity</span>(<span class="synConstant">1024</span>);
    <span class="synStatement">let</span> <span class="synType">mut</span> clients <span class="synStatement">=</span> <span class="synPreProc">ClientsHolder</span><span class="synSpecial">::</span><span class="synIdentifier">new_from</span>(<span class="synIdentifier">Token</span>(<span class="synConstant">1</span>));

    <span class="synStatement">loop</span> {
        poll.<span class="synIdentifier">poll</span>(<span class="synType">&amp;mut</span> events, <span class="synConstant">None</span>).<span class="synIdentifier">unwrap</span>();

        <span class="synStatement">for</span> event <span class="synStatement">in</span> events.<span class="synIdentifier">iter</span>() {
            <span class="synStatement">match</span> event.<span class="synIdentifier">token</span>() {
                SERVER <span class="synStatement">=&gt;</span> {
                    <span class="synComment">// Accept and drop the socket immediately, this will close</span>
                    <span class="synComment">// the socket and notify the client of the EOF.</span>
                    <span class="synStatement">let</span> (stream, _) <span class="synStatement">=</span> server.<span class="synIdentifier">accept</span>().<span class="synIdentifier">unwrap</span>();
                    <span class="synStatement">let</span> tok <span class="synStatement">=</span> clients.<span class="synIdentifier">next_token</span>();
                    poll.<span class="synIdentifier">register</span>(<span class="synType">&amp;</span>stream, tok, <span class="synPreProc">Ready</span><span class="synSpecial">::</span><span class="synIdentifier">readable</span>(), <span class="synPreProc">PollOpt</span><span class="synSpecial">::</span><span class="synIdentifier">edge</span>()).<span class="synIdentifier">unwrap</span>();
                    clients.<span class="synIdentifier">register</span>(tok, stream);
                }
                tok <span class="synStatement">=&gt;</span> {
                    <span class="synStatement">let</span> <span class="synType">mut</span> close <span class="synStatement">=</span> <span class="synConstant">false</span>;
                    <span class="synStatement">if</span> <span class="synStatement">let</span> <span class="synConstant">Some</span>(<span class="synType">mut</span> stream) <span class="synStatement">=</span> clients.<span class="synIdentifier">get_mut</span>(tok) {
                        <span class="synStatement">let</span> <span class="synType">mut</span> buf <span class="synStatement">=</span> [<span class="synConstant">0</span>; <span class="synConstant">1024</span>];
                        <span class="synStatement">let</span> n <span class="synStatement">=</span> stream.<span class="synIdentifier">read</span>(<span class="synType">&amp;mut</span> buf).<span class="synIdentifier">unwrap</span>();
                        <span class="synStatement">if</span> n <span class="synStatement">==</span> <span class="synConstant">0</span> {
                            poll.<span class="synIdentifier">deregister</span>(stream).<span class="synIdentifier">unwrap</span>();
                            close <span class="synStatement">=</span> <span class="synConstant">true</span>;
                        } <span class="synStatement">else</span> {
                            stream.<span class="synIdentifier">write</span>(<span class="synType">&amp;</span>buf[<span class="synConstant">0</span>..n]).<span class="synIdentifier">unwrap</span>();
                        }
                    }
                    <span class="synStatement">if</span> close {
                        clients.<span class="synIdentifier">remove</span>(tok);
                    }
                }
            }
        }
    }
}
</pre>


<p>面倒だったので <code>unwrap</code> まみれですが。</p>

<h2>やったこと</h2>

<p><code>mio</code> の全体の流れとしては，<code>Poll</code> 型の値がイベントを監視する役割を担います．
<code>Poll</code> に監視対象を登録していき，<code>Poll::poll</code> でイベントの発火を待ちます．
発火したイベント一覧が <code>Events</code> 型の <code>Events::iter</code> で取れるので，対応していけばよいです．</p>

<p><code>mio</code> では <code>Token</code> という型の値を使って監視対象を識別しています．
監視対象には <code>TcpListener</code> ，<code>TcpStream</code>，<code>Sender</code>，などなどいろんなものがあるので，統一的に扱うために <code>Poll</code> は <code>Token</code> だけを保持します．
<code>Token</code> と監視対象の紐付けはユーザ側の責任でやってくれという感じみたいです．</p>

<p>echo サーバではクライアントの数は不特定なので，「空いている <code>Token</code> を探す」と「<code>Token</code> に対応するクライアント (<code>TcpStream</code>) を探す」がうまくできる必要があります．
そこで，<code>ClientsHolder</code> を定義しました．
こいつが，空いている <code>Token</code> を返すのと <code>Token</code> をキーに <code>TcpStream</code> を返す仕事をします．
<code>remove</code> されたらその <code>Token</code> は再利用します．</p>

<h2>気になるところ</h2>

<p><code>mio</code> はファイルに対する抽象化は提供しない方針のようです．
<code>STDIN</code> / <code>STDOUT</code> も同様です．</p>

<p>ファイル IO もノン<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%D6%A5%ED%A5%C3%A5%AD%A5%F3%A5%B0">ブロッキング</a>にしたい場合はどうしたらいいんでしょう？よくわかっていない．
<code>mio::unix</code> 以下に <a class="keyword" href="http://d.hatena.ne.jp/keyword/UNIX">UNIX</a> 限定の拡張がおいてあって，<code>EventedFd</code> という file descriptor を扱う実装はあるので，<a class="keyword" href="http://d.hatena.ne.jp/keyword/UNIX">UNIX</a> 限定なら力技でなんとかなるのかもしれない．</p>

<p>あと <code>mio</code> は関係ないんですが，実装の部分で，</p>

<pre class="code lang-rust" data-lang="rust" data-unlink><span class="synStatement">let</span> <span class="synType">mut</span> close <span class="synStatement">=</span> <span class="synConstant">false</span>;
<span class="synStatement">if</span> <span class="synStatement">let</span> <span class="synConstant">Some</span>(<span class="synType">mut</span> stream) <span class="synStatement">=</span> clients.<span class="synIdentifier">get_mut</span>(tok) {
    <span class="synStatement">let</span> <span class="synType">mut</span> buf <span class="synStatement">=</span> [<span class="synConstant">0</span>; <span class="synConstant">1024</span>];
    <span class="synStatement">let</span> n <span class="synStatement">=</span> stream.<span class="synIdentifier">read</span>(<span class="synType">&amp;mut</span> buf).<span class="synIdentifier">unwrap</span>();
    <span class="synStatement">if</span> n <span class="synStatement">==</span> <span class="synConstant">0</span> {
        poll.<span class="synIdentifier">deregister</span>(stream).<span class="synIdentifier">unwrap</span>();
        close <span class="synStatement">=</span> <span class="synConstant">true</span>;
    } <span class="synStatement">else</span> {
        stream.<span class="synIdentifier">write</span>(<span class="synType">&amp;</span>buf[<span class="synConstant">0</span>..n]).<span class="synIdentifier">unwrap</span>();
    }
}
<span class="synStatement">if</span> close {
    clients.<span class="synIdentifier">remove</span>(tok);
}
</pre>


<p>というのがあるんですが，これどうやったらスマートなんでしょう．
<code>close = true</code> としている部分で <code>clients.remove(tok);</code> をやるのが普通だと思うんですが，<code>if let Some(mut stream) = clients.get_mut(tok) {</code> のところで <code>clients</code> は borrow されているから mutable borrow はこれ以上作れないのです．</p>

-----
--------