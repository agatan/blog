---
title: "Haskellでechoサーバ"
date: 2015-07-23T12:43:44.000Z
tags: []
---
<p>はいどうもー<br/>
引き続き<a class="keyword" href="http://d.hatena.ne.jp/keyword/Haskell">Haskell</a>の話題です. ちょっと<a class="keyword" href="http://d.hatena.ne.jp/keyword/Haskell">Haskell</a>で<a class="keyword" href="http://d.hatena.ne.jp/keyword/TCP">TCP</a>ソケットを使ってみたくなったので, まず簡単なものから実装してみます.</p>

<p><a class="keyword" href="http://d.hatena.ne.jp/keyword/TCP">TCP</a>ソケットの<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%C1%A5%E5%A1%BC%A5%C8%A5%EA%A5%A2%A5%EB">チュートリアル</a>といえばechoサーバですね！クライアントからの入力をそのまま返すサーバです.<br/>
せっかくなのできちんと<a class="keyword" href="http://d.hatena.ne.jp/keyword/%CA%A3%BF%F4">複数</a>クライアントとの同時通信を可能にしましょう.</p>

<h1>Network.Socket</h1>

<p><a class="keyword" href="http://d.hatena.ne.jp/keyword/Haskell">Haskell</a>で<a class="keyword" href="http://d.hatena.ne.jp/keyword/TCP">TCP</a>ソケットを使うには, <code>Network.Socket</code>を使うようです. <a href="http://hackage.haskell.org/package/network-2.6.2.1/docs/Network-Socket.html">Network.Socket</a></p>

<p>ドキュメントには, 低レベル<a class="keyword" href="http://d.hatena.ne.jp/keyword/API">API</a>が<code>Network.Socket</code>で, 高レベル<a class="keyword" href="http://d.hatena.ne.jp/keyword/API">API</a>が<code>Network</code>と書いてあるのですが, <code>Network</code>モジュールのドキュメントには, 互換性のために残してあるけどこれから使う人は<code>Network.Socket</code>を使ってくれみたいなことが書いてあります.<br/>
適当に<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B0%A5%B0%A4%EB">ググる</a>と<code>Network</code>モジュールを使ったサンプルが散見されますが, ここはドキュメントにしたがって, <code>Network.Socket</code>を使用することにします.</p>

<h1>ソケットの用意</h1>

<p><a class="keyword" href="http://d.hatena.ne.jp/keyword/TCP">TCP</a>のサーバ側は, ソケットの作成 -> ソケットをポート番号指定でbind -> 接続受付を開始(listen) -> 接続を受け付ける(accept) というステップを踏む必要があります.<br/>
というわけでまずは指定したポート番号にbindされたソケットを用意するアクションを定義します.</p>

<pre class="code lang-haskell" data-lang="haskell" data-unlink><span class="synPreProc">import</span> Network.Socket

serveSocket <span class="synStatement">::</span> PortNumber <span class="synStatement">-&gt;</span> Socket
serveSocket port <span class="synStatement">=</span> <span class="synStatement">do</span>
    soc <span class="synStatement">&lt;-</span> socket AF_INET Stream defaultProtocol
    addr <span class="synStatement">&lt;-</span> inet_addr <span class="synConstant">&quot;0.0.0.0&quot;</span>
    bind soc (SockAddrInet port addr)
    return soc
</pre>


<p>これで引数に渡したポート番号にbindされたソケットが作成されます.</p>

<h2>accept</h2>

<p><a class="keyword" href="http://d.hatena.ne.jp/keyword/%CA%A3%BF%F4">複数</a>クライアントとの同時通信を実現するためには, <code>Control.Concurrent</code>のちからを借ります.<br/>
今回は <code>forkIO</code> を使って, 各コネクションごとにスレッドを起動していくことにします. (非同期版も作れるのかな？つくれたらつくります)</p>

<p>というわけで次は <code>accept</code>して<code>forkIO</code>するという処理を繰り返し行うアクションを定義します.<br/>
<code>forkIO</code> した後に実行するアクション(<code>echoLoop</code>)についてはとりあえず<code>undefined</code>とします. <br/>
<a class="keyword" href="http://d.hatena.ne.jp/keyword/Haskell">Haskell</a>の<code>undefined</code>, とても便利ですね. 型で考えるっていうスタイルが実行しやすくなっているのは, <a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%BD%A1%BC%A5%B9%A5%B3%A1%BC%A5%C9">ソースコード</a>上にトッ<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%D7%A5%EC%A5%D9">プレベ</a>ル関数の型指定を書きやすい<a class="keyword" href="http://d.hatena.ne.jp/keyword/Haskell">Haskell</a>の文法と<code>undefined</code>のおかげって感じがします.</p>

<pre class="code lang-haskell" data-lang="haskell" data-unlink>echoLoop <span class="synStatement">::</span> Socket <span class="synStatement">-&gt;</span> IO ()
echoLoop <span class="synStatement">=</span> undefined

<span class="synComment">-- import Control.Concurrent</span>
<span class="synComment">-- import Control.Monad</span>
<span class="synComment">-- が必要</span>
acceptLoop <span class="synStatement">::</span> Socket <span class="synStatement">-&gt;</span> IO ()
acceptLoop soc <span class="synStatement">=</span> forever <span class="synStatement">$</span> <span class="synStatement">do</span>
    (conn, _addr) <span class="synStatement">&lt;-</span> accept soc
    forkIO <span class="synStatement">$</span> echoLoop conn
</pre>


<p><code>forever :: Monad m =&gt; m a -&gt; m b</code>は引数にIOアクションを受け取り, それを無限に繰り返し実行し続けます. (無限にくりかえすので返り値の型変数<code>b</code>は不定)<br/>
<code>forever</code>の引数には, <code>accept</code>して<code>forkIO</code>するアクションを渡しています.</p>

<h1>echo</h1>

<p>最後にソケットから読み込み, そのまま書き出す<code>echoLoop</code>部分を作ります.</p>

<pre class="code lang-haskell" data-lang="haskell" data-unlink><span class="synComment">-- import Control.Exception が必要</span>
echoLoop <span class="synStatement">::</span> Socket <span class="synStatement">-&gt;</span> IO ()
echoLoop conn <span class="synStatement">=</span> <span class="synStatement">do</span>
    sequence_ <span class="synStatement">$</span> repeat <span class="synStatement">$</span> <span class="synStatement">do</span>
        (str, _, _) <span class="synStatement">&lt;-</span> recvFrom soc <span class="synConstant">64</span>
        send soc str
</pre>


<p><code>recvFrom :: Socket -&gt; Int -&gt; IO (String, Int, SockAddr)</code>は, <code>recvFrom conn n</code> で, <code>conn</code>から最大で<code>n</code>文字まで読み込みます.
返り値は, <code>(読み込んだ文字列, 読み込んだ文字数, 読み込み元のアドレス??)</code> を返します.<br/>
そして, <code>send :: Socket -&gt; String -&gt; IO ()</code> でソケットに読み込んだ文字列をそのまま書き込みます.<br/>
このように, 読み込んでそのままｍ書き込むというアクションを <code>repeat</code>でつなげています. <code>repeat :: a -&gt; [a]</code>は無限リストを作る関数です. <code>repeat 0</code>で<code>[0, 0, 0, 0, ...</code>というリストが作成されます. <br/>
このままでは <code>[IO ()]</code>型なので, これを<code>sequence_ :: Monad m =&gt; [m a] -&gt; m ()</code>を使って１つのIOアクションにまとめ上げます.<br/>
<a class="keyword" href="http://d.hatena.ne.jp/keyword/Haskell">Haskell</a>が遅延評価だから出来る芸当ですね. 無限に繰り返す感あふれるコードになっている気がします. (<code>forever</code>使ったほうがいいと思います)</p>

<h1>例外処理</h1>

<p><code>recvFrom</code>は相手側がコネクションを切断すると<code>End of file</code>の例外を投げます. <code>forkIO</code>しているので, １つのスレッドが例外で落ちてもサーバ全体は動き続けますが, ソケットのクローズも出来ませんし, 標準エラーになんかでてきてよろしくないので修正します.</p>

<pre class="code lang-haskell" data-lang="haskell" data-unlink><span class="synComment">-- import Control.Exceptionが必要</span>
echoLoop <span class="synStatement">::</span> Socket <span class="synStatement">-&gt;</span> IO ()
echoLoop conn <span class="synStatement">=</span> <span class="synStatement">do</span>
   sequence_ <span class="synStatement">$</span> repeat <span class="synStatement">$</span> <span class="synStatement">do</span>
       (str, _, _) <span class="synStatement">&lt;-</span> recvFrom conn <span class="synConstant">64</span>
       send conn str
   <span class="synStatement">`catch`</span> (<span class="synStatement">\</span>(SomeException e) <span class="synStatement">-&gt;</span> return ())
   <span class="synStatement">`finally`</span> close conn
</pre>


<p><code>catch</code>と<code>finally</code>を追加しています.<br/>
どちらも<a class="keyword" href="http://d.hatena.ne.jp/keyword/Java">Java</a>とかのそれと同じように動きます.<br/>
<code>SomeException</code>はすべての例外を補足することが出来ますが, ほんとはあんまり良くないですね. ここではEOFに達した(コネクションが切断された)という場合だけを補足したいので. (どの関数がどういう場合にどんな例外を投げるのかっていうドキュメントがわからなかったのでこのままにしておきました)<br/>
そして, 例外が発生してもしなくても, 最後にかならずソケットのクローズをするよう<code>finally</code>を使います.</p>

<p><code>SomeException</code>ですべての例外が捕捉出来るのって不思議じゃないですか？<a class="keyword" href="http://d.hatena.ne.jp/keyword/Haskell">Haskell</a>には<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%AA%A5%D6%A5%B8%A5%A7%A5%AF%A5%C8%BB%D8%B8%FE">オブジェクト指向</a>っぽい型の階層関係なんてないのに.
<a href="http://d.hatena.ne.jp/kazu-yamamoto/20081024/1224819961">Haskellの多相性 - あどけない話</a>このへんが関係しているっぽいなという感じがしますが詳しいことはよくわかりませんでした...</p>

<h1>全体</h1>

<pre class="code lang-haskell" data-lang="haskell" data-unlink><span class="synType">module</span> Main <span class="synType">where</span>

<span class="synPreProc">import</span> Network.Socket
<span class="synPreProc">import</span> Control.Monad
<span class="synPreProc">import</span> Control.Concurrent
<span class="synPreProc">import</span> Control.Exception

main <span class="synStatement">::</span> IO ()
main <span class="synStatement">=</span> <span class="synStatement">do</span>
    soc <span class="synStatement">&lt;-</span> serveSocket <span class="synConstant">8080</span>
    listen soc <span class="synConstant">5</span>
    acceptLoop soc <span class="synStatement">`finally`</span> close soc

serveSocket <span class="synStatement">::</span> PortNumber <span class="synStatement">-&gt;</span> IO Socket
serveSocket port <span class="synStatement">=</span> <span class="synStatement">do</span>
    soc <span class="synStatement">&lt;-</span> socket AF_INET Stream defaultProtocol
    addr <span class="synStatement">&lt;-</span> inet_addr <span class="synConstant">&quot;0.0.0.0&quot;</span>
    bind soc (SockAddrInet port addr)
    return soc

acceptLoop <span class="synStatement">::</span> Socket <span class="synStatement">-&gt;</span> IO ()
acceptLoop soc <span class="synStatement">=</span> forever <span class="synStatement">$</span> <span class="synStatement">do</span>
    (conn, addr) <span class="synStatement">&lt;-</span> accept soc
    forkIO <span class="synStatement">$</span> echoLoop conn

echoLoop <span class="synStatement">::</span> Socket <span class="synStatement">-&gt;</span> IO ()
echoLoop conn <span class="synStatement">=</span> <span class="synStatement">do</span>
    sequence_ <span class="synStatement">$</span> repeat <span class="synStatement">$</span> <span class="synStatement">do</span>
      (str, _, _) <span class="synStatement">&lt;-</span> recvFrom conn <span class="synConstant">64</span>
      send conn str
    <span class="synStatement">`catch`</span> (<span class="synStatement">\</span>(SomeException e) <span class="synStatement">-&gt;</span> return ())
    <span class="synStatement">`finally`</span> close conn
</pre>


<p><code>main</code>内で <code>listen</code>するのを忘れずに！また, <code>acceptLoop</code>中に例外が発生してもソケットをクローズするように<code>finally</code>を使っています. (まぁプログラム終了するのでいらない気もします)</p>

<h1>動作確認</h1>

<p><code>telnet</code>コマンドでテストします.</p>

<pre class="code" data-lang="" data-unlink>% telnet localhost 8080
Trying 127.0.0.1...
Connected to localhost.
Escape character is &#39;^]&#39;.
test
test
aaa
aaa
hooooogle
hooooogle</pre>


<p>ちょっとわかりづらいですが, 入力した文字列が即座にそのまま帰ってきていることがわかります. バッファリングの関係で, 一行ずつになっていますが.</p>

<h1>まとめ</h1>

<p><a class="keyword" href="http://d.hatena.ne.jp/keyword/Haskell">Haskell</a>でechoサーバ, 意外とすんなりかけましたね. 例外関係があまりよく理解できていない感じがしますが...<br/>
非同期版が気になります. 調べてみます.</p>

-----
--------