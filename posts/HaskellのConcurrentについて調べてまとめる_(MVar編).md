---
title: "HaskellのConcurrentについて調べてまとめる (MVar編)"
date: 2015-07-22T01:21:48.000Z
tags: []
---
<p>どうもこんにちは.</p>

<p>前回(<a href="http://agtn.hatenablog.com/entry/2015/07/21/234658">HaskellのConcurrentについて調べてまとめる (IORef編) - プログラミングのメモ帳➚</a>)の続きです.</p>

<p>今回はスレッド間協調のために<code>MVar</code>を使う方法について調べたので, まとめたいと思います.</p>

<h2>MVar</h2>

<p><a class="keyword" href="http://d.hatena.ne.jp/keyword/Haskell">Haskell</a>にかかわらず, 最近の並行処理はメッセージパッシングでやれみたいなのが流行ってますね (<a class="keyword" href="http://d.hatena.ne.jp/keyword/Scala">Scala</a>のAkkaや<a class="keyword" href="http://d.hatena.ne.jp/keyword/golang">golang</a>のchanなど).<br/>
<code>MVar</code>は<a class="keyword" href="http://d.hatena.ne.jp/keyword/Haskell">Haskell</a>における, 容量1のメッセージボックスのようなものです. <code>MVar</code>を使うことで, スレッド間でメッセージのやり取りを協調的に行うことができます.<br/>
<a class="keyword" href="http://d.hatena.ne.jp/keyword/%CA%A3%BF%F4">複数</a>のスレッドが１つの<code>MVar</code>に対して, メッセージを入れたり取り出したりすることでスレッド間協調を行います.</p>

<p>基本となる<a class="keyword" href="http://d.hatena.ne.jp/keyword/API">API</a>はこのような感じ</p>

<pre class="code" data-lang="" data-unlink>newEmptyMVar :: IO (MVar a)
newMVar :: a -&gt; IO (MVar a)
takeMVar :: MVar a -&gt; IO a
putMVar :: MVar a -&gt; a -&gt; IO ()
readMVar :: MVar a -&gt; IO a</pre>


<p>型を見ればなんとなく使い方もわかる気がしますね.<br/>
<code>MVar</code>を作るには<code>newEmptyMVar</code>か<code>newMVar</code>を使用します. <code>newEmptyMVar</code>は空のメッセージボックスを作り, <code>newMVar</code>は第一引数を初期値としてもつメッセージボックスを作ります.</p>

<p><code>MVar</code>にメッセージを格納するには, <code>putMVar</code>を使います. <code>putMVar mvar msg</code> で, <code>msg</code>を<code>mvar</code>に格納します.<br/>
この際, もし<code>MVar</code>にすでにメッセージが格納されている場合, <code>MVar</code>は容量1のボックスなので, <code>putMVar</code>がブロックされます. 他のスレッドが<code>MVar</code>からメッセージを取り出して空にするまで待ってから, メッセージを格納します.</p>

<p>一方, <code>MVar</code>からメッセージを読み取るには, <code>takeMVar</code>か<code>readMVar</code>を使用します.<br/>
<code>takeMVar</code>はメッセージを読み取り, その<code>MVar</code>を空にします. <code>readMVar</code>はメッセージを読み取りますが, <code>MVar</code>の中のメッセージはそのまま残します.<br/>
ここで, <code>put</code>の時と同様に, <code>takeMVar</code>も<code>readMVar</code>も<code>MVar</code>にメッセージが格納されていなかった場合, 他のスレッドが<code>MVar</code>にメッセージを格納するまでブロックします.</p>

<p>というわけで簡単なサンプルコード</p>

<pre class="code" data-lang="" data-unlink>module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar

main :: IO ()
main = do
    mvar &lt;- newEmptyMVar
    forkIO $ do
        msg &lt;- takeMVar mvar
        putStrLn $ &#34;recv: &#34; ++ msg
        threadDelay $ 1 * 10 ^ 6
        putMVar mvar &#34;B&#34;
    putStrLn &#34;sleep 1&#34;
    threadDelay $ 1 * 10 ^ 6
    putStrLn &#34;wake up&#34;
    putMVar mvar &#34;A&#34;
    takeMVar mvar &gt;&gt;= print</pre>


<p>実行結果</p>

<pre class="code" data-lang="" data-unlink>sleep 1
wake up
recv: A
&#34;B&#34;</pre>


<p>確かにメッセージが格納されるまで <code>takeMVar</code>がブロックしていることがわかります</p>

<h2>共有変数としてのMVar</h2>

<p>さて, <code>MVar</code>にはもうひとつの使い方があります. 共有変数としての<code>MVar</code>です.</p>

<p><code>MVar</code>の特徴として, 誰かが<code>take</code>してから<code>put</code>するまでの間は, 他のスレッドはだれも<code>MVar</code>の中身に触れないという点が挙げられます.</p>

<pre class="code" data-lang="" data-unlink>main = do
    mvar &lt;- newMVar 0
    forkIO $ do
        val &lt;- takeMVar mvar
        -- 他のスレッドはMVarの中身に触れない
        putMVar mvar $ val + 1
    ...</pre>


<p>この特徴はまさにロックの特徴といえます. ロックを取得し解放するまでは, 他のスレッドは同じロックで保護された区間にははいれません.<br/>
というわけで<code>MVar</code>は型レベルでロックがついた共有変数とみなすことができますね！(このへんはRustのMutexに似た空気を感じます. どちらも型レベルでロックとそれが保護する中身がつながっています)<br/>
型レベルでロックがくっついているので, 中身にアクセスするには必ずロックをとる(<code>takeMVar</code>)必要があり, ロックの取得忘れがありません.</p>

<p>さらに, <a class="keyword" href="http://d.hatena.ne.jp/keyword/Haskell">Haskell</a>は基本的に破壊的操作があまり登場しない言語であることもこの<code>MVar</code>ロックにプラスに働きます.</p>

<p>例えば, <a class="keyword" href="http://d.hatena.ne.jp/keyword/%CF%A2%C1%DB%C7%DB%CE%F3">連想配列</a>をスレッド間で共有することを考えます. また, ここでは<a class="keyword" href="http://d.hatena.ne.jp/keyword/%CF%A2%C1%DB%C7%DB%CE%F3">連想配列</a>の実装として, hashtableではなく<code>Data.Map</code>を使用するとします(<code>Data.Map</code>はimmutableな構造になっていて, lookupはO(log n)ですが, immutableなので<a class="keyword" href="http://d.hatena.ne.jp/keyword/Haskell">Haskell</a>上で扱いやすいというメリットがあります).</p>

<p><code>Data.Map</code>はimmutableなので, 一度<code>MVar</code>から取得してしまえばそれ以降変更される可能性もないため, ロックを保持し続ける必要がありません. そこで, 単なる読み込みの場合は, <code>takeMVar</code>してすぐに<code>putMVar</code>で戻すだとか, <code>readMVar</code>で読み込むだけにすることで, ロックの粒度を小さくできます.<br/>
<code>MVar</code>の中身を書き換えたい場合は, 単純にロックを取得し, 書き換え後の値を<code>putMVar</code>します.</p>

<pre class="code" data-lang="" data-unlink>module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import qualified Data.Map.Strict as M

main :: IO ()
main = do
    mvar &lt;- newMVar M.empty
    forkIO $ do
        table &lt;- takeMVar mvar
        putMVar mvar table
        -- tableを使用する操作
    forkIO $ do
         table &lt;- readMVar mvar
         -- tableを使用する操作
    forkIO $ do
        table &lt;- takeMVar mvar
        -- tableを変更する操作
        let newTable = ...
        putMVar mvar newTable</pre>


<p>このように<code>MVar</code>とimmutableなデータ構造を組み合わせることで, 粒度の小さいロックを実現することができます.<br/>
一方, <code>MVar</code>とmutableなデータ構造(<code>IORef</code>など)を組み合わせる場合は, たとえ読み込みしかしない場合であっても操作が終わるまではロックを保持しておく必要があることに注意しなければなりません (<code>IORef</code>には前回紹介したように<code>atomicModifyIORef</code>があるのでなかなかこういう状況は起こりませんね)</p>

<p>また, RustのMutexと違い, <code>MVar</code>によるロックの模倣(?)はロックの解放を自動的には行いません. したがって例外が送出された場合にロックを開放し忘れるケースがあるので, 注意が必要です.</p>

<h2>一旦まとめ</h2>

<p>というわけで今回は<code>MVar</code>について紹介しました. <code>MVar</code>でロックを実現する方に関しては, 散々言われているロックの問題点をそのまま持ってきてしまうのであまり使えないかもしれませんね...<br/>
<code>MVar</code>は容量1のメッセージボックスでしたが, <a class="keyword" href="http://d.hatena.ne.jp/keyword/Haskell">Haskell</a>には<code>Chan</code>というものもあります. こちらは<a class="keyword" href="http://d.hatena.ne.jp/keyword/golang">golang</a>のchanにかなり近いもので, 容量の制限がないキューのように働かせることができます. <code>Chan</code>のよみとり専用のスレッドを１つ立てておき, 他の<a class="keyword" href="http://d.hatena.ne.jp/keyword/%CA%A3%BF%F4">複数</a>のスレッドがタスクを<code>Chan</code>に書き込んでいくといった<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%E6%A1%BC%A5%B9%A5%B1%A1%BC%A5%B9">ユースケース</a>が考えられますね. こっちのほうが便利そうな気がしてきました.</p>

<p>ロックはいろいろ厄介で, <a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%C7%A5%C3%A5%C9%A5%ED%A5%C3%A5%AF">デッドロック</a>とか解放忘れとかの問題がついて回ります. それを解決する１つの方法として<code>STM</code>があるようなので, 次はそれについて調べてみようと思います.</p>

-----
--------