---
title: "HaskellのConcurrentについて調べてまとめる (IORef編)"
date: 2015-07-21T14:46:58.000Z
tags: []
---
<p>こんばんは. <a class="keyword" href="http://d.hatena.ne.jp/keyword/Haskell">Haskell</a>(<a class="keyword" href="http://d.hatena.ne.jp/keyword/GHC">GHC</a>)で並行処理を必要とするアプリケーションを書いてみようと思ったのですが, 並列処理に関するいろいろについてよくわかっていない部分が多かったので, 調べたついでにまとめておこうと思います.</p>

<p>もし間違い等ありましたらコメントいただけるとありがたいです</p>

<h2>Concurrent v.s. Parallel</h2>

<p>Concurrentは並行, Parallelは並列と訳されます.<br/>
Concurrentは論理的に同時に実行されることで, 実際に<a class="keyword" href="http://d.hatena.ne.jp/keyword/%CA%A3%BF%F4">複数</a>のタスクが物理的に同時に実行している必要はありません. 実際どうであれ, 同時に実行しているように見えればOKで, <a class="keyword" href="http://d.hatena.ne.jp/keyword/%CA%A3%BF%F4">複数</a>のタスクでCPUを細かく交代で使用しながら実行していくといった実行モデルもConcurrentであるといえます.<br/>
Parallelは物理的に同時に実行されることです. 必然的に<a class="keyword" href="http://d.hatena.ne.jp/keyword/%CA%A3%BF%F4">複数</a>のCPUが必要になります. 物理的に同時に実行されているタスクは, 論理的にも同時に実行しているとみなせるので, ParallelであればConcurrentです.</p>

<p>この記事ではConcurrentについて言及しているつもりです.</p>

<h2><a class="keyword" href="http://d.hatena.ne.jp/keyword/Haskell">Haskell</a>のスレッド</h2>

<p><a class="keyword" href="http://d.hatena.ne.jp/keyword/Haskell">Haskell</a>は処理系実装の軽量スレッドを持ちます. OSが提供するネイティブスレッドと違い, <a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%F3%A5%C6%A5%AD%A5%B9%A5%C8%A5%B9%A5%A4%A5%C3%A5%C1">コンテキストスイッチ</a>(スレッドの切り替え)のオーバーヘッドが少なく, より気軽に扱えるスレッドのようです. 軽量スレッドといえば <a class="keyword" href="http://d.hatena.ne.jp/keyword/Erlang">Erlang</a> や <a class="keyword" href="http://d.hatena.ne.jp/keyword/Golang">Golang</a> が思い浮かびますね.(<a class="keyword" href="http://d.hatena.ne.jp/keyword/Erlang">Erlang</a>は軽量プロセスっていうんでしたっけ)</p>

<p>実際に<a class="keyword" href="http://d.hatena.ne.jp/keyword/Haskell">Haskell</a>で軽量スレッドを立ち上げてみます.  <br/>
まずはスレッドを立ち上げない場合です. (threadDelayは指定したマイクロ秒分スレッドをスリープします)</p>

<pre class="code" data-lang="" data-unlink>module Main where

import Control.Concurrent (threadDelay)

sleepN :: Int -&gt; IO ()
sleepN n = do
    putStrLn $ &#34;sleep &#34; ++ show n
    threadDelay $ n * 10 ^ 6
    putStrLn $ &#34;wake up &#34; ++ show n

main :: IO ()
main = do
    sleepN 3
    threadDelay $ 2 * 10 ^ 6
    putStrLn &#34;sleep 2 and wakeup&#34;
    threadDelay $ 2 * 10 ^ 6
    putStrLn &#34;end&#34;</pre>


<p>実行結果</p>

<pre class="code" data-lang="" data-unlink>sleep 3
wake up 3
sleep 2 and wakeup
end</pre>


<p>全体として, 3秒->2秒->2秒とスリープするので7秒ほどの実行時間になります.</p>

<p>つぎにスレッドを立ち上げる場合です<br/>
<a class="keyword" href="http://d.hatena.ne.jp/keyword/Haskell">Haskell</a>でスレッドを立ち上げるには <code>forkIO :: IO () -&gt; IO ThreadId</code> を使用します. <code>IO ()</code>を渡すと, それを新しく立ち上げたスレッド上で実行してくれます.<br/>
(<code>forkOS :: IO () -&gt; IO ThreadId</code> というものもありますが, こちらは<a class="keyword" href="http://d.hatena.ne.jp/keyword/Haskell">Haskell</a>の軽量スレッドではなく, ネイティブスレッドを立ち上げます)</p>

<pre class="code" data-lang="" data-unlink>module Main where

import Control.Concurrent (forkIO, threadDelay)

sleepN :: Int -&gt; IO ()
sleepN n = do
    putStrLn $ &#34;sleep &#34; ++ show n
    threadDelay $ n * 10 ^ 6
    putStrLn $ &#34;wake up &#34; ++ show n

main :: IO ()
main = do
    forkIO $ sleepN 3
    threadDelay $ 2 * 10 ^ 6
    putStrLn &#34;sleep 2 and wakeup&#34;
    threadDelay $ 2 * 10 ^ 6
    putStrLn &#34;end&#34;</pre>


<p>実行結果</p>

<pre class="code" data-lang="" data-unlink>sleep 3
sleep 2 and wakeup
wake up 3
end</pre>


<p>１つのスレッドが3秒スリープしている間に, もう一つのスレッドのスリープが始まるので, 全体で4秒ほどの実行時間になります.</p>

<h2>共有変数</h2>

<p><a class="keyword" href="http://d.hatena.ne.jp/keyword/%CA%A3%BF%F4">複数</a>スレッドによる並行実行を扱うと, どうしても共有変数的なものが欲しくなる場合があります. <a class="keyword" href="http://d.hatena.ne.jp/keyword/Haskell">Haskell</a>でスレッド間共有をしたい場合はいくつかの方法があるようです.<br/>
もっとも直感的(手続きプログラミング出身者にとって)で馴染みやすいのは <code>Data.IORef</code> かと思います. <code>IO</code> の世界の内側でのみ読み書きができる"変数"です.</p>

<p>まずは単一スレッドで実際に使ってみます.(以後import などは省略します)</p>

<pre class="code" data-lang="" data-unlink>add1 :: IORef Int -&gt; IO ()
add1 v = do
    modifyIORef v (+1)

main :: IO ()
main = do
    ref &lt;- newIORef 0
    v &lt;- readIORef ref
    print v
    add1 ref
    v&#39; &lt;- readIORef ref
    print v&#39;</pre>


<p>実行結果</p>

<pre class="code" data-lang="" data-unlink>0
1</pre>


<p>このように変数として中身を書き換えることができます.<br/>
これは変数なので, ひとつのスレッドで行った書き換えが他のスレッドにも影響を及ぼします.(State<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%E2%A5%CA%A5%C9">モナド</a>のように変数を模倣しているだけではこれはできない)</p>

<pre class="code" data-lang="" data-unlink>add1 :: IORef Int -&gt; IO ()
add1 v = modifyIORef v (+1)

spawn :: IORef Int -&gt; IO ()
spawn ref = do
    forkIO $ add1 ref
    return ()

main :: IO ()
main = do
    ref &lt;- newIORef 0
    spawn ref
    spawn ref
    spawn ref
    threadDelay 1000000
    v &lt;- readIORef ref
    print v</pre>


<h2>データ競合</h2>

<p>一方これを<a class="keyword" href="http://d.hatena.ne.jp/keyword/%CA%A3%BF%F4">複数</a>スレッドで並列に動かすことを考えます. <code>modifyIORef</code> はアトミックではないので,</p>

<pre><code>v の中身を読む
v の中身 + 1 を計算する
v にその結果を入れる
</code></pre>

<p>というそれぞれの計算の間に別のスレッドでの計算が割り込まれる可能性がある.<br/>
上の例で, <code>spawn ref &gt;&gt; spawn ref &gt;&gt; spawn ref</code> という部分は<a class="keyword" href="http://d.hatena.ne.jp/keyword/%CA%A3%BF%F4">複数</a>スレッドから一つの変数を同時に変更しようとしている. そのため, 変更が競合し意図しない動作になる可能性がある.</p>

<p>IORefで競合を防ぐ方法としては <code>atomicModifyIORef :: IORef a -&gt; (a -&gt; (a, b)) -&gt; IO b</code> を使用する方法がある.<br/>
<code>atomicModifyIORef</code> の第2引数は <code>a -&gt; (a, b)</code> である. これは <code>IORef</code> の中身を引数にとって, <code>(変更後の値, atomicModifyIORefの返り値にしたい値)</code> を返す関数である.</p>

<pre class="code" data-lang="" data-unlink>inc :: IORef Int -&gt; IO Int
inc ref = atomicModifyIORef ref (\n -&gt; (n + 1, n))

main :: IO ()
main = do
    ref &lt;- newIORef 0
    res &lt;- inc ref
    v &lt;- readIORef ref
    print res
    print v</pre>


<p><code>inc</code> は<a class="keyword" href="http://d.hatena.ne.jp/keyword/C%B8%C0%B8%EC">C言語</a>の<code>i++;</code>のような動きをする. 加算する前の値を返し, 変数をインクリメントする.<br/>
<code>atomicModifyIORef</code> は名前の通り atomic な操作であり, 分割不可能になるため他のスレッドと処理が競合することがなくなる.</p>

<h2>一旦まとめ</h2>

<p>長くなってきた &amp; 疲れてきたので一旦きります.<br/>
今回はスレッド間共有変数のために <code>IORef</code> を使用し, その変更に <code>atomicModifyIORef</code> を使用することでデータ競合を防ぐ方法を紹介した.</p>

<p><code>MVar</code> や <code>STM</code> を使用する方法もあり, そっちのほうが良い場合もあるっぽいのでそっちについてもまとめたいと思います.</p>

-----
--------