---
title: "Rust で Unix のシグナルを channel 経由でキャッチする"
date: 2017-07-10T11:59:04.000Z
tags: []
---
<p>Rust でシグナルハンドリングをする必要があったのですが，あまり自分の用途にあるライブラリがなかったので作りました．
僕が <a class="keyword" href="http://d.hatena.ne.jp/keyword/Windows">Windows</a> のことをほとんどわからないので，<a class="keyword" href="http://d.hatena.ne.jp/keyword/Windows">Windows</a> 未対応です．</p>

<p><iframe src="https://hatenablog-parts.com/embed?url=https%3A%2F%2Fgithub.com%2Fagatan%2Fsignal-notify" title="agatan/signal-notify" class="embed-card embed-webcard" scrolling="no" frameborder="0" style="display: block; width: 100%; height: 155px; max-width: 500px; margin: 10px 0px;"></iframe><cite class="hatena-citation"><a href="https://github.com/agatan/signal-notify">github.com</a></cite></p>

<p><iframe src="https://hatenablog-parts.com/embed?url=https%3A%2F%2Fdocs.rs%2Fsignal-notify%2F0.1.2%2Fsignal_notify%2F" title="signal_notify - Rust" class="embed-card embed-webcard" scrolling="no" frameborder="0" style="display: block; width: 100%; height: 155px; max-width: 500px; margin: 10px 0px;"></iframe><cite class="hatena-citation"><a href="https://docs.rs/signal-notify/0.1.2/signal_notify/">docs.rs</a></cite></p>

<p><a href="https://crates.io/crates/signal-notify">https://crates.io/crates/signal-notify</a></p>

<p><a class="keyword" href="http://d.hatena.ne.jp/keyword/golang">golang</a> の <code>signal.Notify</code> に寄せた <a class="keyword" href="http://d.hatena.ne.jp/keyword/API">API</a> になっていて，標準ライブラリの <code>std::sync::mpsc::{Sender, Receiver}</code> 経由でシグナルを待ち受けることができます．</p>

<pre class="code lang-rust" data-lang="rust" data-unlink><span class="synStatement">extern</span> <span class="synStatement">crate</span> <span class="synIdentifier">signal_notify</span>;
<span class="synStatement">use</span> <span class="synPreProc">signal_notify</span><span class="synSpecial">::</span>{notify, Signal};
<span class="synStatement">use</span> <span class="synPreProc">std</span><span class="synSpecial">::</span><span class="synPreProc">sync</span><span class="synSpecial">::</span><span class="synPreProc">mpsc</span><span class="synSpecial">::</span>Receiver;

<span class="synStatement">fn</span> <span class="synIdentifier">main</span>() {
    <span class="synStatement">let</span> rx: Receiver<span class="synStatement">&lt;</span>Signal<span class="synStatement">&gt;</span> <span class="synStatement">=</span> <span class="synIdentifier">notify</span>(<span class="synType">&amp;</span>[<span class="synPreProc">Signal</span><span class="synSpecial">::</span>INT, <span class="synPreProc">Signal</span><span class="synSpecial">::</span>USR1]);
    <span class="synStatement">for</span> sig <span class="synStatement">in</span> rx.<span class="synIdentifier">iter</span>() {
        <span class="synStatement">match</span> sig {
            <span class="synPreProc">Signal</span><span class="synSpecial">::</span>INT <span class="synStatement">=&gt;</span> {
                <span class="synPreProc">println!</span>(<span class="synConstant">&quot;Interrupted!&quot;</span>);
                <span class="synStatement">break</span>;
            }
            <span class="synPreProc">Signal</span><span class="synSpecial">::</span>USR1 <span class="synStatement">=&gt;</span> <span class="synPreProc">println!</span>(<span class="synConstant">&quot;Got SIGUSR1!&quot;</span>),
        }
    }
}
</pre>


<p>Rust で <a class="keyword" href="http://d.hatena.ne.jp/keyword/Unix">Unix</a> シグナルを取るライブラリとしては <a href="https://github.com/BurntSushi/chan-signal">GitHub - BurntSushi/chan-signal: Respond to OS signals with channels.</a> というのが有名です．
こちらは標準ライブラリの <code>mpsc::channel</code> ではなく，<code>chan</code> クレイトの <code>channel</code> を使っています．
<code>chan</code> クレイトはケースによってはかなり便利で，</p>

<ol>
<li>複数の consumer を作れる (<code>receiver.clone()</code> ができる)</li>
<li><code>chan_select!</code> マクロによって <a class="keyword" href="http://d.hatena.ne.jp/keyword/golang">golang</a> の <code>select</code> 的なことができる</li>
</ol>


<p>という利点があります．</p>

<p>一方で複数 consumer にする必要がない &amp; <code>chan_select!</code> が必要ないケースでは，シグナルハンドリングのためだけに <code>chan</code> にも依存するのもなんとなくはばかられるという気持ちがありました．
また，自分の目的として「<code>SIGWINCH</code> と <code>SIGIO</code> が取りたい」というのがあったのですが，<code>chan-signal</code> の仕組みだとデフォルトで無視されるシグナルをキャッチできない(<a class="keyword" href="http://d.hatena.ne.jp/keyword/macOS">macOS</a> だけ)という問題もありました．
報告するときに方法を考えていたのですが，あまり自信がなかったのとほとんど完全に仕組みを書きなおす形になりそうだったので，自分の手元で <code>std::sync::mpsc</code> を使って実験してみたという経緯です．</p>

<h2>仕組み</h2>

<ol>
<li>初期化時にパイプを作る</li>
<li>シグナルごとに通知すべき <code>Sender</code> を覚えておく</li>
<li>シグナルごとに <code>sigaction</code> でハンドラをセットする

<ul>
<li>シグナルが来たらそれをパイプに <code>write(2)</code> する</li>
</ul>
</li>
<li>シグナル待受＆通知用のスレッドを起動する

<ul>
<li>パイプからシグナル番号を読んで，適切な <code>Sender</code> に <code>send</code> する</li>
</ul>
</li>
</ol>


<p>という仕組みで動いています．
自信がなかったのは，「シグナルハンドラでやっていいこと一覧」をちゃんと把握していないという点です．
一応 <code>sigaction</code> の man を見ると <code>write</code> は読んでもいい関数一覧にいる気がするし，実際動いてはいるのでセーフだろうと判断しました．
（もしアウトだったら教えてください）</p>

<p>ちなみに <code>chan-signal</code> の方は，</p>

<ol>
<li>シグナルごとに通知すべき <code>Sender</code> を覚えておく</li>
<li>監視用スレッドを起動し，メインスレッドでは <code>pthread_sigmask</code> を使ってシグナルをブロックする

<ul>
<li>シグナルがすべて監視用スレッドに渡るようにする</li>
</ul>
</li>
<li>監視用スレッドで <code>sigwait</code> して適切な <code>Sender</code> に <code>send</code> する</li>
</ol>


<p>という仕組みで動いているようです．
<code>sigwait</code> は指定したシグナルが投げられるまでブロックします．
ただし，<a class="keyword" href="http://d.hatena.ne.jp/keyword/macOS">macOS</a> で <code>sigwait</code> の man を見ると，</p>

<blockquote><p>Processes which call sigwait() on ignored signals will wait indefinitely. Ignored signals are dropped immediately by the system, before delivery to a waiting process.</p></blockquote>

<p>とあって，無視されるシグナルを <code>sigwait</code> で待っても補足できないようです．
<a class="keyword" href="http://d.hatena.ne.jp/keyword/Linux">Linux</a> の man を見るとそんなことは書いていないし，普通に動くっぽいです．</p>

<p>今の実装だと，シグナルを受け取る <code>Receiver</code> がすべて閉じても，監視スレッドは動き続けるしハンドラも残り続けるので，これはなんとかしたいなぁと思っています．
アプリケーションの実行時間のうち，ある期間だけシグナルをとってそれ以外はスルーしたいというケースもそんなにないかなというのと，内部的な変更にしかならないので <a class="keyword" href="http://d.hatena.ne.jp/keyword/API">API</a> が変わらないというのがあるので，この状態でとりあえず public にしました．</p>

<p><a class="keyword" href="http://d.hatena.ne.jp/keyword/CLI">CLI</a> を書いていると意外と普通に <code>SIGINT</code> は取りたくなることがあると思うので，ぜひ使ってみてください．
issue 報告等お待ちしています．</p>

-----
--------