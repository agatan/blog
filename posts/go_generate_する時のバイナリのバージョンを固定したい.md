---
title: "go generate する時のバイナリのバージョンを固定したい"
date: 2017-08-05T16:58:13.000Z
tags: []
---

<p><a href="https://github.com/golang/mock">https://github.com/golang/mock</a> は <code>mockgen</code> というコマンドを提供しています．
これは，interface から mock を自動生成するコマンドで <code>go generate</code> と合わせて使うと interface に追従する mock がとても簡単に作れます．</p>

<p>他にも <a class="keyword" href="http://d.hatena.ne.jp/keyword/yacc">yacc</a> とかリソースをバイナリに埋め込むとか，色々便利ツールがあり，<code>go generate</code> でコード生成をするのは <a class="keyword" href="http://d.hatena.ne.jp/keyword/golang">golang</a> のアプリケーションではよくあることだと思います．</p>

<p>しかし，<a class="keyword" href="http://d.hatena.ne.jp/keyword/golang">golang</a> の <code>vendor/</code> の仕組みは基本的に package として使うことを考えて作られているので，プロジェクトごとに <code>mockgen</code> などの生成コマンドのバージョンを固定するためには使えません．</p>

<p>ここで，<code>go generate</code> で使うバイナリのバージョンが固定されていないと起こりうる問題として</p>

<ul>
<li>生成されたコードに毎回 diff が出る

<ul>
<li>気軽に <code>git add .</code> 出来ないし，コミット漏れや無駄コミットにつながる</li>
</ul>
</li>
<li>バージョンAのコマンドとバージョンBのコマンドによって生成されたコードが混ざる</li>
<li>ライブラリのバージョンと生成コマンドのバージョンが一致しないためバグる

<ul>
<li><code>github.com/golang/mock/mockgen</code> は <code>github.com/golang/mock/gomock</code> というライブラリとセットで使うので，<code>gomock</code> package と <code>mockgen</code> バイナリのバージョンは揃えたい</li>
</ul>
</li>
</ul>

<p>などがあります．</p>

<p>これ結構嫌な問題だと思ったのですが，パッとぐぐってみてもあまり困っている声を聞かないので普通どうやって解決しているのか気になっています．
(もしかして僕が知らないだけで普通に解決されている問題だったりするのだろうか&hellip;)</p>

<p>とりあえず <code>vendor</code> 以下の package をビルドしたり固定されたバージョンのバイナリをぱぱっと実行するために</p>

<p><iframe src="https://hatenablog-parts.com/embed?url=https%3A%2F%2Fgithub.com%2Fagatan%2Fbindor" title="agatan/bindor" class="embed-card embed-webcard" scrolling="no" frameborder="0" style="display: block; width: 100%; height: 155px; max-width: 500px; margin: 10px 0px;"></iframe><cite class="hatena-citation"><a href="https://github.com/agatan/bindor">github.com</a></cite></p>

<p>を作ってみました．</p>

<p>shell script で書けそうな単純な仕事しかしていませんが，go で実装されています．</p>

<p><code>bindor build github.com/golang/mock/mockgen</code> で <code>./.bindor/mockgen</code> というバイナリが出来ます．
<code>bindor exec command args...</code> とやると <code>PATH=./.bindor:$PATH</code> した環境下で <code>command args...</code> を実行します．</p>

<pre class="code lang-sh" data-lang="sh" data-unlink>$ glide get github.com/golang/mock
$ bindor build github.com/golang/mock/mockgen
$ bindor <span class="synStatement">exec</span> which mockgen
/path/to/current/.bindor/mockgen
</pre>

<p>という感じです．</p>

<p><code>//go:generate bindor mockgen</code> としてもいいですが，<code>bindor exec go generate</code> とすれば<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%BD%A1%BC%A5%B9%A5%B3%A1%BC%A5%C9">ソースコード</a>を書き換えなくても <code>.bindor</code> 以下のバイナリを使うようになるはずです．</p>

<p><code>bindor</code> 自体にバージョンを固定する仕組みは入れていません．glide とかがやってくれている仕事を分散させても管理が面倒になるだけでメリットがなさそうだし，ライブラリとしても使う package の場合はどうせ glide で管理するので，<code>vendor</code> 以下の<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%C7%A5%A3%A5%EC%A5%AF%A5%C8">ディレクト</a>リの奪い合いになってしまいます．</p>

<p>というわけでバイナリを vendoring する <code>bindor</code> を作った話でした．もっといい解決方法があったら教えてください．</p>

---

---
