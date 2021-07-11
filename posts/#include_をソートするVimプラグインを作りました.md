---
title: "#include をソートするVimプラグインを作りました"
date: 2016-01-24T10:11:25.000Z
tags: []
---

<p><a href="http://itchyny.hatenablog.com/entry/2016/01/23/190000">Haskellでimport文をソートするプラグイン vim-haskell-sort-import を作りました - プログラムモグモグ</a>という記事を拝見して，コードを見たらすごくわかりやすくて，これの <a class="keyword" href="http://d.hatena.ne.jp/keyword/C/C%2B%2B">C/C++</a> 版がほしいと思い，書いてみました．</p>

<p><a class="keyword" href="http://d.hatena.ne.jp/keyword/vim">vim</a> script はほとんど書いたことがないんですが，やっぱりエディタ拡張用の<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B9%A5%AF%A5%EA%A5%D7%A5%C8">スクリプト</a>なので，普通の言語と違う部分は多いですね…
でもその分エディタという UI が既に用意されている状態なので，なんというか書いていて楽しかったです．さくっと書けますし．（先ほどのコードを参考にしているというのもありますが）</p>

<h2>使い方</h2>

<p><code>NeoBundle</code> や <code>vim-plug</code> のような<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%D7%A5%E9%A5%B0%A5%A4%A5%F3">プラグイン</a>マネージャを使うなどして runtime path に突っ込んでください．
提供する機能は <code>SortInclude</code> コマンドのみです．</p>

<p><span itemscope itemtype="http://schema.org/Photograph"><img src="http://cdn-ak.f.st-hatena.com/images/fotolife/a/agtn/20160124/20160124191335.gif" alt="f:id:agtn:20160124191335g:plain" title="f:id:agtn:20160124191335g:plain" class="hatena-fotolife" itemprop="image"></span></p>

<p>こんな感じの動作をします．</p>

<p><code>#include</code> は <code>""</code> を使う場合と <code>&lt;&gt;</code> を使う場合があり，それぞれファイルパスの探索場所が異なるので，それぞれ別のグループとしてソートするようにしました．</p>

<pre class="code lang-c" data-lang="c" data-unlink><span class="synPreProc">#include </span><span class="synConstant">&lt;iostream&gt;</span>
<span class="synPreProc">#include </span><span class="synConstant">&quot;a.h&quot;</span>
<span class="synPreProc">#include </span><span class="synConstant">&quot;z.h&quot;</span>
</pre>

<p>が</p>

<pre class="code lang-c" data-lang="c" data-unlink><span class="synPreProc">#include </span><span class="synConstant">&quot;a.h&quot;</span>
<span class="synPreProc">#include </span><span class="synConstant">&lt;iostream&gt;</span>
<span class="synPreProc">#include </span><span class="synConstant">&quot;z.h&quot;</span>
</pre>

<p>にソートされたら気持ち悪いと思うので．</p>

<p>あとは参考にさせていただいた<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%D7%A5%E9%A5%B0%A5%A4%A5%F3">プラグイン</a>と同様，空行を挟むなどブロック化されている場合は，ブロック内でソートします．</p>

<p><code>#include</code> をソートするとか既にありふれてそうですが，はじめての <a class="keyword" href="http://d.hatena.ne.jp/keyword/vim">vim</a> <a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%D7%A5%E9%A5%B0%A5%A4%A5%F3">プラグイン</a>ということで．
せっかくなのでドキュメントなども <a class="keyword" href="http://d.hatena.ne.jp/keyword/vim">vim</a> の help フォーマットにしたがって書いてみました．</p>

---

---
