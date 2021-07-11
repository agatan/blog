---
title: "Sokoban.nim を書いてみた"
date: 2015-10-29T13:10:42.000Z
tags: []
---
<h1>Sokoban.nim を書いてみた</h1>

<p><a href="http://www.nim-lang.org">Nim</a> という言語を使って<a class="keyword" href="http://d.hatena.ne.jp/keyword/%C1%D2%B8%CB%C8%D6">倉庫番</a>を書いてみました.</p>

<p><a href="https://github.com/agatan/sokoban-nim">sokoban-nim</a></p>

<p><a href="https://github.com/swatteau/sokoban-rs">sokoban-rs</a> に影響されました．</p>

<p>といっても <a class="keyword" href="http://d.hatena.ne.jp/keyword/SDL">SDL</a> ド素人なので見た目は恐ろしく質素です． Nim の練習のつもりで書いてみました．</p>

<h2>動作</h2>

<p>動作としては，<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%B3%A5%DE%A5%F3%A5%C9%A5%E9%A5%A4%A5%F3">コマンドライン</a>引数にステージ情報を記述したファイル名を指定します．</p>

<pre><code>##########
#   .    #
#  $   . ####
# @ $       #
#           #
#############
</code></pre>

<p>このようなファイルを指定します． クリアするとなにも言わずに終了するようになっています．(また, ESC でも終了します)</p>

<p><span itemscope itemtype="http://schema.org/Photograph"><img src="http://cdn-ak.f.st-hatena.com/images/fotolife/a/agtn/20151029/20151029220921.png" alt="f:id:agtn:20151029220921p:plain" title="f:id:agtn:20151029220921p:plain" class="hatena-fotolife" itemprop="image"></span></p>

<p>緑がプレイヤー, 青がゴール, 赤が箱です. 箱を押して青を塞げばクリアです.</p>

<h2>感想</h2>

<p>Nim で遊びたくて作ってみましたが，思っていた以上に Nim が楽しいですね． ライブラリも整っていますし，特に難しいことはなくサクサク書けました．</p>

<p>Nim で <a class="keyword" href="http://d.hatena.ne.jp/keyword/Lisp">Lisp</a> 処理系を書いたりしていて，息抜きに<a class="keyword" href="http://d.hatena.ne.jp/keyword/GUI">GUI</a>っぽいものをと思って書きました． Cライブラリとの連携も非常にスムーズでしたし，Nim 流行んないかなぁと思っています．</p>

-----
--------