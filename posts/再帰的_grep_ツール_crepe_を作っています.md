---
title: "再帰的 grep ツール crepe を作っています"
date: 2016-07-08T15:36:15.000Z
tags: ["C++"]
---

<p><a class="keyword" href="http://d.hatena.ne.jp/keyword/%BA%C6%B5%A2">再帰</a>的にパスをたどりながらパターンにマッチする行を検索する <a class="keyword" href="http://d.hatena.ne.jp/keyword/grep">grep</a> 系のツールを作っています。</p>

<p><a href="https://github.com/agatan/crepe">agatan/crepe</a></p>

<p>有名どころとしては、<a href="http://beyondgrep.com/">ack</a> とか <a href="https://github.com/ggreer/the_silver_searcher">ggreer/the_silver_searcher</a>(ag) とか <a href="https://github.com/monochromegane/the_platinum_searcher">monochromegane/the_platinum_searcher</a> とか <a href="https://github.com/tkengo/highway">tkengo/highway</a> なんかがあります。群雄割拠ですね。</p>

<p>これらの有名ツール達はそれぞれ特徴があって(速さとか出力形式とか<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%A8%A5%F3%A5%B3%A1%BC%A5%C9">エンコード</a>対応とか)便利に使わせていただいております。<br/>
今回 <code>crepe</code> を作ろうと思った理由は、なんとなく <a class="keyword" href="http://d.hatena.ne.jp/keyword/C%2B%2B">C++</a> でちゃんとスレッド立てて並行処理みたいなコードを書いてみたくなったからです。</p>

<p>速度等も測っていないのでどこまで意味があるのか怪しいのですが、とりあえず現状 3 つの仕事を並列に動かしています。</p>

<p>一つ目は出力を担当するスレッドで、マッチ結果を受け取って出力するだけです。<br/>
二つ目はマッチを担当するスレッドで、ファイル名と <code>FILE*</code> を受け取ってマッチ結果を生成し出力担当スレッドに渡します。<br/>
三つ目はパスを walk するスレッドで、<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%C7%A5%A3%A5%EC%A5%AF%A5%C8">ディレクト</a>リを掘りながらファイルを開いてマッチスレッドに送ります。</p>

<p>現状はまだ部分一致の matcher しか実装していないので<a class="keyword" href="http://d.hatena.ne.jp/keyword/%C0%B5%B5%AC%C9%BD%B8%BD">正規表現</a>や fuzzy マッチは未実装です。<br/>
標準入力かパスから部分一致する行を探してきて出力します。<br/>
出力形式は <code>ag</code> に近い形式で、ファイルごとにグループ分けして行番号とともに出力します（オプションでこの辺の挙動はいじれるようにはなっています）</p>

<p><a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%D0%A5%A4">バイ</a>ナリファイルっぽいファイルはスキップするようになっています。</p>

<h2>やりたいこと</h2>

<p>最終目標として <code>crepe</code> は <a href="https://github.com/peco/peco">peco</a> のような interactive な検索を実装してみたいなと思っています。<br/>
<code>ag</code> と <code>peco</code> の組み合わせで云々みたいな<a class="keyword" href="http://d.hatena.ne.jp/keyword/%A5%E6%A1%BC%A5%B9%A5%B1%A1%BC%A5%B9">ユースケース</a>が割りとあるなぁと思ったので。<br/>
ただこれメモリ使用量とか速度上の制約からまともに働くのかはよくわかっていません。<br/>
単純に考えると、ユーザが一文字入力するたびにファイルの read からやり直す必要があるので...<br/>
入力ファイルが多すぎた場合にどこまでキャッシュするのかとかその辺を相当うまくやらないと死ぬのでは？という気がします。</p>

<p>短期的な目標としては</p>

<ul>
<li>.gitignore 対応 (意外とめっちゃ面倒で苦しんでいます)</li>
<li>fuzzy マッチ</li>
<li><a class="keyword" href="http://d.hatena.ne.jp/keyword/%C0%B5%B5%AC%C9%BD%B8%BD">正規表現</a>マッチ</li>
</ul>

<p>あたりから順にやっていきたいなと思っています。とりあえずこっちからやろうかなと思います。</p>

<p>というわけで気合を入れる意味も込めて記事にしました。プルリクやフィードバック大歓迎なのでぜひよろしくお願いします。</p>

---

COMMENT:
AUTHOR: smaranrebub
EMAIL: ruysqucp@gmail.com
URL: http://cialisvipsale.com
IP: 200.26.168.127
DATE: 03/24/2018 15:30:29

Very good info, Thanks!
walgreens price for cialis <a href="http://cialisvipsale.com">buy brand cialis cheap</a>
prices on cialis 10 mg <a href="http://cialisvipsale.com">http://cialisvipsale.com</a>
acheter du cialis a geneve <a href="http://cialisvipsale.com">effetti del cialis</a>
prices for cialis 50mg

---

---
