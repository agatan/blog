---
title: "再帰的 grep ツール crepe を作っています"
postSlug: 再帰的_grep_ツール_crepe_を作っています
pubDatetime: 2016-07-08T15:36:15.000Z
tags: ["C++"]
---

再帰的にパスをたどりながらパターンにマッチする行を検索する grep 系のツールを作っています。

[agatan/crepe](https://github.com/agatan/crepe)

有名どころとしては、[ack](http://beyondgrep.com/) とか [ggreer/the_silver_searcher](https://github.com/ggreer/the_silver_searcher)(ag) とか [monochromegane/the_platinum_searcher](https://github.com/monochromegane/the_platinum_searcher) とか [tkengo/highway](https://github.com/tkengo/highway) なんかがあります。群雄割拠ですね。

これらの有名ツール達はそれぞれ特徴があって(速さとか出力形式とかエンコード対応とか)便利に使わせていただいております。

今回 `crepe` を作ろうと思った理由は、なんとなく C++ でちゃんとスレッド立てて並行処理みたいなコードを書いてみたくなったからです。

速度等も測っていないのでどこまで意味があるのか怪しいのですが、とりあえず現状 3 つの仕事を並列に動かしています。

一つ目は出力を担当するスレッドで、マッチ結果を受け取って出力するだけです。

二つ目はマッチを担当するスレッドで、ファイル名と `FILE*` を受け取ってマッチ結果を生成し出力担当スレッドに渡します。

三つ目はパスを walk するスレッドで、ディレクトリを掘りながらファイルを開いてマッチスレッドに送ります。

現状はまだ部分一致の matcher しか実装していないので正規表現や fuzzy マッチは未実装です。

標準入力かパスから部分一致する行を探してきて出力します。

出力形式は `ag` に近い形式で、ファイルごとにグループ分けして行番号とともに出力します（オプションでこの辺の挙動はいじれるようにはなっています）

バイナリファイルっぽいファイルはスキップするようになっています。

## やりたいこと

最終目標として `crepe` は [peco](https://github.com/peco/peco) のような interactive な検索を実装してみたいなと思っています。

`ag` と `peco` の組み合わせで云々みたいなユースケースが割りとあるなぁと思ったので。

ただこれメモリ使用量とか速度上の制約からまともに働くのかはよくわかっていません。

単純に考えると、ユーザが一文字入力するたびにファイルの read からやり直す必要があるので...

入力ファイルが多すぎた場合にどこまでキャッシュするのかとかその辺を相当うまくやらないと死ぬのでは？という気がします。

短期的な目標としては

- .gitignore 対応 (意外とめっちゃ面倒で苦しんでいます)
- fuzzy マッチ
- 正規表現マッチ

あたりから順にやっていきたいなと思っています。とりあえずこっちからやろうかなと思います。

というわけで気合を入れる意味も込めて記事にしました。プルリクやフィードバック大歓迎なのでぜひよろしくお願いします。

---

COMMENT:
AUTHOR: smaranrebub
EMAIL: ruysqucp@gmail.com
URL: http://cialisvipsale.com
IP: 200.26.168.127
DATE: 03/24/2018 15:30:29

Very good info, Thanks!
walgreens price for cialis [buy brand cialis cheap](http://cialisvipsale.com)
prices on cialis 10 mg [http://cialisvipsale.com](http://cialisvipsale.com)
acheter du cialis a geneve [effetti del cialis](http://cialisvipsale.com)
prices for cialis 50mg

---

---
