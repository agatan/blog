---
title: "include をソートするVimプラグインを作りました"
postSlug: include-cpp-vim-plugin
pubDatetime: 2016-01-24T10:11:25.000Z
tags: ["C++", "Vim"]
---

[Haskell で import 文をソートするプラグイン vim-haskell-sort-import を作りました - プログラムモグモグ](http://itchyny.hatenablog.com/entry/2016/01/23/190000)という記事を拝見して，コードを見たらすごくわかりやすくて，これの C/C++ 版がほしいと思い，書いてみました．

vim script はほとんど書いたことがないんですが，やっぱりエディタ拡張用のスクリプトなので，普通の言語と違う部分は多いですね…
でもその分エディタという UI が既に用意されている状態なので，なんというか書いていて楽しかったです．さくっと書けますし．（先ほどのコードを参考にしているというのもありますが）

## 使い方

`NeoBundle` や `vim-plug` のようなプラグインマネージャを使うなどして runtime path に突っ込んでください．
提供する機能は `SortInclude` コマンドのみです．

![f:id:agtn:20160124191335g:plain](/i/20160124191335.gif "f:id:agtn:20160124191335g:plain")

こんな感じの動作をします．

`#include` は `""` を使う場合と `<>` を使う場合があり，それぞれファイルパスの探索場所が異なるので，それぞれ別のグループとしてソートするようにしました．

```
#include <iostream>
#include "a.h"
#include "z.h"

```

が

```
#include "a.h"
#include <iostream>
#include "z.h"

```

にソートされたら気持ち悪いと思うので．

あとは参考にさせていただいたプラグインと同様，空行を挟むなどブロック化されている場合は，ブロック内でソートします．

`#include` をソートするとか既にありふれてそうですが，はじめての vim プラグインということで．
せっかくなのでドキュメントなども vim の help フォーマットにしたがって書いてみました．

---

---
