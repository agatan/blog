---
title: Nord Theme を使い始めた
postSlug: nord-theme
pubDatetime: 2023-09-13
tags: ["雑談"]
description: "気分転換にいろんなツールのカラーテーマをNordに変えてみた"
---

[Nord](https://www.nordtheme.com/) を使い始めた。

昔は color scheme を変えて気分転換する、というのはとても頻繁にやっていて、自分の中ではお手軽リフレッシュのネタだった。
が、iTerm2, VSCode, Vim, IntelliJ, etc... と、いろんなツールを使っていると、色が揃わない気持ち悪さがあって、コロコロ変えるのも面倒だったり、そもそもその色合いが特定のツールで（簡単に）使えるかどうかも微妙だったりして、おそらく 8 年くらいは [iceberg](https://cocopon.github.io/iceberg.vim/)を使い続けていた。

今回たまたま別の調べ物をしていて [WezTerm に移行してみている](https://zenn.dev/ymotongpoo/scraps/ec945f11b2b750) を見つけ、その中に Nord への言及があったので、チラ見してみたら、かなり好みの色合いだった。
しかもだいぶいろんなツール郡に普及している雰囲気があったので、久々に重い腰を上げて気分転換をやってみた。

ツールのカバレッジは非常に高く、[Nord - Ports](https://www.nordtheme.com/ports)をみると対応ツールを一覧できる。
自分が今回変えたのは、

- VS Code
- iTerm2
- Vim / Neovim
- IntelliJ
- Chrome (これは余計だったかもしれない)
- Obsidian

Obsidian のテーマは結構悩ましかったところだったので、今回の Nord で定着するといいなと思っている。

VS Code については、コメントの foreground color とワードのハイライトの background color が似すぎていて見辛くなってしまっていたので、以下の様な設定を settings.json に加えてちょっとだけ色をいじった。

```json
    "workbench.colorCustomizations": {
        "[Nord]": {
            "editor.wordHighlightBackground": "#404859"
        }
    },
```

色は適当だけど、まぁまぁ悪くない。
しばらく使ってみる。
気に入ったらこのブログの色合いも寄せてもいいかもしれない。

（それはそれとして、当初の目的だった WezTerm も試してみたい）
