---
title: "NCurses の Crystal binding を作った"
date: 2016-12-08T13:33:17.000Z
tags: ["Crystal"]
---

この記事は、 Crystal Advent Calendar 2016 の８日目の記事です。
[qiita.com](http://qiita.com/advent-calendar/2016/crystal)

ncurses という CUI を作るためにスクリーンやキー入力を扱う有名なライブラリの Crystal binding を作りました。

[github.com](https://github.com/agatan/ncurses.cr)

ほとんど C の ncurses と同じ感じで使えるようになっていると思います．
Ruby や Python の curses ライブラリを参考にしつつ，なるべく使用感がかわらないようにしています．

## examples

```
require “ncurses”

NCurses.open do
  NCurses.cbreak
  NCurses.noecho
  NCurses.move(x: 30, y: 20)
  NCurses.addstr(“Hello, world!”)
  NCurses.refresh

  NCurses.notimeout(true)
  NCurses.getch
end

```

`NCurses.addstr` とか `NCurses.move` とかは ncurses で言う `addstr` や `move` に当たる関数で，`stdscr` に対して `waddstr` とか `wmove` するやつです．

`w = NCurses::Window.new(height: height, width: width)` とすることで subwindow が作れます．
`w.addstr` や `w.move` という形で `w` prefix な関数たちが呼べるようになっています．

`pad` や `attron` / `attroff` などなども使えます．

詳細な例は `example/` 以下のにおいてあります

## なぜ作ったのか

実は ncurses bindings for Crystal はすでにあります．([https://github.com/jreinert/ncurses-crystal](https://github.com/jreinert/ncurses-crystal))

curses が Crystal の標準ライブラリから外されることが決まったときの CHANGELOG を見ると，今後はそっちを使ってねと書いてあったりします．

じゃあなんでわざわざ別で作ったのかという話ですが，API が足りない & 提供する API の形を変えたいと思ったからです．

単純に bind されている API が少なくてやりたいことができなかったので，最初は追加して PR を出そうと思っていたのですが，すでに提供されている API が割と高級になっていて 1 : 1 で C API と対応していない感じでした．
個人的には C library の wrapper にはなるべく薄くなっていてもらって基本的な API は覚え直さないでも使えるようになっていてほしいというふうに思ったので，C API と 1 : 1 で対応した形の API を提供する wrapper を作ってみようという経緯で新しく作ることにしました．

## おまけ

C bindings を書くときに，wrapper API として `LibC::Int` が登場しちゃうのがなんとなく嫌で，`LibC::Int` を C が要求してくる関数を呼ぶ wrapper 関数には型指定をあえてしないという選択をしたんですがどうなんでしょう．

```
lib LibNCurses
  fun wmove(w : Window, y : LibC::Int, x : LibC::Int) : Result
end

```

```
class Window
  def move(y, x)
     LibNCurses.wmove(@win, y, x)
  end
end

```

みたいな感じです．（多少簡略化しています）

これどうなんですかね．なるべく外に見せる API には型を明記するようにしたかったのですが，`LibC:Int` 系は環境によって異なるのでそういうわけにも行かず…

`y : LibC::Int, x : LibC::Int` とかは別に良いんですが，ncurses は文字と属性をくっつけた `chtype` なる型を持っていてこれが結構厄介というか混乱を招くのではと思っています．
`chtype` は `char` ではなく `unsigned int` で，文字と属性を bitor でくっつけたものになっています．`addch` のように `char` をとることを連想させる関数の引数が実は `chtype = unsigned int` でしかも Crystal の文字型 `Char` は 32bit なのでものすごく混乱します…

C は型変換を勝手にやってくれるので，`unsigned int` を返す関数から受け取った値を `short` を受け取る関数に渡すみたいなことをよくやっていて，Crystal のような型変換を暗黙にやらない言語から使おうとすると難しいんだなぁと思いました．
なにか良い方法があればぜひ知りたいです．

---

---
