---
title: "go generate する時のバイナリのバージョンを固定したい"
date: 2017-08-05T16:58:13.000Z
tags: ["Go"]
---

[https://github.com/golang/mock](https://github.com/golang/mock) は `mockgen` というコマンドを提供しています．
これは，interface から mock を自動生成するコマンドで `go generate` と合わせて使うと interface に追従する mock がとても簡単に作れます．

他にも yacc とかリソースをバイナリに埋め込むとか，色々便利ツールがあり，`go generate` でコード生成をするのは golang のアプリケーションではよくあることだと思います．

しかし，golang の `vendor/` の仕組みは基本的に package として使うことを考えて作られているので，プロジェクトごとに `mockgen` などの生成コマンドのバージョンを固定するためには使えません．

ここで，`go generate` で使うバイナリのバージョンが固定されていないと起こりうる問題として

- 生成されたコードに毎回 diff が出る
  - 気軽に `git add .` 出来ないし，コミット漏れや無駄コミットにつながる
- バージョン A のコマンドとバージョン B のコマンドによって生成されたコードが混ざる
- ライブラリのバージョンと生成コマンドのバージョンが一致しないためバグる
  - `github.com/golang/mock/mockgen` は `github.com/golang/mock/gomock` というライブラリとセットで使うので，`gomock` package と `mockgen` バイナリのバージョンは揃えたい

などがあります．

これ結構嫌な問題だと思ったのですが，パッとぐぐってみてもあまり困っている声を聞かないので普通どうやって解決しているのか気になっています．
(もしかして僕が知らないだけで普通に解決されている問題だったりするのだろうか…)

とりあえず `vendor` 以下の package をビルドしたり固定されたバージョンのバイナリをぱぱっと実行するために

[github.com](https://github.com/agatan/bindor)

を作ってみました．

shell script で書けそうな単純な仕事しかしていませんが，go で実装されています．

`bindor build github.com/golang/mock/mockgen` で `./.bindor/mockgen` というバイナリが出来ます．
`bindor exec command args...` とやると `PATH=./.bindor:$PATH` した環境下で `command args...` を実行します．

```
$ glide get github.com/golang/mock
$ bindor build github.com/golang/mock/mockgen
$ bindor exec which mockgen
/path/to/current/.bindor/mockgen

```

という感じです．

`//go:generate bindor mockgen` としてもいいですが，`bindor exec go generate` とすればソースコードを書き換えなくても `.bindor` 以下のバイナリを使うようになるはずです．

`bindor` 自体にバージョンを固定する仕組みは入れていません．glide とかがやってくれている仕事を分散させても管理が面倒になるだけでメリットがなさそうだし，ライブラリとしても使う package の場合はどうせ glide で管理するので，`vendor` 以下のディレクトリの奪い合いになってしまいます．

というわけでバイナリを vendoring する `bindor` を作った話でした．もっといい解決方法があったら教えてください．

---

---
