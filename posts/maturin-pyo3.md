---
title: "maturin でさくっと Rust 製 Python 拡張を書く"
date: 2022-02-13
tags: ["Rust", "Python"]
---

機械学習やデータ処理をやっていると、一部だけ Python では遅すぎるので C, C++, Rust のような高速な言語で処理を書きたくなることがまれによくあります。
C++ なら [pybind11](https://github.com/pybind/pybind11)、Rust なら [PyO3](https://github.com/PyO3/pyo3) が非常に有名で、これらをつかえばかなり簡単に Python <-> C++ / Rust を相互に行き来する処理を書くことができます。

と、ここまでは昔の知識で把握していたのですが、最近改めて必要に迫られて Rust で Python Extension を書こうとドキュメントを読んでいたところ、更に簡単にそういった処理を書けるようにする開発ツールである [maturin](https://github.com/PyO3/maturin) なるものがリリースされているのを知りました。

## セットアップ

[Maturin User Guide](https://maturin.rs/) がきちんとしているので、わざわざ使い方を書く必要もないのですが、

```sh
$ maturin new --bindings pyo3 foo
$ tree foo
.
├── Cargo.toml
├── pyproject.toml
└── src
    └── lib.rs
```

で新規プロジェクトを生成できます。
pyproject.toml は、以下のようになっており、 `build-system` が指定されているため、もうこの時点で `pip install <name>` すれば Python から使える状態になります。
(ref. [PEP517](https://www.python.org/dev/peps/pep-0517/), [PEP518](https://www.python.org/dev/peps/pep-0518/))

```toml
[build-system]
requires = ["maturin>=0.12,<0.13"]
build-backend = "maturin"

[project]
name = "foo"
requires-python = ">=3.6"
classifiers = [
    "Programming Language :: Rust",
    "Programming Language :: Python :: Implementation :: CPython",
    "Programming Language :: Python :: Implementation :: PyPy",
]
```

ちなみに `Cargo.toml` があるので `cargo build` したくなるのですが、それはできません。 (あんまり調べてないですが、Python 関連のシンボルが undefined になってビルドに失敗する。)
代わりに `maturin develop` でビルド + 現在アクティブになっている virtualenv へのインストールを行います。
(`maturin develop --release` でリリースビルドができる。)

Rust コードだけでなく Python wrapper も同時に開発するようなケースでは、 `pip install -e foo` をつかうほうが便利かもしれません。
Python wrapper 側の変更だけならビルドしなおす必要がなくなり、即座に反映されるようになります。
(Rust 側をいじったときはビルドしなおす必要がある。)

## 配布

まだ自分で使っている範囲では PyPI に publish することはなく、プロジェクト内ローカルなパッケージとして使用しているだけなので、基本的には上記のようなコマンド群だけで開発が完結しているのですが、maturin では配布のことも考えたユーティリティも用意されています。

`maturin build` で配布用の wheel を生成できるのですが、これがまた便利そうで、システムに存在しているすべての Python バージョンに対して wheel を生成してくれます。
たとえば、仮に現在アクティブな virtualenv が Python 3.10 系だとしても、Python 3.9 系がインストールされているシステム上で `maturin build` をすると、3.9, 3.10 用の wheel をそれぞれビルドしてくれるのです。

さらに、もろもろセットアップが必要ですが、クロスコンパイルのための仕組みも充実しており、 `maturin build --universal2` で macOS 用の universal binary (x86_64, arm 両対応)を出力したり、 `maturin build --target=...` でクロスコンパイルしたりできます。

さらにさらに Python の Native Package のお作法である manylinux 対応(特定の動的ライブラリとしかリンクしないことで、いろんな Linux 環境でうごくことを保証する) もビルドオプションで解決可能です。 (`maturin build --release --target aarch64-unknown-linux-gnu --zig`。 ここで zig がでてくるのにちょっと感動しました。)

[Docker イメージ](https://hub.docker.com/r/konstin2/maturin) や [GitHub Actions](https://github.com/messense/maturin-action) もすでにコミュニティによってサポートされており、CI 上でも簡単に build & publish ができそうです。

## まとめ

Python だと遅いけど C++ や Rust でかいてブリッジするのも面倒だし、そこまでやるのは大袈裟かなーとかおもって結局 Python のままいったり Cython, numba でいったりする、という経験をいままで何度かしてきました。
が、maturin が想像以上に体験が良く、開発効率もよかったので、今後はより気軽に Python プロジェクトに Rust をつっこんでいくスタイルが通用しそうだなと思いました。

また、Rust は効率追いもとめたいときには本当に優秀なツールだと改めて実感しました。PyO3 もめちゃくちゃ良くできていて、混み入ったアルゴリズムだったら Rust で書いたほうが、開発効率的にも好ましいんじゃないかと思います。
