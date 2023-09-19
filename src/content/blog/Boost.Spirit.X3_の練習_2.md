---
title: "Boost.Spirit.X3 の練習 2"
postSlug: Boost.Spirit.X3_の練習_2
pubDatetime: 2015-12-17T14:20:03.000Z
tags: ["C++", "Boost"]
---

[Boost.Spirit.X3 の練習 1 - プログラミングのメモ帳 ➚](http://agtn.hatenablog.com/entry/2015/12/17/190505)に引き続き，`Boost.Spirit.X3` のお勉強メモです．

## セマンティックアクション

構文解析にはセマンティックアクションというのがつきものです．

`yacc` や `parsec` など有名な構文解析のためのツール/ライブラリにもありますね．

セマンティックアクションとは，定義したパーサのルールにマッチした時に実行するプログラムのことです．

といってもわかりにくいと思うので，コードを出してしまいます．

以下のコードは，浮動小数点数にマッチしてその値を標準出力に出力するパーサの定義です．

```
#include <boost/spirit/home/x3.hpp>

#include <iostream>
#include <string>


namespace x3 = boost::spirit::x3;

auto const print\_action =
  [](auto& ctx) { std::cout << \_attr(ctx) << std::endl; };

int main()
{

  std::string src{ "( 123.4 )" };

  auto first = std::cbegin(src);
  auto const last = std::cend(src);

  auto parser = ("(" >> x3::double\_ >> ")")[print\_action];

  bool success = x3::phrase\_parse(first, last, parser, x3::ascii::space);

  if (!success || first != last) {
    // parse failed
    std::cerr << "Parse failed." << std::endl;
  }
}

```

```
$ clang++ -std=c++14 main.cpp
$ ./a.out
123.4
```

パーサの結果を受け取らないようにしています．`parser` は `double` を返しますが，その結果は無視しています．

そして，セマンティックアクション部分で，出力を行っています．

`auto const print_action` の定義が，セマンティックアクションです．

`C++14` のジェネリックラムダの機能をつかっています．

セマンティックアクションは，`X3` パーサのコンテキストを第一引数に取ります．

その具体的な型を気にしてはいけません．ジェネリックラムダで受け取ります．

`_attr(ctx)` で，現在マッチしているパーサの attribute にアクセス出来ます．
`"(" >> x3::double_ >> ")"` の attribute は `double` 型なので，`_attr(ctx)` はマッチした浮動小数点数になります．

`Qi` のセマンティックアクションは，関数オブジェクトなどを単純に使用することが出来ませんでした(?) が，`X3` ではセマンティックアクションはただの関数オブジェクトです．

パーサのコンテキストを引数に受け，結果や attribute への参照をそのまま関数内で扱えるようになったため，セマンティックアクションの記述がより _普通_ の関数っぽくかけるようになったと感じました．

## 名前付きパーサの定義

今までのサンプルでは，シンプルなパーサを１つ定義しているだけでした．

一方現実には，パーサが再帰的になることは珍しくありません．

そこで，パーサの名前を前方宣言し，あとから定義を記述するようなパーサの書き方を使用します．

```
#include <boost/spirit/home/x3.hpp>

#include <iostream>
#include <string>


namespace x3 = boost::spirit::x3;

namespace detail {

  x3::rule<class constant, int> const constant;

  auto const constant\_def = x3::int\_;

  BOOST\_SPIRIT\_DEFINE(constant);

} // namespace detail

using detail::constant;

int main()
{

  std::string src{ "123" };

  auto first = std::cbegin(src);
  auto const last = std::cend(src);


  int result;
  bool success = x3::phrase\_parse(first, last, constant, x3::ascii::space, result);

  if (!success || first != last) {
    // parse failed
    std::cerr << "Parse failed." << std::endl;
  } else {
    std::cout << "Parse: " << result << std::endl;
  }
}

```

`namespace detail` の中身がパーサの定義になっています．
(`namespace` を分けたのは，`constant` そのものの名前以外を隠すためです．)
`X3` では，`x3::rule<class, result type>` というテンプレートクラスがパーサルールになります．`class` 部分は今は前方宣言だけで構わないようです．(後で `on_error` とかの属性を付与する際に必要になる？)

`result type` はそのパーサが返すべき値になります．

つまり，`x3::rule<class constant, int> const constant;` は，`int` 型の値を返す，特別な属性を持たない `constant` という名前のパーサを宣言したことになります．

そして，その実装は `constant_def` という名前で与えられます．

`hogehoge_def` という名前にする規約のようです．(`BOOST_SPIRIT_DEFINE` 部分を書き換えれば違う名前にしても大丈夫のようだが，素直に従っておけば良さそう)

今回はシンプルに `x3::int_` そのものとしています．

最後に `BOOST_SPIRIT_DEFINE` することで，`constant` というパーサの宣言と，`constant_def` という実装をひも付けます．

使い方は今までと全く同じです．

## 使ってみる

セマンティックアクションと名前付きパーサの両方を使って，整数を受け取って 2 倍にした値を返すパーサを書いてみます．

```
#include <boost/spirit/home/x3.hpp>

#include <iostream>
#include <string>


namespace x3 = boost::spirit::x3;

namespace detail {

  auto const twice = [](auto& ctx) { \_val(ctx) = \_attr(ctx) * 2; };

  x3::rule<class constant, int> const constant;

  auto const constant\_def = x3::int\_[twice];

  BOOST\_SPIRIT\_DEFINE(constant);

} // namespace detail

using detail::constant;

int main()
{

  std::string src{ "123" };

  auto first = std::cbegin(src);
  auto const last = std::cend(src);


  int result;
  bool success = x3::phrase\_parse(first, last, constant, x3::ascii::space, result);

  if (!success || first != last) {
    // parse failed
    std::cerr << "Parse failed." << std::endl;
  } else {
    std::cout << "Parse: " << result << std::endl;
  }
}

```

セマンティックアクション内では，パーサの `result type` に `_val(ctx)` でアクセス出来ます．
`_val(ctx)` は参照を返すので，ここに適当な値を代入してやれば，パーサの返り値にすることが出来ます．

`_attr(ctx)` は先程と同じです．`x3::int_` の attribute は `int` です．

実行してみると，`Parse: 246` が返るはずです．

## 一旦まとめ

セマンティックアクションと名前付きパーサの宣言と定義をまとめました．

`Qi` のころより，セマンティックアクションはかなり書きやすくなっている気がします．

今回の例では，`_attr(ctx)` が単純な値だったのでわかりやすいですが，`x3::int_ >> x3::double_` の `_attr(ctx)` は `Boost.Fusion` が登場したりしてちょっとややこしそうです．

また，再帰的パーサを定義できるようにパーサの宣言をまとめたのに，再帰的パーサを書いていませんが，これは別記事に電卓でも作ってまとめたいと思います．

---

---
