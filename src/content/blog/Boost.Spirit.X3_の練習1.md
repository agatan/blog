---
title: "Boost.Spirit.X3 の練習1"
postSlug: Boost.Spirit.X3_の練習1
pubDatetime: 2015-12-17T10:05:05.000Z
tags: ["C++", "Boost"]
---

# Boost.Spirit.X3 の練習 1

[Boost.Spirit.X3](http://ciere.com/cppnow15/x3_docs/) という C++ のための パーサコンビネータライブラリを使ってみています．

`Boost.Spirit` というと， C++ の黒魔術の塊みたいなイメージがあります． ちなみに `Boost.Spirit.Qi` が安定版のパーサコンビネータライブラリで， `X3` はまだ開発中のようなので注意してください．

`X3` は `Qi` と異なり，C++14 以降の規格を前提にしています．そのため，ジェネリックラムダなどを用いてよりわかりやすいプログラムが書けるようになっているようです．

`Qi` はコンパイル時間が爆発していたのですが， `X3` だと多少マシのようです．

## 第一歩

まずは猛烈にシンプルなパーサを使ってみましょう．
`X3` も `Qi` 同様に定義済みのパーサがあるので，それを単純に使用するだけのサンプルです．

```
#include <boost/spirit/home/x3.hpp>

#include <iostream>
#include <string>


int main()
{
  namespace x3 = boost::spirit::x3;

  int result;
  std::string src{ "123" };

  auto first = std::cbegin(src);
  auto const last = std::cend(src);
  bool success = x3::parse(first, last, x3::int\_, result);

  if (!success || first != last) {
    // parse failed
    std::cerr << "Parse failed." << std::endl;
  } else {
    std::cout << "Parse: " << result << std::endl;
  }
}

```

### コンパイル & 実行

```
$ clang++ -std=c++14 int\_parser.cpp
$ ./a.out
Parse: 123
```

`Boost.Spirit.X3` を使用する場合は, `#include <boost/spirit/home/x3.hpp>` とすればオッケーです．(これは使用していない機能のヘッダも読み込んでしまっているので，コンパイルは重くなります… が，これ以降使う機能を増やす度にヘッダを書き換えるのは面倒ですし，`X3` の機能の多くを使用するプログラムの場合は，大差ないと思います．)

名前空間は `boost::spirit::x3` です．頻繁に出てくるので `namespace x3` でエイリアスしました．

実際にパースしている部分は,

```
bool success = x3::parse(first, last, x3::int\_, result);

```

の部分です．

`x3::parse` は，第一引数にソース文字列の先頭イテレータ，第二引数にソース文字列の終端イテレータをとります．

第三引数が，パーサの定義です．ここでは， `x3::int_` という定義済みパーサを使用しました． これは, 整数を表す文字列を受けて，それを整数に変換するパーサです． たとえば `"1"` を `1` に変換します．整数以外にあたった場合はマッチせず，パースに失敗します．(`x3::parse` の返り値が `false` になる)
第四引数は，指定したパーサの返す値です．ここちょっと説明が難しいですね．

`x3::int_` は `int` 型の値を返すパーサです(これを `x3::int_` の attribute が `int` 型であると表現している？).

そこで，`x3::int_` の返す値を格納する変数として，`int result` の参照を渡しているという感じです．

パースが成功していれば，`result` の中身は `x3::int_` の返り値になっているはずです．

パースの成否判定は `success` と `first == last` で行います．

```
if (!success || first != last) {

```

`success` はそもそもパーサにソースがマッチしなかった場合, `false` になります．

また，`"123abc"` に `x3::int_` をマッチさせると，`"123"` だけが消費され，`"abc"` が残ります．この時，`first` は `"a"` の位置まで進んでいます．
もしソースが先頭から終端まで，パーサにマッチしていたならば，`first == last` となるはずです．

というわけでこのプログラムは，`x3::int_` に `"123"` をパースさせるプログラムでした．結果，きちんと整数の `123` が取得できていることがわかります．

## コンビネータ

`Boost.Spirit.X3` はパーサコンビネータライブラリです．個々のパーサを組み合わせてみましょう．

`" ( 1234.5)"` とか `"(67.8 ) "` にマッチして，`double` を返すようなパーサを定義してみます．

```
auto parser = x3::lit("(") >> x3::double\_ >> x3::lit(")");
bool success = x3::phrase\_parse(first, last, parser, x3::ascii::space, result);

```

まずは `parser` の定義です．
`x3::lit` はリテラルを表します．引数にとった文字列にマッチし，何も返さないパーサです．`x3::lit("(")` は `(` にマッチし，何も返さないパーサということになります．

`x3::double_` は `x3::int_` と同じく，定義済みパーサで，浮動小数点数にマッチしその値を返します．

そして重要なのが，`>>` です．

これは，「左辺にマッチした後，右辺にマッチするパーサ」を作り出すコンビネータです．

ここでは，`x3::lit("(") >> x3::double_` ですから，`(` にマッチした後，浮動小数点数にマッチするパーサ，ということになります.

通常，`>>` は左辺の返す値と右辺の返す値のタプルを返します．(`x3::int_ >> x3::double_` ならば，`int` と `double` のタプル)

しかし，左辺右辺どちらか一方が値を返さない(正確には `x3::unused_type` を返す) 場合には，もう一方の値だけを返します．

つまり，`x3::lit("(") >> x3::double_` は `double` だけを返し，`>> x3::lit(")")` と続けても `double` だけが返ります．

次に，空白の読み飛ばしについてです．

```
bool success = x3::phrase\_parse(first, last, parser, x3::ascii::space, result);

```

ひとつ目の例と異なり，`x3::parse` ではなく `x3::phrase_parse` を使っています．

こちらは，`attribute` を示す最後の引数の前に，スキップパーサを取ります．
スキップパーサとは，文字通り，スキップしてほしい文字列にマッチするパーサです．

`x3::ascii::space` は定義済みのパーサで，スペース，改行文字，タブにマッチします．したがって，これらの文字はスキップされます．
スキップ判定のタイミングは `>>` の部分です．つまり，"12 3" は `x3::int_` でパースすると, `12` までしかマッチしません．`x3::int_ >> x3::int_` とすることで，スペースがスキップされます．

```
#include <boost/spirit/home/x3.hpp>

#include <iostream>
#include <string>


int main()
{
  namespace x3 = boost::spirit::x3;

  std::string src{ "( 123.4 )" };

  auto first = std::cbegin(src);
  auto const last = std::cend(src);
  double result;
  auto parser = x3::lit("(") >> x3::double\_ >> x3::lit(")");
  bool success = x3::phrase\_parse(first, last, parser, x3::ascii::space, result);

  if (!success || first != last) {
    // parse failed
    std::cerr << "Parse failed." << std::endl;
  } else {
    std::cout << "Parse: " << result << std::endl;
  }
}

```

ちなみに，`x3::lit("(") >> x3::double_ >> x3::lit(")")` の部分ですが，`operator<<` の引数の内，片方がパーサであれば，`char const*` から暗黙変換が働くので， `"(" >> x3::double_ >> ")"` と書くことが出来ます．

コンビネータは他にもあります．

- `|`
  - `a | b` で，`a` にマッチするか `b` にマッチするか
  - `a` を先に試すので，両方にマッチする場合でも `a` にマッチしたことになる (PEG の特徴ですね)
  - 返り値は `boost::variant<a, b>` です．`a` と `b` が同じ型を返す場合はその型を返します
- `>`
  - `>>` とほぼ同じです．
  - `a >> b` は `a` にマッチして `b` にマッチしなかった場合，`a` の前にバックトラックします.
  - `a > b` はその場合，即座にパース失敗を通知します．
- `*`
  - `*a` という形で使う
  - `a` の 0 回以上の繰り返し
  - 返り値は `std::vector<a>`
- `+`
  - `+a` という形で使う
  - `*` の一回以上繰り返し版
- `-`
  - `-a` という形で使う
  - `a` が来ても来なくても良いというパーサ
  - 返り値は `boost::optional<a>`

...

`>` と `>>` の違いはわかりにくいですね．

`(x3::lit("(") >> ")") | ("(" >> x3::int_ >> ")")` に `"(123)"` を食わせると，`|` の後半部分にマッチしてくれます．
一方, `>>` を `>` に置換えた場合，ソース先頭の`"("` が`x3::lit("(")` にマッチするにもかかわらず，その直後に`")"` が来ていないため，その時点でエラーを通知してしまいます．

## 一旦むすび

とりあえず最も基本的な部分をまとめてみました．

ここまでは `Qi` と同じなんですよね．

セマンティックアクションや `on_error` などの扱いががっつり変わっているようなので，一旦ここで切って，それぞれ調べてからまとめたいと思います．
何か間違い等あればぜひご指摘お願いします．

---

---
