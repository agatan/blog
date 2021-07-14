---
title: "C++ でパーサコンビネータを書きました"
date: 2016-04-29T15:30:09.000Z
tags: ["C++"]
---

C++ で構文解析といえば，Boost.Spirit や yacc 系などが有名ですが，どうにも使うの辛かったので作りました．

### 2016/05/01 追記

いろいろ更新しました．肯定先読み以外はプリミティブも実装し終わっているかと思います．

ドキュメントはまだ無いのですが，すべての機能についてテストは書いてあるので，それを見てもらえればなんとか使い方もわかるかと思います．

[agatan/coco](https://github.com/agatan/coco)

`coco::combix` がパーサコンビネータライブラリの namespace です．

Boost.Spirit は高機能かつ高性能なんですが，かなり変態的な構文で記述する必要があり(まぁ C++ なんですけど)，さらにその性能や便利さ，構文のために異常なまでにテンプレートを多用しています．私は構文解析後の構文木の構築に Boost.Variant を使ってみているのですが，Boost.Spirit と Boost.Variant の両面から，ジェネリックすぎるがゆえのコンパイルエラー爆発攻撃を食らって本当に辛いです．

そこで，Haskell の [parsec](https://hackage.haskell.org/package/parsec) や Rust の [combine](https://github.com/Marwes/combine) を参考にしつつ，C++ でパーサコンビネータを書いてみました．(実際これを使ってもコンパイルエラーは割りと発狂しますが)

## 例

例となるコードは [agatan/coco-combix-demo](https://github.com/agatan/coco-combix-demo) においてあります．

ドキュメントもないので，なんとなく雰囲気だけコードから読み取る必要があります．(例に出ていない機能もちょいちょい実装されてしまっています．)

以下にちょっと簡略版のコードを載せてみます．ありがちな電卓です．AST を作らず直接計算しています．

```
#include <string>
#include <iostream>
#include <functional>

#include <coco/combix.hpp>

namespace cbx = coco::combix;

using stream\_type = cbx::iterator\_stream<std::string::const\_iterator>;

cbx::parser<int, stream\_type> expression();

cbx::parser<int, stream\_type> number() {
  return cbx::expected(cbx::map(cbx::many1(cbx::digit()),
                                [](auto&& is) {
                                  int acc = 0;
                                  for (auto i : is) {
                                    acc = acc * 10 + i;
                                  }
                                  return acc;
                                }),
                       "integer number");
}

cbx::parser<int, stream\_type> factor() {
  return cbx::choice(
      number(),
      cbx::between(cbx::skip(cbx::token('('), cbx::spaces()),
                   cbx::skip(cbx::token(')'), cbx::spaces()),
                   cbx::skip(cbx::lazy\_fun(expression), cbx::spaces())));
}

cbx::parser<int, stream\_type> term() {
  auto op = cbx::map(
      cbx::skip(cbx::choice(cbx::token('*'), cbx::token('/')), cbx::spaces()),
      [](auto c) -> std::function<int(int, int)> {
        if (c == '*') {
          return std::multiplies<int>();
        } else {
          return std::divides<int>();
        }
      });
  return cbx::chainl1(cbx::skip(factor(), cbx::spaces()), op);
}

cbx::parser<int, stream\_type> expression() {
  auto op = cbx::map(
      cbx::skip(cbx::choice(cbx::token('+'), cbx::token('-')), cbx::spaces()),
      [](auto c) -> std::function<int(int, int)> {
        if (c == '+') {
          return std::plus<int>();
        } else {
          return std::minus<int>();
        }
      });
  return cbx::chainl1(cbx::skip(term(), cbx::spaces()), op);
}

int main() {
  std::string src;
  std::getline(std::cin, src);
  auto n = number();
  auto stream = cbx::range\_stream(src);
  auto const parser = expression();
  if (auto res = cbx::parse(parser, stream)) {
    std::cout << res.unwrap() << std::endl;
  } else {
    std::cout << cbx::to\_string(res.unwrap\_error()) << std::endl;
  }
}

```

## 特徴

parsec を知っている方であれば読めるはずです...

特徴としては，多くのパーサは入力ストリームの型に依存せずに作れるようになっていることです．例えば，あらゆる入力一つを受け付け消費する `any` というパーサは，入力が `char` のストリームであろうと `int` のストリームであろうとパースを実行できるようになっています．

本来はエラーメッセージの爆発や読みづらさを防ぐために，すべてのパーサ自体にストリームの型をひも付けたかったのですが，そうすると，`any` を使うたびに，`any<cbx::iterator_stream<typename std::vector<int>::const_iterator>>()` とか `any<cbx::iterator_stream<std::string::const_iterator>>()` とかしなくてはなりません．これは Haskell や Rust と違って C++ の型推論が限定的であるためです．(Haskell や Rust では後でその値がどう使われているかも推論の根拠として使われます．)

そこで，パーサ自体には入力ストリームの型を指定させずに，実際にパースする部分で初めて入力ストリームの型を検査することにしました．

で，`cbx::parser<int, stream_type>` はパーサを type erasure を使ってラップします．普通に使っていると簡単に `cbx::expected<cbx::map_parser<cbx::many1_parser<cbx::digit_parser>, (lambda at ...)>>` 型とかが出てきます(`cbx::expected(cbx::map(cbx::many1(cbx::digit()), [](auto&&) {...}), "integer number")` の型です)

これを関数定義のたびに書くとか発狂してしまうので，type erasure を使って型をラップし短絡します．

ただしパフォーマンスの観点から行くとおそらく型をラップするために仮想関数を使ってしまうので，インライン展開等がきかなくなると思われます．まぁ仕方ないです．

ただ，型を膨らませすぎずに適度にラップしてやると，コンパイルエラーの内容がかなり読みやすくなるはずです．なのでなんかわからんけどエラーになるっていうときは細かくパーサを分割してラップしてやると良いかもしれません．

## まとめ

あまりにもドキュメントやコメント書かなすぎてひどいですが，ちょっと構文解析したいとかっていうときに便利だと思います．

Boost.Spirit と違って普通に C++ のプログラムとして書けます．(Boost.Spirit も C++ プログラムとして書けてはいるんですが，なんかあれはあれで別の言語を覚えているような気分になってしまったので...)

あと PEG のプリミティブをまだ完全に実装していないと思います．先読みや否定先読みが出来ません．(実装します…)

---

---
