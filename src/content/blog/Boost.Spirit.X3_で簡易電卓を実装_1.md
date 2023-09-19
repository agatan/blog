---
title: "Boost.Spirit.X3 で簡易電卓を実装 1"
postSlug: Boost.Spirit.X3_で簡易電卓を実装_1
pubDatetime: 2015-12-18T14:27:05.000Z
tags: ["C++", "Boost"]
---

[agtn.hatenablog.com](http://agtn.hatenablog.com/entry/2015/12/17/190505)
[agtn.hatenablog.com](http://agtn.hatenablog.com/entry/2015/12/17/232003)

引き続き，`Boost.Spirit.X3` です．

今回は，前回までの知識をつかって，簡易電卓を実装してみます．

## 仕様

今回定義する電卓は，

- `+`
- `-`
- `*`
- `/`

の 4 つの演算と単項の `-` をサポートします．

また，整数型のみを扱うものとします．

`(`, `)` でくくることで，演算子の結合優先順位を書き換えられ，`*` と `/` は `+` と `-` より優先されるとします．

要するに整数の四則演算のみをサポートする電卓です．

このような電卓を実装するサンプルは `Boost.Spirit.X3` 以外のライブラリ/ツールでも大量に出てくると思います．

今回は，構文解析そのものというよりは `Boost.Spirit.X3` の使い方についてメモしたいので，構文解析そのものの話はぐぐってみてください．

## パーサの骨格

演算子の結合規則をサポートするために，`primary`(定数と `()` で囲まれた式), `neg_expr`(単項 `-`), `mul_expr`(`*`, `/`), `add_expr`(`+`, `-`), `expression` というパーサをそれぞれ定義します．

先頭から順に結合強度が強くなっています．(`expression` が最弱, `primary` が最強)

`primary` は `()` で囲まれた式，つまり `"(" > expression > ")"` を受け付ける必要があり，また，`primary` 自体も `expression` の一部です．

したがって，この規則を定義するためには，再帰的なパーサを記述する必要があります．

`X3` で再帰的なパーサを記述する方法は[前回の記事](http://agtn.hatenablog.com/entry/2015/12/17/232003)にまとめました．

```
  struct primary;
  struct neg\_expr;
  struct mul\_expr;
  struct add\_expr;
  struct expression;

  x3::rule<primary, int> const primary;
  x3::rule<neg\_expr, int> const neg\_expr;
  x3::rule<mul\_expr, int> const mul\_expr;
  x3::rule<add\_expr, int> const add\_expr;
  x3::rule<expression, int> const expression;

```

それぞれのパーサは attribute として整数型を持ちます．ここに演算結果が格納されることになります．

`struct primary` などは，今は前方宣言のみで十分です．`on_error` などを実装したくなった時に定義します．

## primary

まずは `primary` を定義します.

`primary` は整数定数か， `()` で囲まれた `expression` を受理します．

```
auto const primary\_def =
    x3::int\_
  | "(" > expression > ")"
  ;

```

attribute を考慮しなければこんな感じでしょうか．`expression` は既に宣言されているので使用可能です．(`expression` の実装がこの時点で見えていなくても使用できます.)

単純に attribute を結果として返すセマンティックアクションはこの後もよく出てくるので，ヘルパとして定義しておきます．

```
namespace detail {

  decltype(auto) assign()
  {
    using x3::\_attr;
    using x3::\_val;
    return [](auto&& ctx) { \_val(ctx) = \_attr(ctx); };
  }

} // namespace detail

```

`assign` は attribute を結果に代入する関数オブジェクトを返します．

関数にする必要が特にありませんが，この後出てくるヘルパと見た目を合わせたいので関数にしました．

これを使うと，

```
auto primary\_def =
    x3::int\_[detail::assign()]
  | ("(" > expression > ")")[detail::assign()]
  ;

```

こんな感じで `primary` が定義できます．

## 単項マイナス

次に `neg_expr` を定義します．
セマンティックアクションを考えなければ，

```
auto const neg\_expr\_def =
    primary
  | "-" > primary
  ;

```

となります．

`"-" > primary` のセマンティックアクションとしては，attribute を符号反転して結果に格納するというアクションが求められます．

ここはちょっと汎用的に，attribute に関数オブジェクトを適用して結果に格納するアクションを返すような関数を定義して解決してみます．

```
namespace detail {
  template <typename F>
  decltype(auto) assign\_f(F&& func)
  {
    return [func](auto&& ctx) { \_val(ctx) = func(\_attr(ctx)); };
  }
} // namespace detail

```

`assign_f` は `assign` と異なり，関数オブジェクトを１つ引数に取ります．

そして，その関数オブジェクトを `_attr(ctx)` に適用し結果に格納します．

これを使って，`neg_expr` は

```
auto const neg\_expr\_def =
    primary[detail::assign()]
  | ("-" > primary)[detail::assign(std::negate<int>{})]
  ;

```

となります．`std::negate` は `<functional>` で定義された型で，ここでは `int` 型の値を符号反転する関数オブジェクトとして使用しています．

## 乗除

次に結合強度が強いのは `*` と `/` です．

ちょっとわかりにくいですが，セマンティックアクションを無視すれば，`mul_expr` は

```
auto const mul\_expr\_def =
    neg\_expr
    >> *(
        ("*" >> neg\_expr)
      | ("/" >> neg\_expr)
    )
  ;

```

と定義できます．`mul_expr` は `1` や `(1 + 2)`, `-1` の後に，`* 1` とか `/ -3` とか `* (1 - 2)` とかが 0 回以上現れるような式です．

`1 * -2` はちょっと気持ち悪い気もしますが… 今気がついたので許してください．

セマンティックアクションとしては，`("*" >> neg_expr)` が現れる度に，`_val(ctx)` を `_val(ctx) * _attr(ctx)` に更新すれば良いです．

始めの `neg_expr` の結果を `_val(ctx)` に格納すれば，`_val(ctx)` は常に現在の計算結果を表すことになります．`("*" >> neg_expr)` は現在の計算結果に，今処理した式(`*` の後に続く式のこと) を処理した結果をかければ良いということです．

というわけで分かりにくいとは思いますが，ほしいアクションは，

```
[](auto&& ctx) { \_val(ctx) = \_val(ctx) * \_attr(ctx); }

```

です．

さて，では `/` の場合を考えます．

`/` の場合であってもほとんどは `*` と同じであることがわかります．

ほしいアクションは

```
[](auto&& ctx) { \_val(ctx) = \_val(ctx) / \_attr(ctx); }

```

であり，`*` と `/` の違いしか有りません．

そこでこれも関数にまとめてしまいます．

```
namespace detail {

  template <typename Op>
  decltype(auto) calc\_op(Op&& op)
  {
    return [op](auto&& ctx) { \_val(ctx) = op(\_val(ctx), \_attr(ctx)); };
  }

} // namespace detail

```

こんな関数を定義して，

```
auto const mul\_expr\_def =
    neg\_expr[detail::assign()]
    >> *(
        ("*" >> neg\_expr)[detail::calc\_op(std::multiplies<int>{})]
      | ("/" >> neg\_expr)[detail::calc\_op(std::divides<int>{})]
    )
  ;

```

と使います．

`calc_op` は関数オブジェクトを引数に取り，`_val(ctx)` と `_attr(ctx)` に適用した結果を格納するアクションを返します．

`add_expr` は `mul_expr` とほぼおなじなので詳細はスキップします．

## expression

最後に `expression` です．これは単純に `add_expr` と一致します．

命名のわかりやすさと，今後拡張していく際に便利そうということで分けてあるだけです．

```
auto const expression\_def =
    add\_expr[detail::assign()]
  ;

```

## 確認

コード全体を掲載します．

```
#include <boost/spirit/home/x3.hpp>

#include <iostream>
#include <string>
#include <functional>

namespace x3 = boost::spirit::x3;

namespace grammar {

  namespace detail {

    decltype(auto) assign()
    {
      using x3::\_attr;
      using x3::\_val;
      return [](auto&& ctx) { \_val(ctx) = \_attr(ctx); };
    }

    template <typename F>
    decltype(auto) assign\_f(F&& func)
    {
      return [func](auto&& ctx) { \_val(ctx) = func(\_attr(ctx)); };
    }

    template <typename Op>
    decltype(auto) calc\_op(Op&& op)
    {
      return [op](auto&& ctx) { x3::\_val(ctx) = op(x3::\_val(ctx), x3::\_attr(ctx)); };
    }

  } // namespace detail

  struct primary;
  struct neg\_expr;
  struct mul\_expr;
  struct add\_expr;
  struct expression;

  x3::rule<primary, int> const primary;
  x3::rule<neg\_expr, int> const neg\_expr;
  x3::rule<mul\_expr, int> const mul\_expr;
  x3::rule<add\_expr, int> const add\_expr;
  x3::rule<expression, int> const expression;

  auto const primary\_def =
      x3::int\_[detail::assign()]
    | ("(" > expression > ")")[detail::assign()]
    ;

  auto const neg\_expr\_def =
      primary[detail::assign()]
    | ("-" > primary)[detail::assign\_f(std::negate<int>{})]
    ;

  auto const mul\_expr\_def =
      neg\_expr[detail::assign()]
      >> *(
          ("*" >> neg\_expr)[detail::calc\_op(std::multiplies<int>{})]
        | ("/" >> neg\_expr)[detail::calc\_op(std::divides<int>{})]
      )
    ;

  auto const add\_expr\_def =
      mul\_expr[detail::assign()]
      >> *(
          ("+" > mul\_expr)[detail::calc\_op(std::plus<int>{})]
        | ("-" > mul\_expr)[detail::calc\_op(std::minus<int>{})]
      )
    ;

  auto const expression\_def =
      add\_expr[detail::assign()]
    ;

  BOOST\_SPIRIT\_DEFINE(
      primary,
      neg\_expr,
      mul\_expr,
      add\_expr,
      expression
      );

} // namespace grammar
using grammar::expression;

int main()
{
  std::string str;
  std::getline(std::cin, str);

  auto first(std::cbegin(str));
  auto const last(std::cend(str));

  int result;
  bool success = x3::phrase\_parse(first, last, expression, x3::ascii::space, result);

  if (!success || first != last) {
    std::cerr << "Parse failed." << std::endl;
    return 1;
  }

  std::cout << "Parsed: " << result << std::endl;
  return 0;
}

```

実行してみます．

```
$ clang++ -std=c++14 main.cpp
$ ./a.out
1 + 2 * 3
Parsed: 7
$ ./a.out
(1 + 2) * 3
Parsed: 9
```

演算子の優先順位が正しく解決できていることが確認出来ます．

## まとめ

今回は，セマンティックアクションで計算自体を行ってしまいましたが，普通は抽象構文木(AST) に変換するためにセマンティックアクションを使うのが正道だと思います．

`X3` は AST のための色々を提供してくれていますが，自前で作った AST でもちょっと苦労はするかもしれませんが変換できるはずなので，時間があれば，自前 AST に変換してから実行する電卓も作ってみたいと思います．

また，AST に変換して計算する場合，AST に位置情報を付与することで，エラーレポートが便利になったりするはずです( 0 除算のエラーを通知する際，どの部分でのエラーなのかを吐いてくれればうれしいですよね).

パース失敗時にもどこで失敗したのかをレポートしてくれたほうが便利です．

`X3` で `on_error`, `on_success` を使ってこれらを実装してみようと考えています．

今回のコードでは `decltype(auto)` など，C++14 の機能を使っています．`X3` は C++14 前提のライブラリなので，迷いなくこういった機能を使用できて幸せデスね．

---

---
