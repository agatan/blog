---
title: "C++テンプレートイディオム CRTP"
date: 2016-06-16T10:27:08.000Z
tags: ["C++"]
---

C++テンプレートの有名なイディオムとして、CRTP というものがあります。
今回はそれについて。
複雑な部分特殊化みたいな話もないですし、メリットもわかりやすい良いイディオムだと思うので、ちょっとまとめておきます。
(Control キーのことをよく CTRL と書くので、CTRP とタイポしがち）

詳細はこちらを参照してください。
[More C++ Idioms/奇妙に再帰したテンプレートパターン(Curiously Recurring Template Pattern) - Wikibooks](<https://ja.wikibooks.org/wiki/More_C%2B%2B_Idioms/%E5%A5%87%E5%A6%99%E3%81%AB%E5%86%8D%E5%B8%B0%E3%81%97%E3%81%9F%E3%83%86%E3%83%B3%E3%83%97%E3%83%AC%E3%83%BC%E3%83%88%E3%83%91%E3%82%BF%E3%83%BC%E3%83%B3(Curiously_Recurring_Template_Pattern)>)

## CRTP の利点

細かい実装の話の前に、CRTP を使うと何がうれしいのかを簡単に。

**静的 Template Method パターンの実現**\_ 。これが CRTP の利点です。

Template Method パターンについてはここでは説明しませんが、大枠のアルゴリズムを共有しつつその内部で使用する実装の詳細をクラスごとに切り替えるといった目的に使われるデザインパターンです。

通常 Template Method パターンを C++ で実現しようと思うとどうしても仮想関数を使うことになると思います。これによって実行時のオーバヘッドがかかってしまいます。
Template Method パターンは、いわゆるクラスベースの動的なポリモーフィズムを必要としないクラスであっても、アルゴリズムの共有やボイラープレートコードの削減に非常に有用なパターンです。
これを静的に実現するのが CRTP の目的なのです。

## 実装

CRTP とは、その名の通り、奇妙に再帰したテンプレートのパターンのことです。
情報量０の文章ですね。実際のコードも見たほうがわかりやすいと思います。

以下に、`compare` というメソッドから比較演算子を _derive_ (自動導出) する例をのせます。

```
#include <iostream>

template <typename T>
struct comparable {
  template <typename U>
  friend bool operator==(comparable const& lhs, U const& rhs) {
    return static\_cast<T const&>(lhs).compare(rhs) == 0;
  }

  template <typename U>
  friend bool operator>(comparable const& lhs, U const& rhs) {
    return static\_cast<T const&>(lhs).compare(rhs) > 0;
  }

  template <typename U>
  friend bool operator<(comparable const& lhs, U const& rhs) {
    return static\_cast<T const&>(lhs).compare(rhs) < 0;
  }
};

struct person : comparable<person> {
  int age;

  int compare(person const& rhs) const {
    return age - rhs.age;
  }
};

int main() {
  person p1, p2;
  p1.age = 10;
  p2.age = 100;

  std::cout << std::boolalpha << (p1 == p2) << std::endl;
  std::cout << std::boolalpha << (p1 < p2) << std::endl;
  std::cout << std::boolalpha << (p1 > p2) << std::endl;
}

```

`person` クラスには `operator==` など定義していないにもかかわらず、`person` を比較することが出来ています。

CRTP の中心となるのは `struct person : comparable<person>` という部分です。
クラスを定義する際に、自分をテンプレート引数にとるクラスを継承するというコード、これこそが「奇妙な再帰」なのです。
実際、`struct person : comparable<person>` の部分ではまだ `person` がどんな実装になるかはわかっていません。奇妙ですね。

さて、まずは `person` の中身を見てみます。
`person` では、`person const&` を引数にとり、それが自分より大きければ正の値を、小さければ負の値を、等しければ 0 を返すような、`compare` というメソッドを定義しています。
`person` の仕事はこれだけです。

`comparable` は、テンプレート引数にひとつの型をとります。

`comparable` はその型が `compare` というメソッドをもつことを期待しています。(暗黙のインターフェース)

`comparable` の仕事は `compare` というひとつのメソッドから、`operator==`, `operator<`, `operator>` を自動的に導くことです。

Template Method パターンをご存じの方ならすんなり理解できるかと思います。

CRTP のすごいところは、仮想関数をまったく使わないことです。つまり、実行時のテーブルルックアップは発生しません。すべてが静的に決定されるのです。

## おまけ

先に挙げた `compare` から `operator==` を自動導出する例ですが、Haskell の `Ord` 型クラスを意識しています。

```
class Eq a => Ord a where
  compare :: a -> a -> Ordering
  (<), (<=), (>), (<=) :: a -> a -> Bool
  max, min :: a -> a -> a

```

`Ord` 型クラスのインスタンスになるためには、最低でも `compare` を実装している必要があります。
逆にいえば、`compare` だけ実装すれば、他の関数は自動的に実装されます。

Haskell では、`Ord` になるためには `Eq` のインスタンスである必要があります。
これを C++ で表現するためには、

```
template <typename T>
struct ord : eq<T> {
  ...
};

```

こんな感じでしょうか。もちろん型クラスの代替にはなりえないんですけどね。
Haskell の型クラスの利点のひとつである、最小限のインターフェース実装による関数の自動導出っぽいこともできるよというお話でした。

実際にテンプレートライブラリを書いてみて改めて有用性がわかったテクニックでした。
拙作の coco にも導入したい... すべてのパーサにユーティリティメンバ関数を追加するみたいなことが出来るはず... いつかやります。

---

---
