---
title: "C++ で result 型を作る"
pubDatetime: 2016-07-01T14:30:09.000Z
tags: ["C++"]
---

Haskell や Rust など多くの強力な型システムを持つプログラミング言語は、`Either` とか `Result` といった「失敗するかもしれない」計算の値を示す型を持っています。

現在の C++ の標準ライブラリにはこのような型はありませんので、それを自作してみようというわけです。

ちなみに現在策定中の C++17 には `std::optional` が入ることが決定しているようです。これは、`result` と同様に失敗するかもしれな値を示しますが、失敗した原因がなんなのかを通知する仕組みを持っていません。

## そもそもどういう型か

Rust の `Result` 型を例にみてみます。

```
enum Result<V, E> {
  Ok(V),
  Err(E),
}

```

Rust における `Result` 型はだいたいこんな感じで定義されています。

`Result` は型引数を２つとります。`V` が成功時の値で、`E` が失敗時のエラー情報です。
例えば、`fn parse_int(s: &str) -> Result<isize, String>;` は、文字列を受け取り、それが整数としてパース出来れば `isize` に変換し、`Ok(isize)` として返します。
もし整数としてパース出来ないなどのエラーがあれば、それを `String` で表現し、`Err(String)` で返します。

本質的にはこれが全てです。ここに、`Result` から中身を取り出す(`Err` なら `panic` する)関数などを定義してあげれば便利にエラー状態を表現できます。

(Rust の `try` マクロはとても便利ですよね)

## まずはベースとなる result を作る

まずはベースとなる `result` 型を作ってみます。

```
template <typename T, typename E>
struct result {

  result(T const& ok) : t(tag::OK) {
    ok\_ = ok;
  }

  result(E const& e) : t(tag::OK) {
    err\_ = e;
  }

  ~result() {
    switch (t) {
      case tag::OK:
        ok\_.~T();
        break;
      case tag::ERROR:
        err\_.~E();
        break;
    }
  }

  result(result const& r): t(r.t) {
    switch (t) {
      case tag::OK:
        ok\_ = r.ok\_;
        break;
      case tag::ERROR:
        err\_ = r.err\_;
        break;
    }
  }

  T const& get() const {
    if (t != tag::OK) {
      throw "invalid get operation";
    }
    return ok\_;
  }

  E const& get\_error() const {
    if (t != tag::ERROR) {
      throw "invalid get operation";
    }
    return err\_;
  }

private:
  enum class tag {
    OK,
    ERROR,
  };
  tag t;
  union {
    T ok\_;
    E err\_;
  };

};

```

かなり雑ですが、ざっくりこんな感じになるはずです。
C++11 から拡張されて自由度がかなり高くなった `union` がとても便利です。

これで `result<int, std::string>(1).get()` とやれば `1` が返るし `result<int, std::string>(std::string("test")).get_error()` で `"test"` が返るはずです。

## C++ でやると何が難しいか

C++ で難しいのは、Rust より弱い型推論が引き起こす問題です。

Rust では、`Ok(1isize)` とか `Err("error!".to_owned())` とすれば、その値がどういう型であることが期待されているのかまで含めて型推論や単一化が行われます。
すなわち、`Ok(1isize)` だけを見てもエラーの型がわからないため、`Result<isize, E>` の `E` を決定することが出来ないが、Rust は強力な型推論機構を持つため、これを決定することが出来ます。

一方、C++ では `result<int, std::string> f() { return 1; }` は `int` から `result<int, std::string>` の暗黙変換がきくので可能ですが、`result<int, int>` などとした瞬間、暗黙変換に頼ることはできなくなります。
そこで、出来れば `ok(1)` とか `err("test")` という感じにしたいのですが、これは一筋縄では行きません。

```
template <typename T, typename E>
result<T, E> ok(T);

```

これだと `T` は推論されても `E` が推論されないので、`ok<int, std::string>(1)` などとしなければなりません。これは使いづらすぎます。

## じゃあどうするか

先ほどとは違う形ですが、やっぱり暗黙の型変換を応用します。

要するに `ok` を表す型と `error` を表す型を区別しつつ、`result<V, E>` とはなるべくシームレスに変換をしたいというわけですから、それぞれ専用の型を作ってしまえば良いのです。

```
template <typename T>
struct ok\_value {
  explicit ok\_value(T t): t(t) {}

  template <typename V, typename E>
  operator result<V, E> () const;

private:
  T t;
};

template <typename T>
template <typename V, typename E>
ok\_value<T>::operator result<V, E> () const {
  return result<V, E>(t);
}

template <typename T>
ok\_value<T> ok(T t) {
  return ok\_value<T>(t);
}

```

`ok` 側だけ示しました。

`ok` 関数はテンプレートになっており、`T` 型の値をとって `ok_value<T>` を返します。（本当は値渡し以外にも対応すべきですが、簡単のために値渡しだけ実装しています）

`ok_value<T>` は型変換演算子 `operator result<V, E>() const` を持ちます。これによって `ok_value` から `result` への暗黙変換が可能になります。

`ok_value<T>` は `result<T, E>` に変換出来れば良さそうに見えるのですが、それでは不十分です。

`ok("test")` は `ok_value<const char*>` を返します。`ok_value<T> -> result<T, E>` の変換しか提供していない場合は、`result<std::string, E>` への変換ができなくなってしまいます。これは不便ですよね。

そこで新たにテンプレート引数を導入することでこれを解決しています。もっときちんとやるなら `std::is_constructible` などを使ってチェックをするべきだとは思いますが。

`error` 側もほぼ同様のコードを書いてやれば、

```
result<int, std::string> parse\_digit(char c) {
  if (c < '0' || '9' < c) {
    return error("invalid character");
  }
  return ok(c - '0');
}

```

というように書けます。

## まとめ

`T` から `result<T, E>` への暗黙変換を許すという方針も全然ありだとは思いますが、個人的に Rust などで `ok` なら `ok` と明示するスタイルに慣れているので、こっちのほうが気に入っています。

明らかに正常に値を返していそうな感じがコードにあらわれて好きです。

暗黙の型変換って危険だしあまり良いイメージはないと思うのですが、やっぱりあれば便利ですね。
C++ を使っている時点で気を抜いたら死なので、「取り扱いを把握して全力で注意しながら使えば危険じゃない」という気持ちで便利に使いたいものです。

---

---
