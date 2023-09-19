---
title: "Rust の Result と Iterator"
postSlug: Rust_の_Result_と_Iterator
pubDatetime: 2016-09-05T14:47:25.000Z
tags: ["Rust"]
---

Rust には失敗するかもしれない値を表す `Result<T, E>` という型があります。
[std::result::Result](https://doc.rust-lang.org/std/result/enum.Result.html)

そして iterate できることを表す `Iterator` という trait があります。
[std::iter::Iterator](https://doc.rust-lang.org/std/iter/trait.Iterator.html)

また、`Iterator` trait は要素型を表す関連型を持ちます。~~例えば `String` は `Iterator<Item=char>` を `impl` しています。これは `char` 型を要素にもつ `Iterator` であることを意味します。~~
ここ間違っていました。`String` が直接 `Iterator` を `impl` しているのではありませんでした。

たまに `Iterator<Item=Result<T, E>>` のようになっている型を見かけます（T, E にはなにかしら具体的な型が入っていると思ってください）。
例えば、`std::io::stdin().bytes()` の返り値である [std::io::Bytes](https://doc.rust-lang.org/stable/std/io/struct.Bytes.html) は `Iterator<Item=Result<u8>>` を `impl` しています。
（ちょっとわかりにくいのですがここでの `Result` は `std::result::Result` ではなくて `std::io::Result` です。`std::io::Result` は `std::result::Result<T, std::io::Error>` のエイリアスです。）

さて、このような `Iterator` からすべての要素が `Ok(_)` であれば `Ok<Vec_>>` を、`Err(_)` があれば `Err<_>` を返すような処理を書きたいということは割りとよくあります。
で、これを一生懸命実装しようとしていたのですが、標準ライブラリの範囲内ですでに実装されていました。べんり。

```
let result = std::io::stdin().bytes().collect::<Result<Vec<\_>, \_>>();

```

これだけです。これで要件を満たす `Result<Vec<_>, _>` が返って来ます。すばらしい。

タネは簡単な話で `Result` が `FromIterator` trait を `impl` しているので `collect` で変換が可能であるというお話でした。
[std::iter::FromIterator](https://doc.rust-lang.org/stable/std/iter/trait.FromIterator.html)

---

---
