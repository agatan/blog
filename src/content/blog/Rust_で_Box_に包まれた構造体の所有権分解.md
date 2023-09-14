---
title: "Rust で Box に包まれた構造体の所有権分解"
pubDatetime: 2016-12-04T07:45:12.000Z
tags: ["Rust"]
---

ちょっとはまったのでメモ

```
struct A {
    foo: Vec<i32>,
    bar: Vec<bool>,
}

```

こんな構造体があったとする。
普通、`A` の所有権を分解して `foo` と `bar` にしたいときは

```
fn xxx(x: A) -> (Vec<i32>, Vec<bool>) {
    let A { foo, bar } = x;
    (foo, bar)
}

```

とやれば良い（この例だともっと簡単に書ける気もするけど）

一方、`Box<A>` から `foo` と `bar` に分解したい場合は話が変わってくる。

```
fn error1(x: Box<A>) -> (Vec<i32>, Vec<bool>) {
    (x.foo, x.bar)
}

fn error2(x: Box<A>) -> (Vec<i32>, Vec<bool>) {
    let A { foo, bar } = *x;
    (foo, bar)
}
```

これらは両方共コンパイルできない。
人間から見ると，`Box<A>` は `A` の所有権を持っているのだから、`A` -> `foo/bar` に分解できるなら `Box<A>` も同様にできる気がする。

実際にはこのようにするとコンパイルが通る。

```
fn success(x: Box<A>) -> (Vec<i32>, Vec<bool>) {
    let x = *x;
    let A { foo, bar } = x;
    (foo, bar)
}
```

うーん、エラーになるケースだと `Deref` トレイトの機能を経由している感じになるのかな？
`Deref` 経由で `foo` の所有権をとるとその時点で `Box<A>` の所有権は奪われちゃうから `bar` の所有権が取れないということなのだと想像した。
`success` のようなコードが突然出てきたら混乱しそうだ。

---

---
