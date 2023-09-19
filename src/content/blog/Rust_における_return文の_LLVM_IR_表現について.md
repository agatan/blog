---
title: "Rust における return文の LLVM IR 表現について"
postSlug: Rust_における_return文の_LLVM_IR_表現について
pubDatetime: 2016-04-13T09:34:03.000Z
tags: ["Rust", "LLVM", "コンパイラ"]
---

- `if` 文が値を返す
- `return` 文を持つ

以上のような特徴を持つ言語はどういう感じでコンパイルされるのか知りたくて，Rust について調べてみました．

Rust では以下の様なことが出来ます．

```
fn f() {
  let x = if cond {
    return None;
  } else {
    1
  };
  ...
}

```

Scala とかもできると思います．`cond` が真だった場合は，`x` の値を返すのではなく，関数から抜けてしまうという意味です．

これを Rust ではどんな LLVM IR に落とし込んでいるのか．

# `return` 文がない場合

```
fn noreturn(x: isize) -> isize {
  x
}

```

最も単純な場合です．この場合，生成される LLVM IR は，

```
define internal i64 @\_ZN4hoge8noreturn17h811bf1a871f85432E(i64) unnamed\_addr #0 {
entry-block:
  %x = alloca i64
  store i64 %0, i64* %x
  %1 = load i64, i64* %x
  ret i64 %1
}
```

となります．
名前がマングルされていますが，上記の `noreturn` 関数です．
やっていることは単純で，第一引数を読み込んで返すだけです．

# `return` に相当する文が一つのみの場合

```
fn onereturn(x: isize) -> isize {
  let y = if x == 0 {
    1
  } else {
    x
  };
  return x;
}

```

実際に値を返す部分が一箇所しかない場合です．途中に分岐があっても最終的に一箇所になっていれば多分同じ結果になります．

```
define internal i64 @\_ZN4hoge9onereturn17h8b718f32daa6a379E(i64) unnamed\_addr #0 {
entry-block:
  %x = alloca i64
  %y = alloca i64
  store i64 %0, i64* %x
  %1 = load i64, i64* %x
  %2 = icmp eq i64 %1, 0
  br i1 %2, label %then-block-18-, label %else-block

then-block-18-:                                   ; preds = %entry-block
  store i64 1, i64* %y
  br label %join

else-block:                                       ; preds = %entry-block
  %3 = load i64, i64* %x
  store i64 %3, i64* %y
  br label %join

join:                                             ; preds = %else-block, %then-block-18-
  %4 = load i64, i64* %x
  br label %clean\_ast\_10\_

return:                                           ; preds = %clean\_ast\_10\_
  ret i64 %4

clean\_ast\_10\_:                                    ; preds = %join
  br label %return
}
```

`return` という BasicBlock ができています．これは `return` 文が現れると作られるよう？です．
で，その中では単純に `x` に該当する値を返しています．

最後の `return x;` 文を 単純に `x` に置き換えてみると，

```
define internal i64 @\_ZN4hoge9onereturn17h8b718f32daa6a379E(i64) unnamed\_addr #0 {
entry-block:
  %x = alloca i64
  %y = alloca i64
  store i64 %0, i64* %x
  %1 = load i64, i64* %x
  %2 = icmp eq i64 %1, 0
  br i1 %2, label %then-block-18-, label %else-block

then-block-18-:                                   ; preds = %entry-block
  store i64 1, i64* %y
  br label %join

else-block:                                       ; preds = %entry-block
  %3 = load i64, i64* %x
  store i64 %3, i64* %y
  br label %join

join:                                             ; preds = %else-block, %then-block-18-
  %4 = load i64, i64* %x
  ret i64 %4
}
```

となります． `return` ブロックが消えていますね．なので `return` 文があると `return` ブロックが作られる、で良さそう？

# 複数のパスから値を返す

```
fn multireturn(x: isize) -> isize {
  let y = if x == 0 {
    return -1;
  } else {
    x
  };
  y
}

```

さて，では最初に述べた，`if` の分岐内にある `return` についてです．
これは，

```
define internal i64 @\_ZN4hoge11multireturn17had379e8ce5a18f08E(i64) unnamed\_addr #0 {
entry-block:
  %sret\_slot = alloca i64
  %x = alloca i64
  %y = alloca i64
  store i64 %0, i64* %x
  %1 = load i64, i64* %x
  %2 = icmp eq i64 %1, 0
  br i1 %2, label %then-block-18-, label %else-block

then-block-18-:                                   ; preds = %entry-block
  store i64 -1, i64* %sret\_slot
  br label %return

else-block:                                       ; preds = %entry-block
  %3 = load i64, i64* %x
  store i64 %3, i64* %y
  br label %join

join:                                             ; preds = %else-block
  %4 = load i64, i64* %y
  store i64 %4, i64* %sret\_slot
  br label %return

return:                                           ; preds = %join, %then-block-18-
  %5 = load i64, i64* %sret\_slot
  ret i64 %5
}
```

こうなりました．
まず，`return` 文があるため？，`return` ブロックが作られています．
しかし今回は，パスによって返すものが違います．(値が違うという意味ではなく，同じ変数ですらないという意味です...)

よく IR を読むと，関数の頭で `%sret_slot` という名前でスタック領域を確保していることがわかります．
そして，`return` ブロック内では，これを読んできて返しています．

さらに，`if` 文の then 節にあたる，`then-block-18-` というブロックでは，`%sret_slot` に値を格納して `return` ブロックへジャンプしています．
else 節のあとの部分 (`join` ブロック) でも同様に, `%sret_slot` に値を格納して `return` ブロックへジャンプしています．

# まとめ

というわけで，様々な Rust コードを LLVM IR に変換して見てみた結果，複数のパスから値を返す場合は，「ローカル変数として返り値を定義し，そこに返したい値を格納してから `return` に goto」という形になっていることがわかりました．

(ほとんど LLVM IR を乗っけるだけになってしまった...)

## ちなみに ...

# `if` 文の返す値をそのまま返す

```
fn ifreturn(x: isize) -> isize {
  if x == 0 {
    1
  } else {
    x
  }
}

```

Rust に慣れていないとちょっとわかりにくいですが，`x == 0` の場合は 1 を返し，そうでない場合は `x` を返す関数です．

これは，

```
define internal i64 @\_ZN4hoge8ifreturn17hcdaab6e376d6c95cE(i64) unnamed\_addr #0 {
entry-block:
  %sret\_slot = alloca i64
  %x = alloca i64
  store i64 %0, i64* %x
  %1 = load i64, i64* %x
  %2 = icmp eq i64 %1, 0
  br i1 %2, label %then-block-15-, label %else-block

then-block-15-:                                   ; preds = %entry-block
  store i64 1, i64* %sret\_slot
  br label %join

else-block:                                       ; preds = %entry-block
  %3 = load i64, i64* %x
  store i64 %3, i64* %sret\_slot
  br label %join

join:                                             ; preds = %else-block, %then-block-15-
  %4 = load i64, i64* %sret\_slot
  ret i64 %4
}
```

こうなります．やっていることは上記の例たちとあまり変わりません．
しかし，`return` 文がないので？，`return` ブロックが作られていません．が, `%sret_slot` は定義されていますね...

これはどういうことなんでしょう．`rustc` のコードを読むべきなのかもしれませんが，イマイチ内部処理が想像しにくいです...

普通に翻訳していったら，

```
let x = if x == 0 { 1 } else { x };
x
```

と同じ感じになる気がするので，`%sret_slot` という名前が出てくる余地は無い気がするのですが...(実質同じ処理ではあります)
分岐が直接返戻値になる場合は特別扱いしているのかな？

---

---
