---
title: "Rust のパーサコンビネータライブラリ combine を使う時の tips"
date: 2016-05-14T16:03:19.000Z
tags: ["Rust"]
---

Rust のパーサコンビネータライブラリの一つである [Marwes/combine: A parser combinator library for Rust](https://github.com/Marwes/combine) を使ってみています．

詳しい使い方はきちんとしたドキュメントがあるのでそちらを参照してください．

ざっくりいうと Haskell の [parsec: Monadic parser combinators | Hackage](https://hackage.haskell.org/package/parsec) の Rust 版という感じです．

(ちなみに私も combine を参考に C++ でパーサコンビネータを作ってみたりしました. )
[agtn.hatenablog.com](http://agtn.hatenablog.com/entry/2016/04/30/003009)

で、このライブラリ、とてもジェネリックなコードで書かれているので、かなりコンパイル時間が増加します… (Boost.Spirit 系に近いです． コンパイルエラーなどは遥かに読みやすいのであまり困ることはないですが)

パーサを書いている時にはテストは頻繁に行いたいので，ちょっとコンパイルがおそいのはつらい．

なにか解決策はないかなぁと思っていたら本家に issue がたっていました．

[Extremely long compile times · Issue #21 · Marwes/combine](https://github.com/Marwes/combine/issues/21)

今回この issue にかかれていた内容を検証してみたので，ここでまとめておこうと思います．

## 結論

パーサの定義を，ジェネリックな構造体のメソッドとして定義するとコンパイル時間が大幅に短くなる

## 方法

まずはじめに言われているのは，入力ストリーム型を`I: Stream<Item=char>` から `&str` にしてしまうという方法です．

> (It might be possible to specialize the parsers directly as well, say
> `fn expr(input: State<&str>) -> ParseResult<Expr, &str>`
> instead of
> `fn expr<I: Stream>(input: State<I>) -> ParseResult<I, &str>`
> )

これは作ったパーサをジェネリックな入力に対して適用することができなくなりますが，ライブラリの利用者側としては，`char` のストリームといったらだいたい `&str` だと思うので，ぶっちゃけジェネリックじゃなくてもいいじゃんという感じです．

そしてもう一つが, ジェネリックな構造体を作って，パーサの定義をその中に閉じ込めるという方法です．

ちょっとこちらはコード例を実際に見たほうがわかりやすいと思うので後で説明します．

## 実験コード

というわけで，

1. ジェネリックなパーサ
2. &str のみを受理するパーサ
3. ジェネリックな構造体の中に定義されたジェネリックでないパーサ

の三種類について，コンパイル時間をはかってみます．

パーサ界のハローワールド，計算機で実験してみます．
まずはジェネリックなパーサです．

```
use combine::*;
use combine::primitives::Stream;

pub fn integer<I>(input: State<I>) -> ParseResult<i64, I>
    where I: Stream<Item = char>
{
    many1::<Vec<\_>, \_>(digit())
        .map(|is| is.into\_iter().fold(0, |lhs, rhs| lhs + (rhs as i64 - '0' as i64)))
        .parse\_state(input)
}

pub fn factor<I>(input: State<I>) -> ParseResult<i64, I>
    where I: Stream<Item = char>
{
    between(char('('), char(')'), parser(expr)).or(parser(integer)).parse\_state(input)
}

pub fn term<I>(input: State<I>) -> ParseResult<i64, I>
    where I: Stream<Item = char>
{
    let op = char('*').or(char('/')).map(|c| {
        move |lhs, rhs| match c {
            '*' => lhs * rhs,
            '/' => lhs / rhs,
            \_ => unreachable!(),
        }
    });
    chainl1(parser(factor), op).parse\_state(input)
}

pub fn expr<I>(input: State<I>) -> ParseResult<i64, I>
    where I: Stream<Item = char>
{
    let op = char('+').or(char('-')).map(|c| {
        move |lhs, rhs| match c {
            '+' => lhs + rhs,
            '-' => lhs - rhs,
            \_ => unreachable!(),
        }
    });
    chainl1(parser(term), op).parse\_state(input)
}

pub fn parse<I>(input: I) -> Result<(i64, I), ParseError<I>>
    where I: Stream<Item = char>
{
    parser(expr).parse(input)
}

```

それぞれの関数が一つのパーサの役割を担います．それぞれのパーサが独立していて，それぞれ別々に型変数を導入しています．

次に `&str` だけを受け取るパーサです．これは上記のジェネリックなパーサの，型変数を `&str` に置き換えるだけなのでとても簡単です．

一部だけ掲載します．

```
pub fn expr(input: State<&str>) -> ParseResult<i64, &str> {
     let op = char('+').or(char('-')).map(|c| {
        move |lhs, rhs| match c {
            '+' => lhs + rhs,
            '-' => lhs - rhs,
            \_ => unreachable!(),
        }
    });
    chainl1(parser(term), op).parse\_state(input)
}

pub fn parse(input: &str) -> Result<(i64, &str), ParseError<&str>> {
    parser(expr).parse(input)
}

```

最後が，ジェネリックな構造体のメソッド中に，ジェネリックでないパーサを定義して閉じ込める方法です．

```
use combine::*;
use combine::primitives::Stream;
use std::marker::PhantomData;

 truct P<I>(PhantomData<fn(I) -> I>);

impl<I> P<I> where I: Stream<Item = char> {
    pub fn integer(input: State<I>) -> ParseResult<i64, I> {
        many1::<Vec<\_>, \_>(digit())
            .map(|is| is.into\_iter().fold(0, |lhs, rhs| lhs + (rhs as i64 - '0' as i64)))
            .parse\_state(input)
    }

    pub fn factor(input: State<I>) -> ParseResult<i64, I> {
        between(char('('), char(')'), parser(P::<I>::expr))
            .or(parser(P::<I>::integer))
            .parse\_state(input)
    }

    pub fn term(input: State<I>) -> ParseResult<i64, I> {
        let op = char('*').or(char('/')).map(|c| {
            move |lhs, rhs| match c {
                '*' => lhs * rhs,
                '/' => lhs / rhs,
                \_ => unreachable!(),
            }
        });
        chainl1(parser(P::<I>::factor), op).parse\_state(input)
    }

    pub fn expr(input: State<I>) -> ParseResult<i64, I> {
        let op = char('+').or(char('-')).map(|c| {
            move |lhs, rhs| match c {
                '+' => lhs + rhs,
                '-' => lhs - rhs,
                \_ => unreachable!(),
            }
        });
        chainl1(parser(P::<I>::term), op).parse\_state(input)
    }
}

pub fn parse(input: &str) -> Result<(i64, &str), ParseError<&str>> {
    parser(P::expr).parse(input)
}

```

言葉で説明すると難しいのですが，型変数を導入する部分を構造体の定義部分だけにしてあげることで，型変数をそれぞれのパーサが別々に導入する必要がなくなっています．

コードも割りとすっきりしますね．

## 結果

上記をコードを `cfg` を使ってコンパイル時に切り替えながらコンパイルしてみました．

本当はきちんと繰り返し計測すべきですが，ちょっとサボっています．まぁ何度実行してもだいたい同じくらいになったので許してください．

| 実装方法       | コンパイル時間 |
| -------------- | -------------- |
| ジェネリック   | 2.666s         |
| `&str`         | 1.70s          |
| 構造体内で定義 | 1.55s          |

このような結果になりました．

つまり，先ほどの issue で述べられているコンパイル時間の短縮方法はかなり効き目があるということですね．

構造体の中に閉じ込める方法が，`&str` しか受理しないようにする方法よりもはやくコンパイルできるのは意外でした… 参照を引数にとると暗黙に lifetime 変数が導入されたと記憶しているので，その関係なのかな？

構造体内で定義する方法では，`&str` 以外の入力ストリーム型を受けつけることを可能にしつつもコンパイル時間を短縮できるということで，積極的にこの方式でパーサを定義するべきということがわかりました．

注意点として，構造体内で別のパーサを呼ぶときには，必ず `P::term` という形ではなく，`P::<I>::term` という形で呼び出すようにする必要があるようです．

きちんと明示的に指定しないと，結局型推論するはめになって意味がないということのようです．

---

COMMENT:
AUTHOR: anevaden
EMAIL: pgbsajrq@gmail.com
URL: http://cialisvipsale.com
IP: 60.173.69.118
DATE: 03/24/2018 13:14:29

Regards. Quite a lot of posts.

tadalafil 20 mg [cialis tablets australia](http://cialisvipsale.com)
interactions for cialis [http://cialisvipsale.com](http://cialisvipsale.com)
cialis 5mg prix [cialis generico](http://cialisvipsale.com)
cialis dose 30mg

---

---
