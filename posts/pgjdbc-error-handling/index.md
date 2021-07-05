---
date: 2021-06-23
tags:
  - Java
  - Kotlin
---

最近になって仕事で初めて JVM 上で動く言語を書いています。
そこでちょっとハマったことがあり、色々調べて解決したのでメモがてら共有しようと思い、記事にすることにしました。
もっと遭遇している人多そうなんですが、パッとググった限り日本語でこの問題について言及している記事が見当たらなかったので、有用だと信じています。（経験ある JDBC ユーザにとっては当たり前なんですかね？）

## TL;DR

https://github.com/pgjdbc/pgjdbc を使っていると、 `java.sql.SQLException.getErrorCode()` が常に 0 になる。
代わりに `java.sql.SQLException.getSQLState()` を使おう。

## 問題

UNIQUE制約をはったテーブルに対して「レコードがすでにあればそれを SELECT する。なければ新規に作る。」という操作をしたいことがあります。
そういったときに、文字通り「1. まず SELECT し、 2. なかったら INSERT」というふうに実装してしまうと、同時に二つ以上のリクエストが処理されると想定通りの挙動になりません。（1. と 2. の間に別のリクエストによって 2. が実行されるかもしれない。）
そこで、こういったケースでは「1. まず INSERT を試み、 2. UNIQUE制約に引っ掛かったら SELECT」というふうに実装するのが正しいです。（Rails 的にいうと `find_or_create_by` ではなく、 `create_or_find_by` しよう、という話です。）

```kotlin
val record = try {
    Users.insertAndGet(...)
} catch (ex: SQLException) {
    if ( /* ex が UNIQUE VIOLATION である */ ) {
	    Users.select(...)
	} else {
	    throw ex
	}
}
```

こんな感じです。（疑似コードですが）

さて、ここで `/* ex が UNIQUE VIOLATION である */` ことの確認をする方法が必要です。
[`java.sql.SQLException` のドキュメント](https://docs.oracle.com/en/java/javase/13/docs/api/java.sql/java/sql/SQLException.html) を見てみると、`int getErrorCode()` という API があるので、これを使ってみます。vendor-specific exception code が取得できる、と説明があります。
[PostgreSQL のエラーコード一覧](https://www.postgresql.org/docs/13/errcodes-appendix.html) によると、23505 が `unique_violation` らしいので、以下のようなコードで判定できるように見えます。

```kotlin
const val UNIQUE_VIOLATION = 23505
if (ex.errorCode == UNIQUE_VIOLATION) {
    ...
}
```

## 動かない！

これでテスト書いてうまく動くことを確かめよう〜と思ったら、なぜか全然テストが通らない！
しかも `SQLException` が投げられていて、その内容が完全に UNIQUE 制約に引っ掛かっているというエラーでした。
何事...

デバッガで追ってみると、 `ex.errorCode == UNIQUE_VIOLATION` が false になっているようです。エラーは確かに `unique_violation` だというのに。

さらにデバッガでよく見てみると、なんと `ex.errorCode` が 0 になっています。
何事...

## pgjdbc は getErrorCode に対応していなかった

[PSQLException の実装](https://github.com/pgjdbc/pgjdbc/blob/master/pgjdbc/src/main/java/org/postgresql/util/PSQLException.java) をよく見てみると、そもそも `getErrorCode` の定義がありません。どうやら常にデフォルトの 0 を返すようです。

https://github.com/pgjdbc/pgjdbc/pull/623 で対応が試みられていますが Close されています。
PostgreSQL のエラーコードはアルファベットを含むものもあり、全てを統一的に `getErrorCode` で返すことができないというのが理由でした。
言われてみれば当たり前だし、確かによくみると [PostgreSQL のエラーコード一覧](https://www.postgresql.org/docs/13/errcodes-appendix.html) にはちらほらアルファベットがありますね...

## 対応

`String SQLException.getSQLState()` を使います。
さらに [PSQLState](https://github.com/pgjdbc/pgjdbc/blob/master/pgjdbc/src/main/java/org/postgresql/util/PSQLState.java) という enum が定義されているので、これを併用して

```kotlin
if (ex.sqlState == PSQLState.UNIQUE\_VIOLATION.state) {
    ...
}
```

とすれば期待通りの挙動になります。

## まとめ

しかしこの挙動、もうちょっとドキュメントとかに書いてくれていてもいいんじゃないかなぁと思ったんですがどうなんでしょう。
`PSQLException` のドキュメントを見ても何も書いていないんですよね。
PostgreSQL のドキュメントをちゃんと見れば Error Code が int で表現しきれないことは自明だ、と言われればそれはそうなんですが。

pgjdbc の話からは逸れますが、こういう「統一インターフェースを標準で提供するから内部実装は各自ライブラリでやって差し替える」系のライブラリ、統一インターフェースの方のドキュメントばかり読んでしまって個々のライブラリのドキュメントをあまり読まなくても使えてしまうので、こういう罠があるとハマりますね。よくできているということでもあると思います。
（まぁ今回は個々のライブラリのドキュメントを見ても何も書いていないと思うのですが...）

pgjdbc には別の問題でもハマっていて、そっちは PR 出して無事マージ & リリースされたので、いつかその問題についても書いてみようと思います。
