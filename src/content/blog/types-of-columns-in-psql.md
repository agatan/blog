---
title: PostgreSQLでカラムの型や制約を取得する方法
postSlug: types-of-columns-in-psql
pubDatetime: 2023-12-23 22:13:00
tags: ["PostgreSQL"]
---

[jooq](https://www.jooq.org/) のように、RDBのスキーマからコードを生成するツールをほしいシーンがあった。
「こういうテーブルがあって、そこにはこういう名前のカラムがあって、そのカラムの型はこうで、制約はこうだ」という情報をつかってコードを生成したい。
DDLをパースするという手もありそうだったが、PostgreSQLに限定すればもっと簡単にできそうだった。

以下のようなクエリを実行するだけ。（ここでは例として `users` テーブルが存在すると仮定している）

```sql
SELECT
    c.table_name,
    c.column_name,
    c.data_type,
    e.data_type AS element_type,
    c.is_nullable,
    c.character_maximum_length
FROM
    information_schema.columns c
LEFT JOIN information_schema.element_types e
        ON ((c.table_catalog, c.table_schema, c.table_name, 'TABLE', c.dtd_identifier)
        = (e.object_catalog, e.object_schema, e.object_name, e.object_type, e.collection_type_identifier))
WHERE
    c.table_name = 'users'
ORDER BY
    c.table_name, c.ordinal_position
```

`data_type` には `character varying` のような文字列が入っている。
PostgreSQL には `array` 型があるが、その場合は `data_type` に `array` が入っていて、`element_type` に `character varying` が入っている。
また、`character varying` のような型には `character_maximum_length` が入っている。（ `text` 型の場合は 0 になる）

ただし、この方法だと型は文字列として取得されるので、文字列比較で処理を書く必要があり、若干不安な気持ちになる。（まぁこの文字列表現に非互換な変更をいれてくることは想像し難いので気持ちの問題だけど）  
PostgreSQL には [oid](https://www.postgresql.jp/document/8.0/html/datatype-oid.html) というのがあって、そっちを使うほうが安心かもしれない。同じようなクエリで oid も取得できる。

実際、Rust 製の DB アクセスライブラリである [sqlx](https://github.com/launchbadge/sqlx) では、この oid を使って型を判定している。  
https://github.com/launchbadge/sqlx/blob/929af41745a9434ae83417dcf2571685cecca6f0/sqlx-postgres/src/type_info.rs#L250-L357

外部キー制約などの constraints も `information_schema.table_constraints` などを使うことで取得できそうだが、今回の自分の用途では不要だったので詳しく調べていない。

また、今回はテーブルとそのカラムに関する情報を取り扱うことに限定されていたが、先述の sqlx や [sqlc](https://github.com/sqlc-dev/sqlc) のように、ユーザが書いたクエリに対して型安全なコードを生成するライブラリも最近は流行ってきている。
与えられたクエリ（プレースホルダーあり）に対して、プレースホルダーに与えるべきパラメータの型や結果の型情報を取得することで、型安全なコードを生成することができる。
こういったことを実現するためには、PostgreSQL の通信プロトコルを用いて、PARSE コマンドなどを発行し、型情報を取得するようだ。 (https://github.com/launchbadge/sqlx/blob/0c8fe729ff1d4e67bbd211209c691f2816ab6f02/sqlx-postgres/src/connection/executor.rs#L23)
こっちのパターンも Rust でちょっと実験してみたので、別の記事に書くかもしれない。

とりあえず、なんにせよ「与えられたPostgreSQLのデータベースに対して、型安全なコードを生成する」というマジックをどのようにやっているのか知れたので、勉強になった。意外とやれば簡単に作れそう。  
実用するとなると PostgreSQL だけというわけにはいかないだろうから、他のデータベースでも同じようなことができるかどうかも調べてみたいと思ったのと、うまく抽象化する設計力が求められそうだなと思った。
