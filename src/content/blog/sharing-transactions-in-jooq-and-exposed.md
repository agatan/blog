---
title: "jOOQとExposedでトランザクションを無理やり共有する"
postSlug: sharing-transactions-in-jooq-and-exposed
pubDatetime: 2024-07-07
tags: ["Kotlin"]
---

[jOOQ](https://www.jooq.org/) と [Exposed](https://github.com/JetBrains/Exposed) はどちらも Kolin から使える ORM ライブラリです。（jOOQはJavaメインで、Exposedは最初からKotlin向けに書かれているという違いはある）

僕は株式会社ヘンリーで、レセコン一体型電子カルテ Henry の開発で、Kotlin をつかった API サーバーを書いています。
Henry では Exposed を使っているのですが、いろいろと事情があり、jOOQ などの別のライブラリへの移行を勝手に検討しています。

まだ業務でどうこうというわけではないのですが、そもそも jOOQ に移行するって現実的なんだろうか、というのを自分の中で答えを出しておきたいと思って調べてみました。
（ちなみに Spring の `TransactionManager` を使えばどちらも Spring Integration を提供しているので、この問題は簡単に倒せる問題になります。ここでは Spring をつかわずに倒す方法を検討しています。）

なんで移行しようとしているのかとか、移行するときの意思決定の過程とかは、実際やることになったら [会社の技術ブログ](https://dev.henry.jp/) に書きたいと思っています。

## 移行の際の課題

移行は段階的に行う想定です。すでに非常に巨大になっているサービスなので、一挙に置換することはほぼ不可能と考えています。
そうなると、トランザクション共有の問題が出てきます。

Exposed と jOOQ が同じトランザクションを共有できないと、Repository の実装単位で移行するといったことが難しくなります。「あっちでは jOOQ、こっちでは Exposed をつかっているので、同一トランザクションでその両方を使うことができない」みたいになってしまうと、移行が難しくなります。

トランザクションを共有ためには、具体的には **「`BEGIN;`したコネクションを、両方のライブラリから使うことができる」** という状態を達成できれば OK です。

現代的なアプリケーションでは、`HikariCP`のようなコネクションプールを使うことがほとんどです。
何も考えずにDBアクセスを実行すると、コネクションプールから空いているコネクションを acquire します。
この場合、同じコネクションプールを使っていたとしても、2つのライブラリが実際に実行時に使うコネクションは別物になります。一方が使っているコネクションは、release されない限りもう一方のライブラリから使われることは絶対にないためです。

トランザクションは、`BEGIN;` して何らかの書き込みオペレーションをして `COMMIT;` / `ROLLBACK;` をする一連の処理を、一つのコネクション上で実行しなければなりません。
したがって、今回は **Exposed (jOOQ) がトランザクション用に獲得したコネクションを、jOOQ (Exposed) からも使えるようにする**、ということを達成できればいいわけです。

## jOOQ と Exposed でトランザクションを共有する方法

3パターンの実現方法が考えられます。

- a. Exposed にトランザクション管理を任せる & そのコネクションを jooq から相乗りできるようにする
- b. jooq にトランザクション管理を任せる & そのコネクションを Exposed から相乗りできるようにする
- c. 自前でトランザクション管理をして、そのコネクションを jooq / Exposed から相乗りできるようにする

結論からいうと、**「a しかない & 十分簡単で現実的」**、です。

トランザクション管理は意外と考えることが多くて面倒なので、できれば c は避けたいところです。
あまりにも根幹かつクリティカルな部分なので、自前実装するよりは、ある程度枯れたライブラリに頼るのが得策でしょう。

というわけで、a, b の実現性を見ていきます。

## Exposed のトランザクション管理

Exposed では、以下のようにしてトランザクションをはります。

```kotlin
val hikariConfig: com.zaxxer.hikari.HikariConfig = com.zaxxer.hikari.HikariConfig()
val dataSource = com.zaxxer.hikari.HikariDataSource(hikariConfig)

val db = org.jetbrains.exposed.sql.Database.connect(dataSource)

org.jetbrains.exposed.sql.transactions.transaction(db) {
    // このブロック内で実行されるクエリは、暗黙に一つのコネクションで実行される
    Users.selectAll()
}
```

（そもそも Exposed は、クエリ発行は必ず `transaction` ブロック内で実行する必要があったりします。個人的にはあまり好みではないデザイン。）

実装としては [このあたり](https://github.com/JetBrains/Exposed/blob/cd275341ad989709b807cda0585ae25e048581b7/exposed-core/src/main/kotlin/org/jetbrains/exposed/sql/transactions/ThreadLocalTransactionManager.kt) です。
`TransactionManager.currentOrNull()` を使うと、現在のトランザクションが取得できますが、デフォルトで使われる [`ThreadLocalTransactionManager` の実装](https://github.com/JetBrains/Exposed/blob/cd275341ad989709b807cda0585ae25e048581b7/exposed-core/src/main/kotlin/org/jetbrains/exposed/sql/transactions/ThreadLocalTransactionManager.kt#L117)を見ると、その名の通り単なる `ThreadLocal` であることがわかります。

```kotlin
/** A thread local variable storing the current transaction. */
val threadLocal = ThreadLocal<Transaction>()

// ...中略...

override fun currentOrNull(): Transaction? = threadLocal.get()
```

Exposed は、`TransactionManager.current()` からコネクションを取得し、それを用いてクエリを実行する設計になっているので、Exposedの外で取得したコネクションを Exposed に使わせるには、 `TransactionManager.current()` をどうにかして上書きする必要があります。

が、`TransactionManager` の公開APIを見た限り、それは実現できなさそうです。

そもそもクエリ発行を必ず `transaction` ブロックで実行する必要があるデザインなので、 **コネクション管理とトランザクション管理が結合**しており、剥がすことが出来なさそうでした。
なので、Exposed の外でトランザクションを張って、そのコネクションを Exposed にどうにか渡せたとしても、Exposed のトランザクション管理をバイパスできず、Exposed も自分で `BEGIN; COMMIT;` してしまうわけです。これでは正しいトランザクション管理ができません。

というわけで、この時点で以下の選択肢は潰れてしまいました。

> - b. jooq にトランザクション管理を任せる & そのコネクションを Exposed から相乗りできるようにする
> - c. 自前でトランザクション管理をして、そのコネクションを jooq / Exposed から相乗りできるようにする

## jOOQ のトランザクション管理

jOOQ では、以下のようにしてトランザクションをはります。

```kotlin
val configuration: org.jooq.Configuration = org.jooq.impl.DefaultConfiguration()
val context: org.jooq.DSLContext = configuration.dsl()
context.transaction { tx: org.jooq.Configuration ->
    // tx には、トランザクションを実行するコネクションを使うことを強制する設定が入っている。
    // tx.dsl(): DSLContext を呼び出すことで、そのコネクションを使ってクエリすることができる
    tx.dsl().selectFrom(USERS)
}
```

Exposed と異なり、明示的にトランザクションに関連するオブジェクトを持ち回る必要があるデザインです。（個人的に好きなデザイン。）

`context.transaction` の呼び出しでは、（設定にもよりますが、デフォルトでは） [`DefaultTransactionProvider`](https://github.com/jOOQ/jOOQ/blob/main/jOOQ/src/main/java/org/jooq/impl/DefaultTransactionProvider.java) に処理が移譲されており、内部でコネクションを取得していることがわかります。

取得したコネクションは、 トランザクションコンテキストの `Configuration` のインスタンスの中にある `Map<Any, Any>` のなんでもはいる入れ物に、 `DATA_DEFAULT_TRANSACTION_PROVIDER_CONNECTION` というキーで格納されます。
このコンテキストはトランザクションの始まりから終わりまでで共有されるコンテキストです。Go の `context.Context` みたいなもんです。

```kotlin
private final DefaultConnectionProvider connection(Configuration configuration) {
    DefaultConnectionProvider connectionWrapper = (DefaultConnectionProvider) configuration.data(DATA_DEFAULT_TRANSACTION_PROVIDER_CONNECTION);

    if (connectionWrapper == null) {
        connectionWrapper = new DefaultConnectionProvider(connectionProvider.acquire());
        configuration.data(DATA_DEFAULT_TRANSACTION_PROVIDER_CONNECTION, connectionWrapper);
    }

    return connectionWrapper;
}
```

そして、その `Configuration` インスタンスからコネクションを取得するには、 `Configuration.connectionProvider()` を呼ぶのですが、[実装](https://github.com/jOOQ/jOOQ/blob/a9163f3c73213c59b61acaf4f6899a6fe4d8dd39/jOOQ/src/main/java/org/jooq/impl/DefaultConfiguration.java#L2048-L2062) をみると、さっき格納した`DATA_DEFAULT_TRANSACTION_PROVIDER_CONNECTION`を取得していることがわかります。

```kotlin
@Override
public final ConnectionProvider connectionProvider() {

    // [#3229] [#5377] If we're currently in a transaction, return that transaction's
    // local DefaultConnectionProvider, not the one from this configuration
    TransactionProvider tp = transactionProvider();
    ConnectionProvider transactional = tp instanceof ThreadLocalTransactionProvider t
        ? t.localConnectionProvider
        : (ConnectionProvider) data(DATA_DEFAULT_TRANSACTION_PROVIDER_CONNECTION);

    return transactional != null
         ? transactional
         : connectionProvider != null
         ? connectionProvider
         : new NoConnectionProvider();
}
```

さて、今回は残る選択肢の一つである、

> - a. Exposed にトランザクション管理を任せる & そのコネクションを jooq から相乗りできるようにする

を実現する方法を考えたいのでした。

グダグタと jOOQ のトランザクション管理について書いてきましたが、実はこれをやるのはとても簡単で、jOOQのトランザクション管理について知る必要もありません。

要するに、jOOQの基本要素である `Configuration` のインスタンスに「このコネクションをつかえ」と差し込むことができればよいわけです。
そしてそれは `configuration.derive(connection)` とやるだけで簡単に実現できます。
`derive` は、現在の Configuration のコピーを作って、一部だけ書き換える、という役割をもっています。

先程見たように、通常のjOOQでのトランザクション実行も、そのトランザクション専用のスコープを持つ `Configuration` インスタンスを作っていたわけなので、API としても全く同じ形にすることができます。

## 成果物

というわけでごちゃごちゃ書きましたが、成果物はすごくシンプルです。

```kotlin
fun <T> transactionInExposedAndJooq(
    configuration: org.jooq.Configuration,
    block: (org.jooq.Configuration) -> T,
): T {
    return org.jetbrains.exposed.sql.transactions.transaction {
        // this@transaction には、Exposed の `Transaction` が入っているので、
        // そこからコネクションをぶっこぬいて jOOQ にわたす
        val derived = configuration.derive(
            this@transaction.connection.connection as java.sql.Connection
        )
        block(derived)
    }
}
```

使う側は Exposed も jOOQ もどちらも元の API とほぼ遜色ない形で使えます。

```kotlin
// Exposed
transactionInExposedAndJooq(config) {
    // Exposed は ThreadLocal を使って暗黙にコネクションを引き出すので、明示的な指定は不要。
    Users.selectAll().forEach {
        println(it)
    }
}

// jOOQ
transactionInExposedAndJooq(config) { tx ->
    // tx には、Exposed が取得したトランザクション用のコネクションが差し込まれている
    println(tx.dsl().selectFrom(USERS))
}
```

比較のために、改めて、それぞれ単独で使う場合のコードも載せておくと

```kotlin
// Exposed
transaction {
    Users.selectAll().forEach {
        println(it)
    }
}

// jOOQ
config.dsl().transaction { tx ->
    println(tx.dsl().selectFrom(USERS))
}
```

こんな感じです。起点となる関数が変わっただけで、中身は全く同じですね。

## おわりに

これで Exposed から jOOQ に段階的かつ安全に移行できることがわかりました。

技術的に今回紹介した方法一択感がありますが、そうでなくても、いままで Exposed にゆだねていたトランザクション管理を、一気に別の方式に置き換えるのは、全く同じ挙動になっていることを保証するための確認コストが大きすぎるので、Exposed に委ねたまま移行を開始できるのは悪くないかもしれないですね。
（まぁどうせ移行を完全に終わらせたときにはそのコストを払わないといけなくなるのですが...）

個人的な好みとして、ThreadLocal をつかった暗黙の状態管理はちょっと... という思いが元々あったのですが、今回の件でよりその思いを強くしました。
jOOQ がそういう設計思想じゃなかったおかげで、無事移行が可能そうで助かりました。 （そのへんのフィーリングが合うので jOOQ を検討している、という順番ではあるのですが）

ORMライブラリの移行や並行稼働については、いろいろと考えなければならないこと（過渡期どうするのとか、スキーマの二重管理にならないのかとか）も多いので、実際あまりやられていないような気もしています。
が、実際継続的に開発をするならDBアクセスライブラリの技術選定は非常に重要で、開発者体験や生産性に直結するものなので、移行も視野に改善していく価値のあるものだと思います。

今回は実装にフォーカスして書いたので個人ブログに書きましたが、うまく進められそうだったら、その他の観点についても[会社のブログ](https://dev.henry.jp/) に書いてみようかなと思っています。

（地味に個人ブログで Kotlin の話をしたの初めてかもしれない。）
