---
title: "Haskellでechoサーバ"
date: 2015-07-23T12:43:44.000Z
tags: ["Haskell"]
---

はいどうもー

引き続き Haskell の話題です. ちょっと Haskell で TCP ソケットを使ってみたくなったので, まず簡単なものから実装してみます.

TCP ソケットのチュートリアルといえば echo サーバですね！クライアントからの入力をそのまま返すサーバです.

せっかくなのできちんと複数クライアントとの同時通信を可能にしましょう.

# Network.Socket

Haskell で TCP ソケットを使うには, `Network.Socket`を使うようです. [Network.Socket](http://hackage.haskell.org/package/network-2.6.2.1/docs/Network-Socket.html)

ドキュメントには, 低レベル API が`Network.Socket`で, 高レベル API が`Network`と書いてあるのですが, `Network`モジュールのドキュメントには, 互換性のために残してあるけどこれから使う人は`Network.Socket`を使ってくれみたいなことが書いてあります.

適当にググると`Network`モジュールを使ったサンプルが散見されますが, ここはドキュメントにしたがって, `Network.Socket`を使用することにします.

# ソケットの用意

TCP のサーバ側は, ソケットの作成 -> ソケットをポート番号指定で bind -> 接続受付を開始(listen) -> 接続を受け付ける(accept) というステップを踏む必要があります.

というわけでまずは指定したポート番号に bind されたソケットを用意するアクションを定義します.

```
import Network.Socket

serveSocket :: PortNumber -> Socket
serveSocket port = do
    soc <- socket AF\_INET Stream defaultProtocol
    addr <- inet\_addr "0.0.0.0"
    bind soc (SockAddrInet port addr)
    return soc

```

これで引数に渡したポート番号に bind されたソケットが作成されます.

## accept

複数クライアントとの同時通信を実現するためには, `Control.Concurrent`のちからを借ります.

今回は `forkIO` を使って, 各コネクションごとにスレッドを起動していくことにします. (非同期版も作れるのかな？つくれたらつくります)

というわけで次は `accept`して`forkIO`するという処理を繰り返し行うアクションを定義します.

`forkIO` した後に実行するアクション(`echoLoop`)についてはとりあえず`undefined`とします.

Haskell の`undefined`, とても便利ですね. 型で考えるっていうスタイルが実行しやすくなっているのは, ソースコード上にトップレベル関数の型指定を書きやすい Haskell の文法と`undefined`のおかげって感じがします.

```
echoLoop :: Socket -> IO ()
echoLoop = undefined

-- import Control.Concurrent
-- import Control.Monad
-- が必要
acceptLoop :: Socket -> IO ()
acceptLoop soc = forever $ do
    (conn, \_addr) <- accept soc
    forkIO $ echoLoop conn

```

`forever :: Monad m => m a -> m b`は引数に IO アクションを受け取り, それを無限に繰り返し実行し続けます. (無限にくりかえすので返り値の型変数`b`は不定)

`forever`の引数には, `accept`して`forkIO`するアクションを渡しています.

# echo

最後にソケットから読み込み, そのまま書き出す`echoLoop`部分を作ります.

```
-- import Control.Exception が必要
echoLoop :: Socket -> IO ()
echoLoop conn = do
    sequence\_ $ repeat $ do
        (str, \_, \_) <- recvFrom soc 64
        send soc str

```

`recvFrom :: Socket -> Int -> IO (String, Int, SockAddr)`は, `recvFrom conn n` で, `conn`から最大で`n`文字まで読み込みます.
返り値は, `(読み込んだ文字列, 読み込んだ文字数, 読み込み元のアドレス??)` を返します.

そして, `send :: Socket -> String -> IO ()` でソケットに読み込んだ文字列をそのまま書き込みます.

このように, 読み込んでそのままｍ書き込むというアクションを `repeat`でつなげています. `repeat :: a -> [a]`は無限リストを作る関数です. `repeat 0`で`[0, 0, 0, 0, ...`というリストが作成されます.

このままでは `[IO ()]`型なので, これを`sequence_ :: Monad m => [m a] -> m ()`を使って１つの IO アクションにまとめ上げます.

Haskell が遅延評価だから出来る芸当ですね. 無限に繰り返す感あふれるコードになっている気がします. (`forever`使ったほうがいいと思います)

# 例外処理

`recvFrom`は相手側がコネクションを切断すると`End of file`の例外を投げます. `forkIO`しているので, １つのスレッドが例外で落ちてもサーバ全体は動き続けますが, ソケットのクローズも出来ませんし, 標準エラーになんかでてきてよろしくないので修正します.

```
-- import Control.Exceptionが必要
echoLoop :: Socket -> IO ()
echoLoop conn = do
   sequence\_ $ repeat $ do
       (str, \_, \_) <- recvFrom conn 64
       send conn str
   `catch` (\(SomeException e) -> return ())
   `finally` close conn

```

`catch`と`finally`を追加しています.

どちらも Java とかのそれと同じように動きます.

`SomeException`はすべての例外を補足することが出来ますが, ほんとはあんまり良くないですね. ここでは EOF に達した(コネクションが切断された)という場合だけを補足したいので. (どの関数がどういう場合にどんな例外を投げるのかっていうドキュメントがわからなかったのでこのままにしておきました)

そして, 例外が発生してもしなくても, 最後にかならずソケットのクローズをするよう`finally`を使います.

`SomeException`ですべての例外が捕捉出来るのって不思議じゃないですか？Haskell にはオブジェクト指向っぽい型の階層関係なんてないのに.
[Haskell の多相性 - あどけない話](http://d.hatena.ne.jp/kazu-yamamoto/20081024/1224819961)このへんが関係しているっぽいなという感じがしますが詳しいことはよくわかりませんでした...

# 全体

```
module Main where

import Network.Socket
import Control.Monad
import Control.Concurrent
import Control.Exception

main :: IO ()
main = do
    soc <- serveSocket 8080
    listen soc 5
    acceptLoop soc `finally` close soc

serveSocket :: PortNumber -> IO Socket
serveSocket port = do
    soc <- socket AF\_INET Stream defaultProtocol
    addr <- inet\_addr "0.0.0.0"
    bind soc (SockAddrInet port addr)
    return soc

acceptLoop :: Socket -> IO ()
acceptLoop soc = forever $ do
    (conn, addr) <- accept soc
    forkIO $ echoLoop conn

echoLoop :: Socket -> IO ()
echoLoop conn = do
    sequence\_ $ repeat $ do
      (str, \_, \_) <- recvFrom conn 64
      send conn str
    `catch` (\(SomeException e) -> return ())
    `finally` close conn

```

`main`内で `listen`するのを忘れずに！また, `acceptLoop`中に例外が発生してもソケットをクローズするように`finally`を使っています. (まぁプログラム終了するのでいらない気もします)

# 動作確認

`telnet`コマンドでテストします.

```
% telnet localhost 8080
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
test
test
aaa
aaa
hooooogle
hooooogle
```

ちょっとわかりづらいですが, 入力した文字列が即座にそのまま帰ってきていることがわかります. バッファリングの関係で, 一行ずつになっていますが.

# まとめ

Haskell で echo サーバ, 意外とすんなりかけましたね. 例外関係があまりよく理解できていない感じがしますが...

非同期版が気になります. 調べてみます.

---

---
