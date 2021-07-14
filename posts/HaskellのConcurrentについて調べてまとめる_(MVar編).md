---
title: "HaskellのConcurrentについて調べてまとめる (MVar編)"
date: 2015-07-22T01:21:48.000Z
tags: ["Haskell"]
---

どうもこんにちは.

前回([Haskell の Concurrent について調べてまとめる (IORef 編) - プログラミングのメモ帳 ➚](http://agtn.hatenablog.com/entry/2015/07/21/234658))の続きです.

今回はスレッド間協調のために`MVar`を使う方法について調べたので, まとめたいと思います.

## MVar

Haskell にかかわらず, 最近の並行処理はメッセージパッシングでやれみたいなのが流行ってますね (Scala の Akka や golang の chan など).

`MVar`は Haskell における, 容量 1 のメッセージボックスのようなものです. `MVar`を使うことで, スレッド間でメッセージのやり取りを協調的に行うことができます.

複数のスレッドが１つの`MVar`に対して, メッセージを入れたり取り出したりすることでスレッド間協調を行います.

基本となる API はこのような感じ

```
newEmptyMVar :: IO (MVar a)
newMVar :: a -> IO (MVar a)
takeMVar :: MVar a -> IO a
putMVar :: MVar a -> a -> IO ()
readMVar :: MVar a -> IO a
```

型を見ればなんとなく使い方もわかる気がしますね.

`MVar`を作るには`newEmptyMVar`か`newMVar`を使用します. `newEmptyMVar`は空のメッセージボックスを作り, `newMVar`は第一引数を初期値としてもつメッセージボックスを作ります.

`MVar`にメッセージを格納するには, `putMVar`を使います. `putMVar mvar msg` で, `msg`を`mvar`に格納します.

この際, もし`MVar`にすでにメッセージが格納されている場合, `MVar`は容量 1 のボックスなので, `putMVar`がブロックされます. 他のスレッドが`MVar`からメッセージを取り出して空にするまで待ってから, メッセージを格納します.

一方, `MVar`からメッセージを読み取るには, `takeMVar`か`readMVar`を使用します.

`takeMVar`はメッセージを読み取り, その`MVar`を空にします. `readMVar`はメッセージを読み取りますが, `MVar`の中のメッセージはそのまま残します.

ここで, `put`の時と同様に, `takeMVar`も`readMVar`も`MVar`にメッセージが格納されていなかった場合, 他のスレッドが`MVar`にメッセージを格納するまでブロックします.

というわけで簡単なサンプルコード

```
module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar

main :: IO ()
main = do
    mvar <- newEmptyMVar
    forkIO $ do
        msg <- takeMVar mvar
        putStrLn $ "recv: " ++ msg
        threadDelay $ 1 * 10 ^ 6
        putMVar mvar "B"
    putStrLn "sleep 1"
    threadDelay $ 1 * 10 ^ 6
    putStrLn "wake up"
    putMVar mvar "A"
    takeMVar mvar >>= print
```

実行結果

```
sleep 1
wake up
recv: A
"B"
```

確かにメッセージが格納されるまで `takeMVar`がブロックしていることがわかります

## 共有変数としての MVar

さて, `MVar`にはもうひとつの使い方があります. 共有変数としての`MVar`です.

`MVar`の特徴として, 誰かが`take`してから`put`するまでの間は, 他のスレッドはだれも`MVar`の中身に触れないという点が挙げられます.

```
main = do
    mvar <- newMVar 0
    forkIO $ do
        val <- takeMVar mvar
        -- 他のスレッドはMVarの中身に触れない
        putMVar mvar $ val + 1
    ...
```

この特徴はまさにロックの特徴といえます. ロックを取得し解放するまでは, 他のスレッドは同じロックで保護された区間にははいれません.

というわけで`MVar`は型レベルでロックがついた共有変数とみなすことができますね！(このへんは Rust の Mutex に似た空気を感じます. どちらも型レベルでロックとそれが保護する中身がつながっています)

型レベルでロックがくっついているので, 中身にアクセスするには必ずロックをとる(`takeMVar`)必要があり, ロックの取得忘れがありません.

さらに, Haskell は基本的に破壊的操作があまり登場しない言語であることもこの`MVar`ロックにプラスに働きます.

例えば, 連想配列をスレッド間で共有することを考えます. また, ここでは連想配列の実装として, hashtable ではなく`Data.Map`を使用するとします(`Data.Map`は immutable な構造になっていて, lookup は O(log n)ですが, immutable なので Haskell 上で扱いやすいというメリットがあります).

`Data.Map`は immutable なので, 一度`MVar`から取得してしまえばそれ以降変更される可能性もないため, ロックを保持し続ける必要がありません. そこで, 単なる読み込みの場合は, `takeMVar`してすぐに`putMVar`で戻すだとか, `readMVar`で読み込むだけにすることで, ロックの粒度を小さくできます.

`MVar`の中身を書き換えたい場合は, 単純にロックを取得し, 書き換え後の値を`putMVar`します.

```
module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import qualified Data.Map.Strict as M

main :: IO ()
main = do
    mvar <- newMVar M.empty
    forkIO $ do
        table <- takeMVar mvar
        putMVar mvar table
        -- tableを使用する操作
    forkIO $ do
         table <- readMVar mvar
         -- tableを使用する操作
    forkIO $ do
        table <- takeMVar mvar
        -- tableを変更する操作
        let newTable = ...
        putMVar mvar newTable
```

このように`MVar`と immutable なデータ構造を組み合わせることで, 粒度の小さいロックを実現することができます.

一方, `MVar`と mutable なデータ構造(`IORef`など)を組み合わせる場合は, たとえ読み込みしかしない場合であっても操作が終わるまではロックを保持しておく必要があることに注意しなければなりません (`IORef`には前回紹介したように`atomicModifyIORef`があるのでなかなかこういう状況は起こりませんね)

また, Rust の Mutex と違い, `MVar`によるロックの模倣(?)はロックの解放を自動的には行いません. したがって例外が送出された場合にロックを開放し忘れるケースがあるので, 注意が必要です.

## 一旦まとめ

というわけで今回は`MVar`について紹介しました. `MVar`でロックを実現する方に関しては, 散々言われているロックの問題点をそのまま持ってきてしまうのであまり使えないかもしれませんね...

`MVar`は容量 1 のメッセージボックスでしたが, Haskell には`Chan`というものもあります. こちらは golang の chan にかなり近いもので, 容量の制限がないキューのように働かせることができます. `Chan`のよみとり専用のスレッドを１つ立てておき, 他の複数のスレッドがタスクを`Chan`に書き込んでいくといったユースケースが考えられますね. こっちのほうが便利そうな気がしてきました.

ロックはいろいろ厄介で, デッドロックとか解放忘れとかの問題がついて回ります. それを解決する１つの方法として`STM`があるようなので, 次はそれについて調べてみようと思います.

---

---
