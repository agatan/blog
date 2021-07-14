---
title: "HaskellのConcurrentについて調べてまとめる (IORef編)"
date: 2015-07-21T14:46:58.000Z
tags: ["Haskell"]
---

こんばんは. Haskell(GHC)で並行処理を必要とするアプリケーションを書いてみようと思ったのですが, 並列処理に関するいろいろについてよくわかっていない部分が多かったので, 調べたついでにまとめておこうと思います.

もし間違い等ありましたらコメントいただけるとありがたいです

## Concurrent v.s. Parallel

Concurrent は並行, Parallel は並列と訳されます.

Concurrent は論理的に同時に実行されることで, 実際に複数のタスクが物理的に同時に実行している必要はありません. 実際どうであれ, 同時に実行しているように見えれば OK で, 複数のタスクで CPU を細かく交代で使用しながら実行していくといった実行モデルも Concurrent であるといえます.

Parallel は物理的に同時に実行されることです. 必然的に複数の CPU が必要になります. 物理的に同時に実行されているタスクは, 論理的にも同時に実行しているとみなせるので, Parallel であれば Concurrent です.

この記事では Concurrent について言及しているつもりです.

## Haskell のスレッド

Haskell は処理系実装の軽量スレッドを持ちます. OS が提供するネイティブスレッドと違い, コンテキストスイッチ(スレッドの切り替え)のオーバーヘッドが少なく, より気軽に扱えるスレッドのようです. 軽量スレッドといえば Erlang や Golang が思い浮かびますね.(Erlang は軽量プロセスっていうんでしたっけ)

実際に Haskell で軽量スレッドを立ち上げてみます.

まずはスレッドを立ち上げない場合です. (threadDelay は指定したマイクロ秒分スレッドをスリープします)

```
module Main where

import Control.Concurrent (threadDelay)

sleepN :: Int -> IO ()
sleepN n = do
    putStrLn $ "sleep " ++ show n
    threadDelay $ n * 10 ^ 6
    putStrLn $ "wake up " ++ show n

main :: IO ()
main = do
    sleepN 3
    threadDelay $ 2 * 10 ^ 6
    putStrLn "sleep 2 and wakeup"
    threadDelay $ 2 * 10 ^ 6
    putStrLn "end"
```

実行結果

```
sleep 3
wake up 3
sleep 2 and wakeup
end
```

全体として, 3 秒->2 秒->2 秒とスリープするので 7 秒ほどの実行時間になります.

つぎにスレッドを立ち上げる場合です

Haskell でスレッドを立ち上げるには `forkIO :: IO () -> IO ThreadId` を使用します. `IO ()`を渡すと, それを新しく立ち上げたスレッド上で実行してくれます.

(`forkOS :: IO () -> IO ThreadId` というものもありますが, こちらは Haskell の軽量スレッドではなく, ネイティブスレッドを立ち上げます)

```
module Main where

import Control.Concurrent (forkIO, threadDelay)

sleepN :: Int -> IO ()
sleepN n = do
    putStrLn $ "sleep " ++ show n
    threadDelay $ n * 10 ^ 6
    putStrLn $ "wake up " ++ show n

main :: IO ()
main = do
    forkIO $ sleepN 3
    threadDelay $ 2 * 10 ^ 6
    putStrLn "sleep 2 and wakeup"
    threadDelay $ 2 * 10 ^ 6
    putStrLn "end"
```

実行結果

```
sleep 3
sleep 2 and wakeup
wake up 3
end
```

１つのスレッドが 3 秒スリープしている間に, もう一つのスレッドのスリープが始まるので, 全体で 4 秒ほどの実行時間になります.

## 共有変数

複数スレッドによる並行実行を扱うと, どうしても共有変数的なものが欲しくなる場合があります. Haskell でスレッド間共有をしたい場合はいくつかの方法があるようです.

もっとも直感的(手続きプログラミング出身者にとって)で馴染みやすいのは `Data.IORef` かと思います. `IO` の世界の内側でのみ読み書きができる"変数"です.

まずは単一スレッドで実際に使ってみます.(以後 import などは省略します)

```
add1 :: IORef Int -> IO ()
add1 v = do
    modifyIORef v (+1)

main :: IO ()
main = do
    ref <- newIORef 0
    v <- readIORef ref
    print v
    add1 ref
    v' <- readIORef ref
    print v'
```

実行結果

```
0
1
```

このように変数として中身を書き換えることができます.

これは変数なので, ひとつのスレッドで行った書き換えが他のスレッドにも影響を及ぼします.(State モナドのように変数を模倣しているだけではこれはできない)

```
add1 :: IORef Int -> IO ()
add1 v = modifyIORef v (+1)

spawn :: IORef Int -> IO ()
spawn ref = do
    forkIO $ add1 ref
    return ()

main :: IO ()
main = do
    ref <- newIORef 0
    spawn ref
    spawn ref
    spawn ref
    threadDelay 1000000
    v <- readIORef ref
    print v
```

## データ競合

一方これを複数スレッドで並列に動かすことを考えます. `modifyIORef` はアトミックではないので,

```
v の中身を読む
v の中身 + 1 を計算する
v にその結果を入れる

```

というそれぞれの計算の間に別のスレッドでの計算が割り込まれる可能性がある.

上の例で, `spawn ref >> spawn ref >> spawn ref` という部分は複数スレッドから一つの変数を同時に変更しようとしている. そのため, 変更が競合し意図しない動作になる可能性がある.

IORef で競合を防ぐ方法としては `atomicModifyIORef :: IORef a -> (a -> (a, b)) -> IO b` を使用する方法がある.

`atomicModifyIORef` の第 2 引数は `a -> (a, b)` である. これは `IORef` の中身を引数にとって, `(変更後の値, atomicModifyIORefの返り値にしたい値)` を返す関数である.

```
inc :: IORef Int -> IO Int
inc ref = atomicModifyIORef ref (\n -> (n + 1, n))

main :: IO ()
main = do
    ref <- newIORef 0
    res <- inc ref
    v <- readIORef ref
    print res
    print v
```

`inc` は C 言語の`i++;`のような動きをする. 加算する前の値を返し, 変数をインクリメントする.

`atomicModifyIORef` は名前の通り atomic な操作であり, 分割不可能になるため他のスレッドと処理が競合することがなくなる.

## 一旦まとめ

長くなってきた & 疲れてきたので一旦きります.

今回はスレッド間共有変数のために `IORef` を使用し, その変更に `atomicModifyIORef` を使用することでデータ競合を防ぐ方法を紹介した.

`MVar` や `STM` を使用する方法もあり, そっちのほうが良い場合もあるっぽいのでそっちについてもまとめたいと思います.

---

---
