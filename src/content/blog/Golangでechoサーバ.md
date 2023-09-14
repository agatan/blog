---
title: "Golangでechoサーバ"
pubDatetime: 2015-09-08T07:14:10.000Z
tags: ["Go"]
---

最近 golang が気になります

golang の特徴はもはやわざわざここに書くまでも無いことだと思うので書きませんが, 気になっている理由を書いてみます.

- バイナリ(しかもポータビリティが非常に高いバイナリ)にコンパイルされること
- C/C++ には及ばずとも実行が非常に速いこと
- C/C++ ほど低レイヤーいじり放題なわけではないが, ある程度低レイヤーまで降りていけること
- コマンドラインツールから Web アプリケーションのような高レイヤーまで十分得意であること
- `interface` による抽象化が, 過度でなく調度良く感じられること

こんな感じでしょうか.

Compiled Python っていう感じが非常に良さそうだなーと思っています.

というわけで Haskell に引き続き golang でも echo サーバを書いてみました

```
package main

import (
    "fmt"
    "io"
    "net"
)

func main() {
    listener, err := net.Listen("tcp", ":8080")
    if err != nil {
        panic(err)
    }
    for {
        conn, err := listener.Accept()
        if err != nil {
            panic(err)
        }
        go func(conn net.Conn) {
            defer conn.Close()
            echo(conn)
        }(conn)
    }
}

func echo(conn net.Conn) {
    buf := make([]byte, 256)
    for {
        n, err := conn.Read(buf)
        if err != nil {
            if err == io.EOF {
                break
            }
            panic(err)
        }
        if n == 0 {
            break
        }
        wn, err := conn.Write(buf[0:n])
        if err != nil {
            panic(err)
        }
        if wn != n {
            panic(fmt.Errorf("could not send all data"))
        }
    }
}

```

さすがは golang というかなんというか. ものすごく普通な空気を感じますね.

golang はこういう普通さが売りの１つだと思っています.

---

---
