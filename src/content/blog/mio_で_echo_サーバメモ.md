---
title: "mio で echo サーバメモ"
postSlug: mio_で_echo_サーバメモ
pubDatetime: 2017-01-07T06:14:55.000Z
tags: ["Rust"]
---

Rust の非同期 IO ライブラリのなかでももっとも低レベルなレイヤーを担っている [mio](https://github.com/carllerche/mio) を使って echo サーバを書いた。
echo サーバばっかり書いているような気がするけど，echo サーバやっておくと簡単な割にライブラリの使い方とかがちゃんと分かる気がするので好きです。

### コード

```
extern crate mio;

use std::io::prelude::*;
use std::collections::HashMap;

use mio::*;
use mio::tcp::{TcpListener, TcpStream};

#[derive(Debug)]
struct ClientsHolder {
    table: HashMap<Token, TcpStream>,
    free\_token: Vec<Token>,
    next\_max\_token: Token,
}

impl ClientsHolder {
    fn new\_from(start\_token: Token) -> Self {
        ClientsHolder {
            table: HashMap::new(),
            free\_token: Vec::new(),
            next\_max\_token: start\_token,
        }
    }

    fn next\_token(&mut self) -> Token {
        if let Some(tok) = self.free\_token.pop() {
            return tok;
        }
        let tok = self.next\_max\_token;
        self.next\_max\_token = Token(tok.0 + 1);
        tok
    }

    fn register(&mut self, tok: Token, client: TcpStream) {
        self.table.insert(tok, client);
    }

    fn get\_mut(&mut self, tok: Token) -> Option<&mut TcpStream> {
        self.table.get\_mut(&tok)
    }

    fn remove(&mut self, tok: Token) -> Option<TcpStream> {
        let result = self.table.remove(&tok);
        if result.is\_some() {
            self.free\_token.push(tok);
        }
        result
    }
}

// Setup some tokens to allow us to identify which event is
// for which socket.
const SERVER: Token = Token(0);

fn main() {

    let addr = "127.0.0.1:13265".parse().unwrap();

    // Setup the server socket
    let server = TcpListener::bind(&addr).unwrap();

    // Create an poll instance
    let poll = Poll::new().unwrap();

    // Start listening for incoming connections
    poll.register(&server, SERVER, Ready::readable(), PollOpt::edge())
        .unwrap();

    // Create storage for events
    let mut events = Events::with\_capacity(1024);
    let mut clients = ClientsHolder::new\_from(Token(1));

    loop {
        poll.poll(&mut events, None).unwrap();

        for event in events.iter() {
            match event.token() {
                SERVER => {
                    // Accept and drop the socket immediately, this will close
                    // the socket and notify the client of the EOF.
                    let (stream, \_) = server.accept().unwrap();
                    let tok = clients.next\_token();
                    poll.register(&stream, tok, Ready::readable(), PollOpt::edge()).unwrap();
                    clients.register(tok, stream);
                }
                tok => {
                    let mut close = false;
                    if let Some(mut stream) = clients.get\_mut(tok) {
                        let mut buf = [0; 1024];
                        let n = stream.read(&mut buf).unwrap();
                        if n == 0 {
                            poll.deregister(stream).unwrap();
                            close = true;
                        } else {
                            stream.write(&buf[0..n]).unwrap();
                        }
                    }
                    if close {
                        clients.remove(tok);
                    }
                }
            }
        }
    }
}

```

面倒だったので `unwrap` まみれですが。

## やったこと

`mio` の全体の流れとしては，`Poll` 型の値がイベントを監視する役割を担います．
`Poll` に監視対象を登録していき，`Poll::poll` でイベントの発火を待ちます．
発火したイベント一覧が `Events` 型の `Events::iter` で取れるので，対応していけばよいです．

`mio` では `Token` という型の値を使って監視対象を識別しています．
監視対象には `TcpListener` ，`TcpStream`，`Sender`，などなどいろんなものがあるので，統一的に扱うために `Poll` は `Token` だけを保持します．
`Token` と監視対象の紐付けはユーザ側の責任でやってくれという感じみたいです．

echo サーバではクライアントの数は不特定なので，「空いている `Token` を探す」と「`Token` に対応するクライアント (`TcpStream`) を探す」がうまくできる必要があります．
そこで，`ClientsHolder` を定義しました．
こいつが，空いている `Token` を返すのと `Token` をキーに `TcpStream` を返す仕事をします．
`remove` されたらその `Token` は再利用します．

## 気になるところ

`mio` はファイルに対する抽象化は提供しない方針のようです．
`STDIN` / `STDOUT` も同様です．

ファイル IO もノンブロッキングにしたい場合はどうしたらいいんでしょう？よくわかっていない．
`mio::unix` 以下に UNIX 限定の拡張がおいてあって，`EventedFd` という file descriptor を扱う実装はあるので，UNIX 限定なら力技でなんとかなるのかもしれない．

あと `mio` は関係ないんですが，実装の部分で，

```
let mut close = false;
if let Some(mut stream) = clients.get\_mut(tok) {
    let mut buf = [0; 1024];
    let n = stream.read(&mut buf).unwrap();
    if n == 0 {
        poll.deregister(stream).unwrap();
        close = true;
    } else {
        stream.write(&buf[0..n]).unwrap();
    }
}
if close {
    clients.remove(tok);
}

```

というのがあるんですが，これどうやったらスマートなんでしょう．
`close = true` としている部分で `clients.remove(tok);` をやるのが普通だと思うんですが，`if let Some(mut stream) = clients.get_mut(tok) {` のところで `clients` は borrow されているから mutable borrow はこれ以上作れないのです．

---

---
