---
title: "Rustで自作シェルもどきを作る(単純なコマンド実行編)"
date: 2014-12-21T14:55:51+09:00
tags: ["Rust"]
url: https://qiita.com/agatan/items/ed2780628d20a0e343b8
---

コード全文はこちら
[agatan/rsh](https://github.com/agatan/rsh)

[Rust で自作シェルもどきを作る(字句解析編) - Qiita](http://qiita.com/agatan/items/8a097ead46df1c1659ff)

前回はユーザからの入力を受け付けてパースするところまで実装したので、今回は実際に入力されたコマンドを実行させてみます。  
ただし、いきなりいろいろやるのはきついので、まずは単純なコマンド(パイプやリダイレクトは一旦無視します)の実行をやってみました。

## Command

Rust には`std::io::process::Command`という構造体があります。これを使えば簡単にコマンド実行は出来そうです。

> http://doc.rust-lang.org/std/io/process/struct.Command.htmlより引用
>
> ```rust
> use std::io::Command;
> ```

> let mut process = match Command::new("sh").arg("-c").arg("echo hello").spawn() {
> Ok(p) => p,
> Err(e) => panic!("failed to execute process: {}", e),
> };

> let output = process.stdout.as_mut().unwrap().read_to_end();

````

いろいろと便利そうな関数が定義されているのですが、今回は最も単純にコマンドを実行させるために、`output`という関数を使用してみました。

```rust:exec.rs
use std;
use std::io::process::Command;

use parse::Token;

pub fn exec(tokens: Vec<Token>) {
    if tokens.iter()
             .find(|&t| match t {
                 &Token::Str(_) => false, _ => true } ).is_some() {
        panic!("Not implemented yet.");
    }
    let first_cmd = tokens.iter()
                          .take_while(|&t| match t {
                              &Token::Str(_) => true,
                              _ => false
                          })
                          .map(|ref t| match *t {
                              &Token::Str(ref x) => x.clone(),
                              _ => panic!("Shouldn't be reached."),
                          })
                          .collect::<Vec<String>>();

    let mut output = match Command::new(first_cmd[0].as_slice())
                              .args(first_cmd.as_slice().tail())
                              .output() {
        Ok(p) => p,
        Err(e) => panic!("Failed to execute: {}", e),
    };

    print!("{}", String::from_utf8_lossy(output.output.as_slice()));

}
````

`exec`は`Vec<Token>`を受け取る関数です。
今回はパイプなどは未実装ですから、`Str(_)`以外の`Token`が含まれていたら`panic!`(例外送出)をしています。

さて、`Command`は`new`で実行したいコマンド名を指定し、それに続けて`arg`や`args`で引数を追加していきます。
ユーザからの入力を利用しているので、ひとつずつしか追加できない`arg`よりは、スライスを渡して複数追加できる`args`を使いたいです。

これらにあわせて`Vec<Token>`を`Vec<String>`もしくは`Vec<&str>`に変換してあげれば良さそうです。
ということで`let first_cmd = ...`という長い文は、その変換を行っています。

正直まだ`&`と`ref`の関係とか所有権の問題はきちんと理解できていないというか書き方がよくわかっていないので、コンパイラに怒ってもらいながらなんとか直してみました。
コンパイルを通るように、を第一に書いていったので汚かったり効率悪かったりするかもしれません...

あとは`Command`に登録していって、`output`を読んであげれば完了です。
ただし`output`はバイト列を返すみたいなので、`String::from_utf8_lossy`を使って、文字に直してあげます。

#### 追記(2014/12/21)

---

このままだと標準入力や標準出力、標準エラー出力が子プロセスと結びついておらず、仮想のパイプとつながったような状態になるようです。
そのため、標準入力から読み取るような関数を実行しようとするとそのプロセスは失敗します。

修正したのが以下のコードです。(`Command`を作成する部分のみ)

```rust
    let mut output = match Command::new(first_cmd[0].as_slice())
                              .args(first_cmd.as_slice().tail())
                              .stdin(StdioContainer::InheritFd(STDIN_FILENO))
                              .stdout(StdioContainer::InheritFd(STDOUT_FILENO))
                              .stderr(StdioContainer::InheritFd(STDERR_FILENO))
                              .spawn() {
        Ok(p) => p,
        Err(e) => panic!("ERROR: {}", e)
                              };
    match output.wait() {
        Ok(e) => println!("Exit: {}", e),
        Err(e) => println!("Error: {}", e)
    }
```

このように、`stdout`を標準出力と関連付けることで、前のコードでの`output`から読み取って文字列に変換して出力、という手順は不要になりました。
C 言語では`fork`したら`wait`しろと言われて育ってきたので、とりあえず`wait`を入れてみていますが、どうも`wait`を入れなくてもゾンビプロセスが残っているようには見えないので謎です。
調査中です。

### 追記ここまで

## main

先ほどの`exec`関数を`main`で使える形にしてみます。

```rust:main.rs
use std::io;
mod parse;
mod exec;

fn main() {
    loop {
        print!("% > ");
        let input = std::io::stdin().read_line().ok().expect("Failed to read.");
        let parser = parse::Parser::new(input);
        let tokens: Vec<parse::Token> = parser.collect();
        exec::exec(tokens);
    }
}
```

プロンプトを表示してからユーザーの入力を受付け、パースして`Vec<Token>`になおしてから`exec`に渡しています。

## 実行結果

```
% > echo hello
hello
% > echo "hello world"
"hello world"
% >
```

あれ？`"`の扱いなんて考えてなかったのになぜかうまくいってますね。不思議。

検証用に`main.rs`を書き換えて実行してみました。

```
% > echo hello
Str(echo)
Str(hello)
hello
% > echo "hello world"
Str(echo)
Str("hello)
Str(world")
"hello world"
% > echo "hello     world"
Str(echo)
Str("hello)
Str(world")
"hello world"
% >
```

`echo`がうまくやってくれているみたいですね。すごい。

## 今後

次はリダイレクト関連ですかね。パイプや&よりも簡単そうだし。
毎度のことですが、ご指摘大歓迎です！色々な意見を伺いたくてこれを書いているので、何かありましたらぜひよろしくお願いします！

[agatan/rsh](https://github.com/agatan/rsh)
