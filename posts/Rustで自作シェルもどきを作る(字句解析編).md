---
title: "Rustで自作シェルもどきを作る(字句解析編)"
date: 2014-12-21T01:15:19+09:00
tags: ["Rust"]
url: https://qiita.com/agatan/items/8a097ead46df1c1659ff
---

Rustが最近とてもおもしろいので、勉強がてら自作シェルみたいなものを作ってみたいと思います。  
とはいえ、C言語でさえろくにシステムプログラムを書いたことがないので、道は険しくなりそうです。

## バージョン
0.13.0-nightlyを使用しました。

コード全文はこちら
[agatan/rsh](https://github.com/agatan/rsh)

## 字句解析
さて、シェルといったらまずはユーザの入力を受け付けてパースし、コマンドを実行しなくてはなりません。  
というわけでまず初めにパース部分についてやってみます。  
さくっと終わらせたかったのですが、どうもまだ`String`と`&str`とかそのへんで詰まってしまいます...

## Tokenの規定
`enum`を使って`Token`を列挙します。  
一応最終的にはパイプやらリダイレクトやらも実装したいなーと思っているので、その辺を考慮に入れた実装にしてみました。

```rust
enum Token {
	Str(String),
	Pipe,
	RedirectTo,
	RedirectFrom,
	Ampersand,
}
```

`Str`は特殊な文字以外の文字列ですから、要素として`String`を保持させておきました。

## parser
パースには(おもしろそうだったので)`iterator`トレイトを実装させることにしました。  
実際つかうときにはいらない気もしますが、ちょっとためしたかったので。

構造体として`Parser`を作ります。ソースとなる文字列と、現在どこまでパース済みなのかを保持する`current`を持たせました。

```rust
pub struct Parser {
    src: String,
    pub current: uint,
}
```

この構造体に`Iterator`トレイトを実装すればよいのですが、補助関数としていくつか実装しておきます。

```rust
impl Parser {
    pub fn new(src: String) -> Parser {
        Parser { src: src, current: 0 }
    }

    pub fn current_char(&self) -> char {
        self.src.char_at(self.current)
    }

    fn skip_whitespace(&mut self) {
        while self.current_char().is_whitespace() {
            self.current += 1;
            if self.current >= self.src.char_len() {
                return;
            }
        }
    }

    fn get_pipe(&mut self) -> Option<Token> {
        if self.current_char() == '|' {
            self.current += 1;
            self.skip_whitespace();
            Some(Token::Pipe)
        } else {
            None
        }
    }

    fn get_ampersand(&mut self) -> Option<Token> {
        if self.current_char() == '&' {
            self.current += 1;
            self.skip_whitespace();
            Some(Token::Ampersand)
        } else {
            None
        }
    }

    fn get_redirect_to(&mut self) -> Option<Token> {
        if self.current_char() == '>' {
            self.current += 1;
            self.skip_whitespace();
            Some(Token::RedirectTo)
        } else {
            None
        }
    }

    fn get_redirect_from(&mut self) -> Option<Token> {
        if self.current_char() == '<' {
            self.current += 1;
            self.skip_whitespace();
            Some(Token::RedirectFrom)

    fn get_str(&mut self) -> Option<Token> {
        let mut i = self.current;
        while !KEYWORDS.contains_char(self.src.char_at(i)) {
            i += 1;
        }
        if i == self.current {
            None
        } else {
            let result = Some(Token::Str(self.src.slice_chars(self.current, i).to_string()));
            self.current = i;
            self.skip_whitespace();
            result
        }
    }
}

```

`new`は新しいパーサーを生成するための関数です。`Parser`は文字列の所有権を要求していますが、入力された文字列はパースする以外に使い道は無いと思ったので大丈夫と判断しました。

`current_char`は現在注目している文字を返します。
`skip_whitespace`は空白文字を飛ばすように`self.current`をいじります。パーサーでは、基本的に1トークンを読み終えたら空白を飛ばして次のトークンになりうる文字の先頭までジャンプするべきなので、必ずトークンを読んだらこの関数を呼び出します。

のこりの関数は、それぞれの`Token`を取得することを試みる関数です。  
`Option<Token>`が帰ってくるので、`None`が帰ってきたら今みているトークンは別の種類のものであるといえます。

これらを用いて`Iterator`を実装しました。

```rust
impl std::iter::Iterator<Token> for Parser {

    fn next(&mut self) -> Option<Token> {
        if self.current >= self.src.char_len() {
            return None;
        }
        let mut result: Option<Token> = self.get_pipe();
        if result.is_some() { return result; }
        result = self.get_ampersand();
        if result.is_some() { return result; }
        result = self.get_redirect_to();
        if result.is_some() { return result; }
        result = self.get_redirect_from();
        if result.is_some() { return result; }
        result = self.get_str();
        if result.is_some() { return result; }
        None
    }
}
```

きれいじゃないコードですが、うまいやり方が他に思いつかなかったので...  
純粋にある種類のトークンを取得しようと試みて`None`が帰ってきたら別の種類で試す、ということを繰り返しています。
先頭で末尾まで読み込んだかを判定しています。
すべての条件に当てはまらなくなったらパース失敗で`None`を返しています。

## 試す

```rust
use std::io;
mod parse;

fn main() {
    loop {
        let input = std::io::stdin().read_line().ok().expect("Failed to read.");
        let mut parser = parse::Parser::new(input);
        for token in parser {
            println!("{}", token);
        }
    }
}
```

実行結果

```
ls -a | grep foo
Str(ls)
Str(-a)
Pipe
Str(grep)
Str(foo)
ls -a| grep foo>result.txt &
Str(ls)
Str(-a)
Pipe
Str(grep)
Str(foo)
RedirectTo
Str(result.txt)
Ampersand
```

このような感じになりました。
`Iterator`を実装しているので、`for .. in ..`が使えて気持ち良いです。

## 反省点
パースは失敗しうる計算だから`Option`かなーどうせ`Option`かえすなら`Iterator`実装しちゃえばお得かなーとおもって漠然と実装してみましたが、パースは失敗した理由がほしいことがほとんどなのでよく考えたら`Result`を使うべきだった気がしてきました。
効率とかは正直Rustでの効率のよい書き方がよくわかっていないのであまり気にせず、とりあえず動くものを、と作ってみました。
あとはパーサを書いたことが殆どなかったので成功法がわからなかったので、もっときれいな書き方があるんじゃないかという気も...

## 今後
とりあえず動くものを、コードをたくさん書こう、の精神で進めてみます。
次は単純なコマンド実行を実装したいです。
といってもRustには`Command`とか`Process`とかがあって、ちょっと読んで見た感じ割りと素直にC言語の`execvp`とかを呼び出しているようなので、それを使えばそこまで難しくはないのかな？

ソースの全文を掲載しますので、Rust固有であってもそうでなくても、より良い書き方などありましたらご教授いただけると幸いです。よろしくお願いします。

```rust:parse.rs
use std;

static KEYWORDS: &'static str = "|&<> \n";

#[deriving(Show)]
pub enum Token {
    Str(String),
    Pipe,
    RedirectTo,
    RedirectFrom,
    Ampersand,
}

pub struct Parser {
    src: String,
    pub current: uint,
}

impl Parser {

    pub fn new(src: String) -> Parser {
        Parser { src: src, current: 0 }
    }

    pub fn current_char(&self) -> char {
        self.src.char_at(self.current)
    }

    fn skip_whitespace(&mut self) {
        while self.current_char().is_whitespace() {
            self.current += 1;
            if self.current >= self.src.char_len() {
                return;
            }
        }
    }

    fn get_pipe(&mut self) -> Option<Token> {
        if self.current_char() == '|' {
            self.current += 1;
            self.skip_whitespace();
            Some(Token::Pipe)
        } else {
            None
        }
    }

    fn get_ampersand(&mut self) -> Option<Token> {
        if self.current_char() == '&' {
            self.current += 1;
            self.skip_whitespace();
            Some(Token::Ampersand)
        } else {
            None
        }
    }

    fn get_redirect_to(&mut self) -> Option<Token> {
        if self.current_char() == '>' {
            self.current += 1;
            self.skip_whitespace();
            Some(Token::RedirectTo)
        } else {
            None
        }
    }

    fn get_redirect_from(&mut self) -> Option<Token> {
        if self.current_char() == '<' {
            self.current += 1;
            self.skip_whitespace();
            Some(Token::RedirectFrom)
        } else {
            None
        }
    }

    fn get_str(&mut self) -> Option<Token> {
        let mut i = self.current;
        while !KEYWORDS.contains_char(self.src.char_at(i)) {
            i += 1;
        }
        if i == self.current {
            None
        } else {
            let result = Some(Token::Str(self.src.slice_chars(self.current, i).to_string()));
            self.current = i;
            self.skip_whitespace();
            result
        }
    }
}

impl std::iter::Iterator<Token> for Parser {

    fn next(&mut self) -> Option<Token> {
        if self.current >= self.src.char_len() {
            return None;
        }
        let mut result: Option<Token> = self.get_pipe();
        if result.is_some() { return result; }
        result = self.get_ampersand();
        if result.is_some() { return result; }
        result = self.get_redirect_to();
        if result.is_some() { return result; }
        result = self.get_redirect_from();
        if result.is_some() { return result; }
        result = self.get_str();
        if result.is_some() { return result; }
        None
    }
}
```

```rust:main.rs
use std::io;
mod parse;

fn main() {
    loop {
        let input = std::io::stdin().read_line().ok().expect("Failed to read.");
        let mut parser = parse::Parser::new(input);
        for token in parser {
            println!("{}", token);
        }
    }
}
```
