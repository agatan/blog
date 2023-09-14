---
title: "Rust で Unix のシグナルを channel 経由でキャッチする"
pubDatetime: 2017-07-10T11:59:04.000Z
tags: ["Rust"]
---

Rust でシグナルハンドリングをする必要があったのですが，あまり自分の用途にあるライブラリがなかったので作りました．
僕が Windows のことをほとんどわからないので，Windows 未対応です．

[github.com](https://github.com/agatan/signal-notify)

[docs.rs](https://docs.rs/signal-notify/0.1.2/signal_notify/)

[https://crates.io/crates/signal-notify](https://crates.io/crates/signal-notify)

golang の `signal.Notify` に寄せた API になっていて，標準ライブラリの `std::sync::mpsc::{Sender, Receiver}` 経由でシグナルを待ち受けることができます．

```
extern crate signal\_notify;
use signal\_notify::{notify, Signal};
use std::sync::mpsc::Receiver;

fn main() {
    let rx: Receiver<Signal> = notify(&[Signal::INT, Signal::USR1]);
    for sig in rx.iter() {
        match sig {
            Signal::INT => {
                println!("Interrupted!");
                break;
            }
            Signal::USR1 => println!("Got SIGUSR1!"),
        }
    }
}

```

Rust で Unix シグナルを取るライブラリとしては [GitHub - BurntSushi/chan-signal: Respond to OS signals with channels.](https://github.com/BurntSushi/chan-signal) というのが有名です．
こちらは標準ライブラリの `mpsc::channel` ではなく，`chan` クレイトの `channel` を使っています．
`chan` クレイトはケースによってはかなり便利で，

1. 複数の consumer を作れる (`receiver.clone()` ができる)
2. `chan_select!` マクロによって golang の `select` 的なことができる

という利点があります．

一方で複数 consumer にする必要がない & `chan_select!` が必要ないケースでは，シグナルハンドリングのためだけに `chan` にも依存するのもなんとなくはばかられるという気持ちがありました．
また，自分の目的として「`SIGWINCH` と `SIGIO` が取りたい」というのがあったのですが，`chan-signal` の仕組みだとデフォルトで無視されるシグナルをキャッチできない(macOS だけ)という問題もありました．
報告するときに方法を考えていたのですが，あまり自信がなかったのとほとんど完全に仕組みを書きなおす形になりそうだったので，自分の手元で `std::sync::mpsc` を使って実験してみたという経緯です．

## 仕組み

1. 初期化時にパイプを作る
2. シグナルごとに通知すべき `Sender` を覚えておく
3. シグナルごとに `sigaction` でハンドラをセットする
   - シグナルが来たらそれをパイプに `write(2)` する
4. シグナル待受＆通知用のスレッドを起動する
   - パイプからシグナル番号を読んで，適切な `Sender` に `send` する

という仕組みで動いています．
自信がなかったのは，「シグナルハンドラでやっていいこと一覧」をちゃんと把握していないという点です．
一応 `sigaction` の man を見ると `write` は読んでもいい関数一覧にいる気がするし，実際動いてはいるのでセーフだろうと判断しました．
（もしアウトだったら教えてください）

ちなみに `chan-signal` の方は，

1. シグナルごとに通知すべき `Sender` を覚えておく
2. 監視用スレッドを起動し，メインスレッドでは `pthread_sigmask` を使ってシグナルをブロックする
   - シグナルがすべて監視用スレッドに渡るようにする
3. 監視用スレッドで `sigwait` して適切な `Sender` に `send` する

という仕組みで動いているようです．
`sigwait` は指定したシグナルが投げられるまでブロックします．
ただし，macOS で `sigwait` の man を見ると，

> Processes which call sigwait() on ignored signals will wait indefinitely. Ignored signals are dropped immediately by the system, before delivery to a waiting process.

とあって，無視されるシグナルを `sigwait` で待っても補足できないようです．
Linux の man を見るとそんなことは書いていないし，普通に動くっぽいです．

今の実装だと，シグナルを受け取る `Receiver` がすべて閉じても，監視スレッドは動き続けるしハンドラも残り続けるので，これはなんとかしたいなぁと思っています．
アプリケーションの実行時間のうち，ある期間だけシグナルをとってそれ以外はスルーしたいというケースもそんなにないかなというのと，内部的な変更にしかならないので API が変わらないというのがあるので，この状態でとりあえず public にしました．

CLI を書いていると意外と普通に `SIGINT` は取りたくなることがあると思うので，ぜひ使ってみてください．
issue 報告等お待ちしています．

---

---
