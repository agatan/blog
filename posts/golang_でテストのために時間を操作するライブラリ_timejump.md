---
title: "golang でテストのために時間を操作するライブラリ timejump"
date: 2017-12-14T14:21:24.000Z
tags: ["Go"]
---

現在時刻に依存するコードをテストするとき，golang で `time.Now` を普通に使っているとモックできずうまくテストが書けないという問題があります．
時間の操作は time パッケージをそのまま使えば良いのですが，time.Now だけはモックできるようにしたいところです．

解決方法としては，グローバル変数に `var NowFunc func() time.Time` を置いておいて，テスト時に入れ替えるという方法があり，ORM である gorm などが実際にこれを行っています．

[gorm/utils.go at 2a1463811ee1dc85d168fd639a2d4251d030e6e5 · jinzhu/gorm · GitHub](https://github.com/jinzhu/gorm/blob/2a1463811ee1dc85d168fd639a2d4251d030e6e5/utils.go#L21)

例:

```
var NowFunc = time.Now

func Do() string {
    return NowFunc().String()
}

func TestDo(t *testing.T) {
    now := time.Date(2009, time.November, 10, 23, 0, 0, 0, time.UTC)
    NowFunc = func() time.Time {
        return now
    }
    got := Do()
    if got != now.String() {
        t.Fail()
    }
}

```

golang の使用上，Ruby の timecop のようなことは出来ないので，こういう工夫をするしかありません．

なんとなくグローバル変数をテストのために置いて書き換えるのが嫌なのと，なんにも考えずに `t.Parallel()` を置けなくなるのがちょっと嫌だなと思っていました．
また，時間経過をシミュレーションしたい場合は，そういう `NowFunc` を毎回書く必要があり，結構面倒です．
あとパッケージをまたぐと厄介だし毎回書くのも嫌．

そこで，Ruby の timecop のように現在時刻をいじくり回せるようにするライブラリを作ってみました．

[github.com](https://github.com/agatan/timejump)

[godoc.org](https://godoc.org/github.com/agatan/timejump)

使用する際は `time.Now` をすべて `timejump.Now` に置き換える必要があります．（ Ruby と違って `time.Now` を直接上書きできないので...）
普段は `timejump.Now` と `time.Now` は `if !active { ... }` が一段挟まるだけなのでパフォーマンスに影響はほとんどないはずです．

テスト時は，`timejump.Now` の挙動を変えたいテストの頭で

```
func TestDo(t *testing.T) {
    timejump.Activate()
    defer timejump.Deactivate()
    ...
}

```

とします．

`timejump.Activate` な区間はロックをとっているので，テストを並列で走らせても並列に走らなくなります．

`timejump.Stop()` で時間停止，`timejump.Jump` で時間移動，`timejump.Move` でタイムゾーンの移動，`timejump.Scale` で時間の経過速度をいじれます．

時間を止めたいだけの場合はグローバル変数に `NowFunc` を持っておいて `t.Parallel` を間違って置かないように気をつけるほうが正直楽だとは思いますが，時間経過をテストしたい場合にはちょっと楽になるはずです．

もともとあるパッケージのテストをするために書いたパッケージだったのですが，目的だったテストを書く前にテストしたいパッケージが御役御免になってしまったので，timejump も御役御免になってしまいました．
いつか使う日が来る気がするので，ここに寝かせておきます．

---

---
