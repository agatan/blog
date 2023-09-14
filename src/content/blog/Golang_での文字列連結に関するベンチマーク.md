---
title: "Golang での文字列連結に関するベンチマーク"
pubDatetime: 2015-09-08T08:09:45.000Z
tags: ["Go"]
---

# まず結論

`append` しよう. `bytes.Buffer` はそんなに速くない.

# きっかけ

こんな記事を見かけました.

[Go では文字列連結はコストの高い操作 - Qiita](http://qiita.com/ruiu/items/2bb83b29baeae2433a79)

`buf += "abc"` はコストが高いよーっていうお話ですね. これは golang にかぎらず, Java とかでもよく話題になる一般的な問題だと思います.

Java だと `StringBuilder` を使うのが良いとされていたと思いますが, golang だと解法がいくつかあるようです.

そこで, 解法をそれぞれ紹介した後, ベンチマーク結果を載せてみたいと思います.

## 1. 普通に `+=`

まずは普通に `+=` で連結していく方法です.

```
func AddString(n int) string {
    base := "abc"
    buf := ""
    for i := 0; i < n; i++ {
        buf += base
    }
    return buf
}

```

こんな感じですね.

これだと, 確実に n 回メモリ割り当てが発生するので遅いというのが問題となります.

## 2. `append` する

golang の `string` は, メモリ上では `[]byte` と同等の表現を持ちます.

そこで, `string` を `[]byte` として扱い, golang のスライスを伸長する `append` 関数を用いるという方法があります.

```
func AppendString(n int) string {
    base := "abc"
    buf := make([]byte, 0)
    for i := 0; i < n; i++ {
        buf = append(buf, base...)
    }
    return string(buf)
}

```

`make([]byte, 0)` によって, 長さ 0 のスライスを作って, それを伸長していく方法となっています.

このあたりは golang のスライスの表現について知っていないとわかりづらいと思うのですが, わかりやすい説明がいろいろなところで読めるので, ここでは説明しません.
`append` 関数は, スライスの len を必要な分だけ大きくします. また, その結果 len が スライスの cap を超える長さになる場合は, スライスの cap を必要以上に大きくすします.

これは, `append` を繰り返し適用するような場合(今回のように)に, メモリ割り当ての回数を最小にするためです. 一度の `append` で大きめにメモリを確保しておくことで, 次の `append` ではメモリ割り当てをしなくても済む可能性が生まれます.

イメージとしては,

| append の回数 | 0   | 1   | 2   | 3   |
| ------------- | --- | --- | --- | --- |
| len           | 0   | 3   | 6   | 9   |
| cap           | 0   | 8   | 8   | 16  |

こんな感じでしょうか(あくまでイメージですが)

`append` は 3 回呼ばれていますが, メモリ割り当ては 2 回に抑えられています.

その分, `+=` よりも速いだろうということですね.

### 2'. `cap` を十分量確保しておく

`make` によるスライスの作成の際には, 長さだけでなくキャパシティを指定することが出来ます.

したがって, はじめから `append` していった後の最終的なスライスの長さがわかっているのであれば, それをキャパシティに指定することで, メモリ割り当てを最小限に抑えることが可能になります.

```
func AppendStringWithCap(n int) string {
    base := "abc"
    buf := make([]byte, 0, 3*n)
    for i := 0; i < n; i++ {
        buf = append(buf, base...)
    }
    return string(buf)
}

```

## 3. `bytes.Buffer` を使う

Java の `StringBuilder` に近い解法ですね.

`bytes.Buffer` は文字通りバイト列のバッファリングを行ってくれます.

`bytes.Buffer` に文字列やバイト列を書き込んでいくと, 自動的にメモリ割り当てを減らすように計らってくれます.

```
func BufferString(n int) string {
    base := "abc"
    var buf bytes.Buffer
    for i := 0; i < n; i++ {
        buf.WriteString(base)
    }
    return buf.String()
}

```

こんな感じです.

# ベンチマーク結果

golang にはベンチマークをとる機能も標準で付いているので, それを利用しました.

```
package main

import "testing"

const N = 1000

func BenchmarkAddString(b *testing.B) {
    for i := 0; i < b.N; i++ {
        AddString(N)
    }
}

func BenchmarkAppendString(b *testing.B) {
    for i := 0; i < b.N; i++ {
        AppendString(N)
    }
}

func BenchmarkAppendStringWithCap(b *testing.B) {
    for i := 0; i < b.N; i++ {
        AppendStringWithCap(N)
    }
}

func BenchmarkBufferString(b *testing.B) {
    for i := 0; i < b.N; i++ {
        BufferString(N)
    }
}

func TestEquality(t *testing.T) {
    base := AddString(N)
    tests := []string{
        AppendString(N),
        AppendStringWithCap(N),
        BufferString(N),
    }
    for \_, test := range tests {
        if base != test {
            t.Fatal("not fair")
        }
    }
}

```

`TestEquality` は, すべての方法で正しく文字列を生成できていることを確認するためのテストです. ベンチマークには関係ありません.

## 結果

上記のファイルを用意した後, `go test -bench . -benchmem` とした結果を以下に示します.

```
PASS
BenchmarkAddString-8                5000        348347 ns/op     1592481 B/op        999 allocs/op
BenchmarkAppendString-8           200000          7346 ns/op       13056 B/op         12 allocs/op
BenchmarkAppendStringWithCap-8    300000          5461 ns/op        6144 B/op          2 allocs/op
BenchmarkBufferString-8           100000         16847 ns/op       12256 B/op          8 allocs/op
ok      github.com/agatan/bench 6.881s
```

というわけで, `make` の時点で十分なメモリを確保しておく 2' の方法が最も速く最もメモリを消費しないことがわかりました.

まぁ当たり前ですねｗｗ

より注目すべきは, 2 と 4 の結果です. 今回の結果だと, 最終的な文字列の長さがわからない場合, `bytes.Buffer` よりも `append` を使ったほうが速いという結果になっています (メモリ使用量は若干 `bytes.Buffer` のほうが小さい)

メモリ割り当ての回数も `bytes.Buffer` のほうが少なく済んでいるため, `[]byte` と `string` の変換など, 文字列連結以外の部分でのオーバーヘッドが大きいため, このような結果になった可能性があります. そこで, `N` の値を変えて実行してみました.

## N = 10 の場合

```
PASS
BenchmarkAddString-8             2000000           613 ns/op         224 B/op          9 allocs/op
BenchmarkAppendString-8          5000000           270 ns/op          96 B/op          4 allocs/op
BenchmarkAppendStringWithCap-8  10000000           142 ns/op          64 B/op          2 allocs/op
BenchmarkBufferString-8          5000000           251 ns/op         144 B/op          2 allocs/op
ok      github.com/agatan/bench 6.581s
```

## N = 10000 の場合

```
PASS
BenchmarkAddString-8                  50      28544042 ns/op    160274378 B/op     10039 allocs/op
BenchmarkAppendString-8            20000         71285 ns/op      160768 B/op         20 allocs/op
BenchmarkAppendStringWithCap-8     30000         55262 ns/op       65536 B/op          2 allocs/op
BenchmarkBufferString-8            10000        151665 ns/op      109280 B/op         11 allocs/op
ok      github.com/agatan/bench 7.393s
```

# 結論

連結する文字列の長さや連結の回数にもよるが, おおよそ `append` のほうが速い！！

`bytes.Buffer` はいつ使えばいいの...

---

---
