---
title: Go で構造の一部が動的に変わる JSON を扱いたい
pubDatetime: 2018-01-03
tags: ["Go"]
---

json.RawMessage を使うと，一部のフィールドを見てから payload の型を決定することができます

## 問題

WebSocket でやりとりするサーバを書いていて，一つのコネクション上でいくつかの種類のコマンドを JSON として受け付けるような仕組みが欲しくなりました．

```json
{
  "action": "increment",
  "payload": {
    "value": 3
  }
}
```

と

```json
{
  "action": "greet",
  "payload": {
    "name": "World"
    "language": "English"
  }
}
```

のように， `action` フィールドに応じて `payload` の構造が変わるという構成です．

golang でふつうに JSON を受け取って構造体にマップする際には，下のようにします．

```go
type Message struct {
  Action string `json:"action"`
  Payload struct {
    Value int `json:"value"`
  } `json:"payload"`
}
var msg Message
json.Unmarshal(body, &msg)
```

この方法では， `action` の値に応じて `payload` の構造が変わることを表現できません．無理やり `Payload` の struct を大きくして

```go
struct {
  Value int `json:"value"`
  Name string `json:"name"`
  Language string `json:"language"`
}
```

のようにすることも出来ますが，どの action の時にどのフィールドが使えるのかわかりにくい上に， `value` のような汎用的な名前のフィールドが複数の型を取るケースを表現できません．( `{ "value": 1 }` と `{ "value": "World" }` など）

## 解決方法

`json.RawMessage` を使って，動的な部分の unmarshal を遅延します．

```go
type Message struct {
  Action string `json:"action"`
  Payload json.RawMessage `json:"payload"`
}
var msg Message
json.Unmarshal(body, &msg)
```

このようにすると， `msg.Payload` には `` []byte(`{"value": 1}`) `` がそのまま残ります．

そこで，それぞれの action ごとの payload 型を定義しておき

```go
type IncrementPayload struct {
  Value int `json:"value"`
}
type GreetPayload struct {
  Name string `json:"name"`
  Language string `json:"language"`
}
```

`msg.Action` に応じて， `msg.Payload` をどう unmarshal するか決定します．

```go
switch msg.Action {
case "increment":
  var p IncrementPayload
  json.Unmarshal(msg.Payload, &p)
  // do increment action
case "greet":
  var p GreetPayload
  json.Unmarshal(msg.Payload, &p)
  // do greet action
}
```

## まとめ

- 動的に構造が決定する JSON を受け付けたかった
- json.RawMessage を使うと，一部のフィールドだけ unmarshal 操作を遅延できる
- 構造の決定に必要な部分だけ先に unmarshal して，そこを見てから残りを unmarshal する

動的に構造が決定する部分を何かしらの interface として抽象化できる場合は，特にこの手法が有効そうです．

そもそも `{ "action": "increment", "increment_payload": { "value": 1 } }` ， `{ "action": "greet", "greet_payload": { "name": "world", "language": "English" } }` のようにフィールド名を変えてしまうという手もありますね．
