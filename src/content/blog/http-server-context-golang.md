---
title: HTTPサーバとcontext.Context
pubDatetime: 2017-12-25
tags: ["Go"]
---

# HTTP サーバと context.Context

golang で HTTP サーバを書く際に，どう context.Context を活用したら良いか，今考えていることをまとめておきます

## はじめに

golang における強力な道具の一つに `context.Context` interface というのがあります．キャンセルをサブルーチンに伝搬したり，限られたスコープ内で一貫してアクセスできるインメモリ KV ストア的な役目を担っています．

サーバにおいても，リクエストがキャンセルされたら handler の内部で行う高コストな計算や外部リソースへのアクセスもキャンセルできるようにしておくのが望ましいはずです．また，リクエストスコープに閉じる値を保持するインメモリ KV ストアとしての役割も context で担えます．

そこで，HTTP サーバを書く際にどのように`context.Context` を活用するのが良いかをまとめたいと思います．

## キャンセルの伝搬

`http.Request` は `func(*Request) Context() context.Context` というメソッドを持っています．これは，サーバから見た場合以下のようなケースにおいて，リクエストのキャンセルを伝えるために使われています．(ref: [https://golang.org/pkg/net/http/#Request.Context](https://medium.com/r/?url=https%3A%2F%2Fgolang.org%2Fpkg%2Fnet%2Fhttp%2F%23Request.Context))

1.  コネクションが切断された場合
2.  リクエストがキャンセルされた場合（HTTP/2）
3.  一つの Handler の `ServeHTTP` に渡され，return してきた場合（= Handler によって適切にハンドリングされた場合）

したがって， `http.Handler.ServeHTTP` の中ではこの context を見ることで，その Request の処理を続ける必要があるか判定することができます．

```go
func doSomething(ctx context.Context, body io.ReadCloser) error {
	// 重たい処理 (DB アクセス，外部 API，...）
	select {
	case <-ctx.Done():
		return ctx.Err()
	default:
		// ...
	}
	return nil
}

type Server struct{}

func (s *Server) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	ctx := r.Context()
	if err := doSomething(ctx, r.Body); err != nil {
		// ...
	}
}
```

## インメモリ KVS

context をインメモリ KVS 的に扱いたい場合， `context.WithValue` を使います． これは，ベースとなる context をもとに，エントリを追加した新しい context を返す関数です．

すでにある context を mutate するのではなく，新しい context を生成するため， `context.WithValue(req.Context(), key, value)` としても `req.Context()` にはなんの変化も起こりません．新しく作られた context を使うようにして上げる必要があります．

そこで `http.Request.WithContext` を使います．引数に新しい context をとって，その context がセットされたリクエストの shallow copy を返します．

どんなケースに使われるかというと，middleware パターンを使ったケースが挙げられます． `func (h http.Handler) http.Handler` のような関数を定義しておくことで，組み合わせて使用可能な機能の部品を作れます．典型的にはログ出力や認証，panic ハンドリングなどが middleware として実装されることが多いです．

```go
func findUser(name string) (id int, found bool) {
	return 123, true
}

type Middleware func(h http.Handler) http.Handler

func Authentication(h http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		user, ok := findUser(r.URL.Query().Get("name"))
		if ok {
			ctx := context.WithValue(r.Context(), "user-id", user)
			r = r.WithContext(ctx)
		}
		h.ServeHTTP(w, r)
	})
}
```

Type caption for embed (optional)

このような形で， Request に紐づく context に値を追加し，再度 `Request.WithContext` で紐付け直すようにします．

値を Get する側は， `r.Context().Value("user-id")`でアクセスすることができます．

通常は package private な const として key を定義しておき，Get/Set を行う utility 関数を用意することで，直接ハンドラ内で `ctx.Value` や `context.WithValue` を呼ばないようにします．（でないと実質なんでも入る静的型情報もないグローバル変数になり，管理できなくなるため）

## context をどう渡すか

context の生まれた経緯が原因なのか，context を Request から取得するのではなく，明示的に渡す形にしている WAF やライブラリが結構あったりします．

`type HandlerCtx func(ctx context.Context, w http.ResponseWriter, r *http.Request)` を基本のハンドラに据えて，middleware も `func(h HandlerCtx) HandlerCtx` のように定義しているといったものです．

個人的には今から書くプログラムは，この形式の context 伝搬は避けるべきだと思っています． `http.Request.Context()` がすでにあるのに，それとは異なる context を渡すということは，context の階層構造の分岐を招きます．実際には `handler(r.Context(), w, r)` のように呼び出すことで分岐は避けることが出来ますが，それでも混乱は生じるはずです．

`context.WithDeadline` や `context.WithCancel` によって作られた context が第一引数に渡った場合，handler 内部ではその context の親に `http.Request.Context()` が含まれているかどうか判断することができません．そのため，第一引数の context と `r.Context()` の両方をチェックしなければならないのでは，という疑問を生じてしまいます．

middleware は `func(h http.Handler) http.Handler` として定義し，新しく context を使いたい場合は必ず `r.Context()` を親に context を作り， `r.WithContext` でセットしなおすようにしたほうが良いはずです．

## context に何を保存するか

context にはリクエストローカルなオブジェクトだけを入れるようにします．典型的にはユーザ認証の結果などです．

ユーザ認証は様々なエンドポイントで行いたいので DRY にするべく middleware として実装するのにもってこいな一方で，権限のチェックはエンドポイントごとに違う処理が必要になるため middleware 内で行うことは出来ません．

そこで認証の結果であるユーザオブジェクトを `r.Context()` の中に入れておくことで，ハンドラ側で簡単に取り出して任意のチェックをすることができます．

一方，リクエストをまたぐデータには，例えばロガーや newrelic などの外部サービスのクライアント，DB クライアントなどが挙げられます．

このようなリクエストを跨いてサーバ起動中にはずっと生かしておきたいオブジェクトは，素直に `http.Handler` を実装する構造体のなかに入れておくのがいいと思います．

```go
type Server struct {
	db     *sql.DB
	logger *log.Logger
}

func (s *Server) ServeHTTP(w http.ResponseWriter, r *http.Request) {
  s.db.Exec("...")
  // ...
}
```

Type caption for embed (optional)

## まとめ

実は今まで `http.Request.Context()` がサーバから見た時にどういう振る舞いをするのかあまり把握していませんでした．クライアントから見た場合，「この context にキャンセルを通知するとリクエストをキャンセルしてくれる」という役割であることは知っていましたが，サーバ側では使ったことがありませんでした．

context とサーバの話は今更感の強い話ではあるなぁと正直思いましたが，意外と `func(ctx context.Context, w http.ResponseWriter, r *http.Request)` 形式で書かれたサンプルも見つかったのと，[https://github.com/gin-gonic/gin](https://medium.com/r/?url=https%3A%2F%2Fgithub.com%2Fgin-gonic%2Fgin) のようなフレームワークを使っていると忘れがちな部分だと思ったのでまとめてみることにしました．
