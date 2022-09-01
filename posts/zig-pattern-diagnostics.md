---
title: "Zig Patterns: Diagnostics"
date: 2022-08-31
tags: ["zig"]
---

最近流行りの zig について、どうやら self-hosting compiler がデフォルトになったという話を聞いて興味を持ち、ちょっと実装を読んだりしながら遊んでみています。
いろいろと特徴的な言語なのですが、今回は error の話です。

## 前提: Zig の error handling

zig には error union という機構があります。詳細は [公式ドキュメント](https://ziglang.org/documentation/master/#Errors) に譲るとして、ざっくり以下のように使います。

```zig
fn fallible() !i32 {
    return error.Failed;
}
// or
fn fallible2() error{Failed}!i32 {
    return error.Failed;
}
// or
const Error = error{Failed, AnotherError, YetAnotherError};
fn fallible2() Error!i32 {
    return error.Failed;
}

// caller
fn f() !void {
    const x: i32 = try fallible();
    std.debug.print("{}\n", .{x});
}
```

ぱっと見は Rust の `Result<T, E>` に似ているのですが、大きな違いとして、Zig の error は単なるタグ以上の情報を持てません。(リソースを開放するもの難しいし、union のサイズが膨らんだりするといろいろ面倒そうなので、仕方なさそう。)
エラーによっては別にタグだけで十分なのですが、もうちょっと詳細な情報がないとエラーとしては不親切になってしまうこともあるかと思います。
(たとえば JSON パーサなどを考えると「どこでエラーが発生したか」のような情報があるとうれしいはず)

## Pattern: Diagnostics

そこで頻出するパターンが `Diagnostics` パターンです。
エラーを蓄積する object を作って、そのポインタを渡すことで、詳細なエラーレポートを可能にします。

```zig
const std = @import("std");

const Diagnostics = struct {
    allocator: std.mem.Allocator,
    messages: std.ArrayList([]const u8),

    fn init(allocator: std.mem.Allocator) @This() {
        return .{
            .allocator = allocator,
            .messages = std.ArrayList([]const u8).init(allocator),
        };
    }

    fn deinit(self: *@This()) void {
        for (self.messages.items) |msg|
            self.allocator.free(msg);
        self.messages.deinit();
    }

    fn emitf(self: *@This(), comptime fmt: []const u8, args: anytype) std.mem.Allocator.Error!void {
        const message = try std.fmt.allocPrint(self.allocator, fmt, args);
        try self.messages.append(message);
    }
};

fn doSomething(diag: ?*Diagnostics) !void {
    // Do something that may fail.
    const status: u32 = 1;
    if (status != 0) {
        if (diag) |d| {
            try d.emitf("non-zero status code: {}", .{status});
        }
        return error.Failed;
    }
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer if (gpa.deinit()) @panic("leak");
    var diag = Diagnostics.init(gpa.allocator());
    defer diag.deinit();

    doSomething(&diag) catch {
        var stderr = std.io.getStdErr().writer();
        for (diag.messages.items) |message| {
            try std.fmt.format(stderr, "{s}\n", .{message});
        }
        std.os.exit(1);
    };
}
```

パターンといっても、 「`Diagnostics` にどういった情報をもたせるか」はなんでもありで、アプリケーション・ライブラリ固有の設計が必要になってくるところです。
サンプルでは単に `[]const u8` (= string) をもたせるだけでしたが、たとえばパーサだったらエラーの位置情報やエラーレベルなんかがあると便利そうです。

とはいえ、ある程度いろいろ遊んでみてわかった Practice みたいなものはあるなと思ったので、紹介しておきます。

### Practice 1: 詳細なエラーレポートはオプショナルなものとして扱う

特にライブラリ作者にとって重要なポイントですが、エラーレポートのコストを支払うかどうかは、そのコードのユーザに決定権を委ねます。
たとえば「JSON パーサに渡す JSON が静的なファイルで絶対に失敗しないことがわかっている」というようなケースでは、 `Diagnostics` を初期化する面倒さを省きたいはずです。
また、Zig の活躍しそうなシーンである組み込みなどを想定すると、デバッグ時以外は詳細なエラーレポートなんて不要だからその分リソースをあけてほしい、というケースもありそうです。

そこで、 `Diagnostics` は optional (nullable) で受け取るようにしておくとよいです。
上記のサンプルで `diag: ?*Diagnostics` と宣言していたのはこのためです。

エラーレポートをつくる側がやや煩雑になる (`if (diag) |d| { ... }`) のが気になりますが、仕方なしと割り切っています。
pointer + nullable で表現するかわりに、 `Diagnostics` の内部で「エラーを書き込むか」を決定するやりかたもあります。

```zig
const Diagnostics = struct {
    messages: ?*std.ArrayList([]const u8) = null,
    allocator: ?std.mem.Allocator = null,

    fn emitf(self: *@This(), comptime fmt: []const u8, args: anytype) !void {
        if (self.messages) |messages| {
            const message = try std.fmt.allocPrint(self.allocator.?, fmt, args);
            try messages.append(message);
        }
    }
}

fn doSomething(diag: Diagnostics) !void {
    //
}

pub fn main() void {
    // if you don't need diagnostics.
    doSomething(.{});
}
```

こちらの方式は、エラーレポートをつくる側がシンプルになるという点は良いのですが、「エラーレポートのためだけの処理」がある場合には無駄な処理が実行されてしまうのを避けられません。

```zig
var gpa: std.mem.Allocator = ...;
var diag: Diagnostics = ...;

if (fail) {
    // エラーメッセージを構築するのに "{s}" より複雑な処理が必要になるようなケースを想定
    // エラーメッセージが不要ならこの allocation も不要なはず
    const detail_string = try self.allocPrintDetail(gpa, detail);
    defer gpa.free(detail_string);
    try diag.emitf("failed. detail = {s}", .{detail_string});
}
```

結局こういう無駄を極限まで避けようとすると、 `if (diag.enabled) { ... }` のような分岐が必要になります。
とはいえ大体のケースでは `Diagnostics` の外で allocation が必要になったりもしない、ということであれば、こちらのやりかたを採用してたまに明示的に `if (diag.enabled)` とするのもありかもしれません。

個人的には今のところ、 `?*Diagnostics` 形式のほうが素直で意図もわかりやすいかなという気がしています。

### Practice 2: `Diagnostics` のリソースは `Diagnostics` に管理させる

ようするに `Diagnostics` に allocator を渡して、 `diag.deinit()` で全部開放できるようにしよう、という話です。
Zig を書いていると「このメモリは誰が Own していて開放の責任をもっているのか」を常に意識して設計する必要があります。
`Diagnostics` の内部に `Diagnostics` が管理していないリソースがあると、 `deinit` がめちゃくちゃ面倒になります。
場合によっては一切動的な allocation が必要ないエラーレポート (たとえば完全にコンパイル時に決定する静的な message だけを表示したいとか) をつくることもあるかと思いますが、そういったものも諦めて `allocator.dupe(u8, static_message)` してしまうくらいのほうが壊しにくいです。
そもそも `Diagnostics` を必要としているくらいなので、リソースをそこまで厳密に切り詰める必要もないだろうと考えて、コードのシンプルさに振ったほうがいいなと思いました。
(先に書いたとおり、`Diagnostics` 自体が optional になっているし)


## まとめ

Zig を読み書きしていて面白いなとおもったので紹介でした。
ここまでリソースをしっかり自前で管理しないといけない言語はあまり触ってこなかったので、おもしろプラクティスがいっぱいありますね。 (C++ や Rust はそれなりに書いてきたつもりだけど、スマポでぽん、の範囲で大体完結していた。)
