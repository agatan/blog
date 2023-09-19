---
title: "Zig Snippets: string to enum"
postSlug: zig-snip-string-to-enum
pubDatetime: 2022-08-31T13:14:35.000Z
tags: ["zig"]
---

## Zig で `[]const u8` から enum に変換する

たぶん builtin にも存在しない気がする？のですが、逆 (enum → string) は `@tagName` で可能なので、 `@typeInfo` や `inline for` をくみあわせて以下のように書くことができます。

```zig
const std = @import("std");

const E = enum { a, b };

fn strToE(s: []const u8) ?E {
    inline for (@typeInfo(E).Enum.fields) |f| {
        if (std.mem.eql(u8, f.name, s)) {
            return @intToEnum(@This(), f.value);
        }
    }
    return null;
}
```

`inline for` 便利。
