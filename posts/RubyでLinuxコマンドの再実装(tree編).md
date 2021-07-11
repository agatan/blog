---
title: "RubyでLinuxコマンドの再実装(tree編)"
date: 2014-10-16T16:20:33+09:00
tags: ["Ruby","Linux"]
url: https://qiita.com/agatan/items/4c50554ae22aa4181cc1
---

Rubyの勉強にRubyで有名ドコロのLinuxコマンドたちを再実装してみたいとおもいます。

少しずつ追記する形にできればと思っています。よりよい実装方法などありましたらぜひご教授いただけると幸いです。

[RubyでLinuxコマンドの再実装(ls編) - Qiita](http://qiita.com/agatan/items/af0c3bbc881f60667c85)
[RubyでLinuxコマンドの再実装(tree編) - Qiita](http://qiita.com/agatan/items/4c50554ae22aa4181cc1)

## tree
`tree`コマンドに挑戦します。
`tree`コマンドは, 指定したディレクトリ以下のファイルを木構造にして表示してくれます。

![スクリーンショット 2014-10-16 16.16.08.png](https://qiita-image-store.s3.amazonaws.com/0/39030/b3ca62be-bc22-ec39-a836-70032b76521d.png "スクリーンショット 2014-10-16 16.16.08.png")

```tree.rb
#! /usr/bin/env ruby -w
#
# tree: 引数で指定されたディレクトリ以下のファイルを
#       再帰的に木構造として表示する.

require 'optparse'

options = ARGV.getopts('aF')

# parentは絶対パス.
def display_entries(parent, prefix, options)
  # '.', '..'を除く. 無限に再帰することを防ぐ
  entries = Dir.entries(parent).delete_if do |entry|
    entry == '.' or entry == '..' or !options['a'] && entry.start_with?('.')
  end

  entries.each_with_index do |entry, index|
    fullpath = File.join(parent, entry)
    entry = f_option(parent, entry) if options['F']
    # 最後の要素かどうか
    if index == entries.size - 1
      puts "#{prefix}└── #{entry}"
      next_prefix = prefix + '    '
    else
      puts "#{prefix}├── #{entry}"
      next_prefix = prefix + '│   '
    end
    if File.directory? fullpath
      display_entries(fullpath, next_prefix, options)
    end
  end
end

def f_option(parent, entry)
  case File.ftype(File.join(parent, entry))
  when "file"
    if File.executable? File.join(parent, entry)
      "#{entry}*"
    else
      entry
    end
  when "directory"
    "#{entry}/"
  when "link"
    "#{entry}@"
  else
    entry
  end
end

target = ARGV[0] || '.'
target_fullpath = File.absolute_path target
init_prefix = ''

puts target
display_entries target_fullpath, init_prefix, options
```

ファイルのタイプを調べる際の処理をls編とちょっとだけ変えてみました。
シンボリックリンクのリンク先を表示する修正が必要ですね...
`color`は`ls`のときとほとんど変わらないので省きました。
`File.ftype`では実行可能ファイルかどうかは判定できないんですかね??そこがちょっと気になります。
あとはオプションのあたりなど複雑になってしまっているので綺麗にしたいです。

