---
title: "RubyでLinuxコマンドの再実装(ls編)"
date: 2014-10-13T23:10:08+09:00
tags: ["Ruby", "Linux"]
url: https://qiita.com/agatan/items/af0c3bbc881f60667c85
---

Ruby の勉強に Ruby で有名ドコロの Linux コマンドたちを再実装してみたいとおもいます。

少しずつ追記する形にできればと思っています。よりよい実装方法などありましたらぜひご教授いただけると幸いです。

[Ruby で Linux コマンドの再実装(ls 編) - Qiita](http://qiita.com/agatan/items/af0c3bbc881f60667c85)
[Ruby で Linux コマンドの再実装(tree 編) - Qiita](http://qiita.com/agatan/items/4c50554ae22aa4181cc1)

## ls

ではまずは`ls`です。簡単そうだったし個人的に一番良く使うコマンドなので。

```rb
#! /usr/bin/env ruby
#
# ls: 引数なしの場合現在のディレクトリの状態を返す。
#     引数ありの場合そのディレクトリの状態を返す。

target = ARGV[0] || Dir.pwd

entries = Dir::entries(target)

entries.select!{|entry| entry[0] != '.'}

entries.map! do |entry|
  if File::directory?(File.join(target, entry))
    "#{entry}/"
  else
    entry
  end
end

puts entries.join(' ')
```

ディレクトリの場合は末尾に`/`をつけるようにしています。

`-a`オプションで隠しファイル隠しディレクトリも表示するようにしたり, `--color`オプションで色をつけたりすることが目標です。

コマンドライン引数の順番や, `-a -l -F`と`-alF`などややこしそうな部分は制約をつけてお茶を濁してしまうかもしれません...

### 追記

---

@riocampos さんにコメントでコマンドライン引数の利用方法を教えていただきました！
`require 'optparse'`とすればパーサが利用できるようになります。

というわけでこれを反映し修正したものがこちらです。

```ls.rb
#! /usr/bin/env ruby -w
#
# ls: 引数なしの場合現在のディレクトリの状態を返す。
#     引数ありの場合そのディレクトリの状態を返す。

require 'optparse'

def set_color_and_reset(entry, color)
  case color
  when :red
    "\e[31m#{entry}\e[0m"
  when :green
    "\e[32m#{entry}\e[0m"
  when :blue
    "\e[34m#{entry}\e[0m"
  when :cyan
    "\e[36m#{entry}\e[0m"
  else
    entry
  end
end

options = ARGV.getopts('aF', 'color')
target = ARGV[0] || Dir.pwd

entries = Dir.entries(target)

if options['a']
  options['a'] = false
else
  entries.select! { |entry| entry[0] != '.' }
end

if options.value? true
  entries.map! do |entry|
    fullpath = "#{target}/#{entry}"
    case
    when File.directory?(fullpath)
      entry = set_color_and_reset entry, :blue if options['color']
      entry += '/' if options['F']
    when File.symlink?(fullpath)
      entry = set_color_and_reset entry, :cyan if options['color']
      entry += '@' if options['F']
    when File.executable?(fullpath)
      entry = set_color_and_reset entry, :red if options['color']
      entry += '*' if options['F']
    end
    entry
  end
end

puts entries.join(' ')
```

`-l`オプションは対応していません...`-aF --color`に対応しました。
表示の桁揃えとかしたほうが綺麗なんでしょうけれど...

次は`tree`に挑戦してみます。
