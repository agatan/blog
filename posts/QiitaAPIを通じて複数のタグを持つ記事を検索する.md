---
title: "QiitaAPIを通じて複数のタグを持つ記事を検索する"
date: 2014-10-13T18:11:43+09:00
tags: ["Qiita","Ruby"]
url: https://qiita.com/agatan/items/55dce1d2bc887de0cd71
---

Ruby初心者なのでなにかお気づきの点がありましたらご教授いただけると幸いです。

## QiitaApiAccessor
QiitaAPIとの通信, 受信情報のハッシュ化を担当している。
`@result_json`をローカル変数にしたいのだけど, `open`ブロック内のローカル変数になってしまうので出来ない...
`get_with_tag`内のローカル変数にならできるけど結局`result_json = {}`として`open`内で更新みたいな書き方になってしまって綺麗じゃない...

```ruby
require 'open-uri'
require 'json'

class QiitaApiAccessor

  def initialize
    @qiita_url = 'http://qiita.com/api/v2'
    @result_json = {}
  end

  def get_with_tag(tag_name)
    open("#{@qiita_url}/tags/#{tag_name}/items?page=1&per_page=100") do |uri|
      @result_json = JSON.parse(uri.read, symbolize_names: true)
    end
    @result_json
  end

end
```

## QiitaArticle
一つの記事についての情報を保持する。
`has_tag?`メソッドでは, 大文字小文字にかかわらず判定が出来るよう`upcase`を使用している。

```rb
class QiitaArticle
  
  def initialize (article_hash)
    @data = article_hash
    @title = article_hash[:title]
    @url = "http://qiita.com/api/v2/items/#{article_hash[:id]}"
    @tags = article_hash[:tags].map { |tag| tag[:name] }
    @body = article_hash[:body]
  end

  attr_reader :title, :url, :tags, :body

  def has_tag?(tag_name)
    tags = @data[:tags]
    tags.any?{ |tag_info| tag_info[:name].upcase == tag_name.upcase }
  end

end
```

## MultiTagsSearcher
これがメインのクラス。コンストラクタに`QiitaApiAccessor#get_with_tag`の戻り値と検索したいタグのリストを渡してやる。
`group_by_relevance`によって含んでいる対象タグの数によってグループ化する。

```rb
class MultiTagsSearcher

  def initialize(articles_data, tags)
    @articles = articles_data.map { |data| QiitaArticle.new(data) }
    @tags = tags
  end

  attr_reader :articles

  def group_by_relevance
    @articles.group_by do |article|
      @tags.count { |tag| article.has_tag?(tag) }
    end
  end

end
```

## 使い方
`QiitaApiAccessor#get_with_tag`にメインとなるキーワードタグを渡す。
その戻り値と、キーワードタグを含む検索したいタグ名のリストを、`MultiTagsSearcher`に渡して`group_by_relevance`してやる。
すると`含んでいる対象タグ数 => [QiitaArticleのインスタンス]`というハッシュが得られるので、適当に`QiitaArticle#url`とかで記事のurlを取ってこられます。

## 問題点・改善点
QiitaAPIの都合上一度に100件までしか記事の情報を取得できないので, 本当はループで全件取得してから`MultiTagsSearcher`に渡したい。
が、そうすると5000件とか普通に行く場合もあるから`QiitaArticle`のインスタンスが無駄に大量生成されてしまうし、時間もかかる。
なので、検索の際のキーワードタグには記事数の少なそうなものを選ぶことでお茶を濁す...
`ruby`ではなく`sinatra`をキーワードにするだけで結果がかなり違ってくる。
けどそれじゃ根本的な解決になってない。なにか良い方法ありましたら教えて下さい。
