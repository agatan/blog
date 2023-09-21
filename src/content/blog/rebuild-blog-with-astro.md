---
title: "ブログをastroに載せ替えました"
postSlug: rebuild-blog-with-astro
pubDatetime: 2023-09-21 22:47:00
tags: ["雑談"]
---

このブログは元々Next.jsで構築していました。テンプレートなどは使っておらず、自前でMarkdownからコンテンツを作って静的にビルドし、Cloudflare Pagesから配信していました。（UIはChakraUI + Tailwind）

が、特にNext.jsである必要がなく、趣味としてもいまいちブログの機能をいじることに楽しみを見出してもいないので、もっとEasyな感じでやったほうが楽かなぁと思っていました。
（まぁそもそもいじってないのでEasyだろうがHardだろうが関係ないんですが。）

そんな折、 [Nord Theme を使い始めた](/posts/nord-theme) にも書いたように、全体的に自分の身の回りのものをNordに寄せていくというのをやっていて、そのブログ記事を書いているうちに、ブログも色合い変えたいなと思いました。
せっかくだしまるっと引っ越すか！というわけで、[Astro](https://astro.build/)を使ってみることにしました。Easyに寄せるつもりだったので、最初からテンプレートを探してそこに乗っかるぞ！という気持ちでいろいろさがしてみたところ、[AstroPaper | Astro](https://astro.build/themes/details/astro-paper/)というのがいい感じだったので、これに乗っかってみることにしました。

基本的にはRepositoryをまるっと持ってきて、コンテンツを配置してまわるだけですが、frontmatterに必要な情報が足りなかったりしたので、そのへんはスクリプトを書いて対応しました。 ([載せ替えたときのPR](https://github.com/agatan/blog/pull/8))

Google Analytics対応を[partytown](https://partytown.builder.io/)経由で行っています。(partytownって[qwik](https://qwik.builder.io/)を作っているbuilder.io製だったんですね)
astroとのintegrationがオフィシャルドキュメントにかかれているので、それに従ってつらつらと作業しただけです。
まぁぶっちゃけ個人ブログでアクセス解析なんてしなくていいんですが、たまに眺めると意外とおもしろいので。

また、Social Linksの設定や[About](/about)の中身をいじりました。
Social Linksは ~~Twitter~~ X のアイコンを追加したり。最近は主に[bluesky](https://bsky.app/profile/agatan.bsky.social)にいるので、そっちを先頭にもってきたかったのですが、SVGアイコンが見つけられず。ここに画像であのアイコンがあってもわからんよなとか、そもそも招待制じゃんとか、いろいろ考えた挙げ句、Topページには置かないことにしました。（[About](/about)には置いた）

本来の目的であるテーマも対応していて、dark themeのときだけnordっぽい色合いになるようにしています。（lightは自分が見ないからなにもいじっていない）

あとは昔の記事を読み返しながら、自慢したい記事に `featured: true` をつけて回るなどしました。

astroはなれないので記法がちょっと気持ち悪いんですが、普通に便利っぽいので、もうちょいいじってみようと思います。
