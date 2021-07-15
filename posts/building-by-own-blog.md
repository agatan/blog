---
title: 個人ブログをつくりました
date: 2021-07-16
tags: ["雑談"]
---

むかしからやってみたいと思いつつ、なかなか腰が重くて行動にうつせていなかった個人ブログ作成をついにやってみました！

Web フロントエンドにほとんどふれてこないまま今日まできてしまったので、どこかしら個人ででもフロントエンドっぽいことを多少なりやりたい・題材としてブログとかやってみたいなと常々おもっていました。
ただ根本的な問題としてブログを継続的に書けるタイプでもないので、作ってしまったはいいもののすでに持て余す未来がみえていますが、逆に場所があることで書くモチベーションがうまれるみたいなのも期待しています。

なんで急にやるきになったのかというと、完全にたまたまで、[Deno](https://deno.land/) を触ってみたい → [Aleph.js](https://alephjs.org/) なる [Next.js](https://nextjs.org/) inspired な SSG/SSR フレームワークがあるらしい → 触ってみるか → はまる → Next.js ではどうなってるんだろう？ → 気付いたら Next.js でブログつくってた、という経緯です。
Deno + Aleph.js は(まだまだ発展途上とはいえ)なかなか体験もよく、これはこれでもうちょっと触って記事にしたい & 結局 Next.js にした理由はいくつかあるのでなにかしら貢献できたら楽しそうだなと思っていたりします。

## 技術

Markdown でかいた記事を、 Next.js で SSG して [Cloudflare Pages](https://pages.cloudflare.com/) でホスティングしています。
スタイルは自分でかくのは苦手意識がすごかったので、 [Chakra UI](https://chakra-ui.com/) を採用しました。

最低限のスタイルは整えたつもりで、 Syntax Hightlight もきくし、スマートフォンのような端末でみてもそれなりに見れる感じにはなっているのではないかと思います。

```typescript
type Props = {
  tag: string;
};

export const TagLink: React.VFC<Props> = (props) => {
  const { tag } = props;
  return (
    <Link href={`/tags/${tag}`} padding="0">
      <Tag colorScheme="blue" variant="outline" padding="1" size="sm">
        <TagLabel>{tag}</TagLabel>
      </Tag>
    </Link>
  );
};
```

[remark](https://github.com/remarkjs/remark) で Markdown → HTML -> rehype -> React Component をやっています。一回 HTML を介する意味がない気がするのですが、試行錯誤のなりゆきでこうなってしまって特にまだ害もないのでこのままにしています。
Highlight は [Prism](https://prismjs.com/) です。

React Component にしているのは、コンテンツ部分も Chakra UI や `next/link` をつかうようにするためです。
Chakra UI は書き心地やメンダルモデルが Flutter とかにちかい感じでかけるのでだいぶ書きやすかったです。(僕が HTML & CSS 素人すぎるだけかもしれないです。なれているひとはこのテンションで HTML/CSS を書いてるんですかね)

RSS もむりやり対応しました。 https://blog.agatan.dev/feed.xml
`/pages/index.tsx` の `getStaticProps` で `/public/feed.xml` にファイルを書き出すという技をきめているのですが、ただしいやりかたなんでしょうか...

せっかくなので Google Analytics も導入してみました。
Next.js はそういう [サンプル](https://github.com/vercel/next.js/tree/master/examples/with-google-analytics) まで用意されていてすごいですね。

og も一応最低限は設定しました。

このブログは記事もソースコードも https://github.com/agatan/blog で管理しています。
なので各記事の最下部に GitHub 上の該当ファイルへのリンクを置いてみました。
なにか間違いを見つけたら PR ください。

ホスティングは最初は GitHub Pages にしようとおもったのですが、Netlify も Vercel も Cloudflare Pages もさわったことがなかったので、なにかしら新しいものに触れたくなり、ユースケース的にもぴったりだった Cloudflare Pages にのせることにしました。
デプロイまわりの設定がぽちぽちやるだけで一瞬で完了し、カスタムドメインをあてるのも異常に簡単で驚きました。

## 移行

記事がないとスタイルもよくわからないし、過去に書いてきたものもいくらかは集約させるかーと思い、Qiita/はてなブログ/Medium から内容をもってきました。
Qiita/はてなブログからの移行は Deno でちょちょっとスクリプトを書いておこないました。こういうのには便利ですね。

- [Qiita から記事を export する Deno スクリプト](https://gist.github.com/agatan/ce03732e03d3cae3a7e70b2ac62a0164)
- [はてなブログから export した MT ファイルを Markdown に変換する Deno スクリプト](https://gist.github.com/agatan/dd306e967c953739f41718ea44638c16)

はてなの方は HTML だったので結局このあと Python で HTML → Markdown を(多少変換を加えつつ)おこなうスクリプトをかきました。
Deno でやろうとおもったのですが、HTML → Markdown のよさそうなライブラリが見当らず、自分でかくほど労力をかけたくもなかったので。

いまのところ人生でいちばん書いているのは Python らしく、ぶっちゃけこういう書き捨てスクリプトは Python で書くのがいちばん速いです。
(Python の標準ライブラリが便利なのもわかっているし、良い言語だとはおもうのですが、個人的な好みとしては静的型検査がある言語がすきなので、書き捨て用の良い言語を探していたりします。Deno をさわりまくっているのもその一環です。このへんも雑談として記事にしたい。)

というわけでブログを作ったという話でした。
つづくかはわかりませんが、転職して(終盤はコロナで前職もリモートでしたが)リモートベースになったのもあって技術的なトピックについてグダグダと話す機会がへっているので、知見未満の雑談っぽい内容をかいてみるのもよいかなーとおもっています。
