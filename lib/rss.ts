import fs from "fs";
import RSS from "rss";

import { PostMeta } from "./posts";

function generateRssFeedXml(postMetas: ReadonlyArray<PostMeta>): string {
  const feed = new RSS({
    title: "↗ agatan blog ↗",
    description:
      "agatan のブログです。主にエンジニアリングに関する内容を書きます。",
    site_url: "https://agatan.github.io",
    feed_url: "https://agatan.github.io/feed.xml",
    language: "ja",
  });

  for (const postMeta of postMetas) {
    feed.item({
      title: postMeta.title,
      description: postMeta.contentMarkdown.slice(0, 300) + "...",
      date: new Date(postMeta.timestamp),
      url: encodeURI(`https://agatan.github.io/posts/${postMeta.slug}`),
    });
  }
  return feed.xml();
}

export const publishRss = async (postMetas: PostMeta[]) => {
  const PATH = "./public/feed.xml";
  const rss = generateRssFeedXml(postMetas);
  fs.writeFileSync(PATH, rss);
};
