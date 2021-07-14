import fs from "fs";
import RSS from "rss";

import { PostMeta } from "./posts";

function generateRssFeedXml(postMetas: ReadonlyArray<PostMeta>): string {
  const feed = new RSS({
    title: "↗ agatan blog ↗",
    description:
      "agatan のブログです。主にエンジニアリングに関する内容を書きます。",
    site_url: "https://blog.agatan.dev",
    feed_url: "https://blog.agatan.dev/feed.xml",
    language: "ja",
  });

  for (const postMeta of postMetas) {
    feed.item({
      title: postMeta.title,
      description: postMeta.contentMarkdown,
      date: new Date(postMeta.timestamp),
      url: encodeURI(`https://blog.agatan.dev/posts/${encodeURIComponent(postMeta.slug)}`),
    });
  }
  return feed.xml();
}

export const publishRss = async (postMetas: PostMeta[]) => {
  const PATH = "./public/feed.xml";
  const rss = generateRssFeedXml(postMetas);
  fs.writeFileSync(PATH, rss);
};
