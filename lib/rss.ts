// Ref: https://blog.kasorin.work/posts/rss_for_nextjs_ssg
import fs from "fs";
import { PostMeta } from "./posts";

const generateRssItem = (postMeta: PostMeta): string => {
  return `
        <item>
            <guid>https://agatan.github.io/posts/${encodeURIComponent(postMeta.slug)}</guid>
            <title>${postMeta.title}</title>
            <link>https://agatan.github.io/posts/${encodeURIComponent(postMeta.slug)}</link>
            <pubDate>${new Date(postMeta.timestamp).toUTCString()}</pubDate>
        </item>
    `;
};

const generateRss = (postMetas: PostMeta[]): string => {
  return `
        <rss version="2.0" xmlns:atom="http://www.w3.org/2005/Atom">
            <channel>
                <title>↗ agatan blog ↗</title>
                <link>https://agatan.github.io</link>
                <description>agatan のブログです。主にエンジニアリングに関する内容を書きます。</description>
                <atom:link href="https://agatan.github.io/feed.xml" rel="self" type="application/rss+xml"/>
                ${postMetas.map(generateRssItem).join("")}
            </channel>
        </rss>
    `;
};

export const publishRss = async (postMetas: PostMeta[]) => {
  const PATH = "./public/feed.xml";
  const rss = generateRss(postMetas);
  fs.writeFileSync(PATH, rss);
};
