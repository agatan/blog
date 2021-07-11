import fs from "fs/promises";
import path from "path";
import remark from "remark";
import html from "remark-html";
import gfm from "remark-gfm";
// @ts-ignore
import prism from "remark-prism";
import matter from "gray-matter";

const POSTDIR = "posts";

export type Post = {
  id: string;
  content: string;
  rawMarkdown: string;
  meta: PostMeta;
};

export type PostMeta = {
  title: string;
  timestamp: number;
  tags: ReadonlyArray<string>;
};

function ensurePostMeta(filename: string, meta: unknown): PostMeta {
  if (typeof meta !== "object") {
    throw new Error(
      `frontmatter of ${filename} has invalid type. expected object, but got ${typeof meta}.`
    );
  }
  if (meta == null) {
    throw new Error(`${filename} has no frontmatter.`);
  }
  const object: { title?: unknown; date?: unknown; tags?: unknown } = meta;
  const title = object.title;
  if (title == null) {
    throw new Error(`${filename} does not have the title.`);
  }
  if (typeof title !== "string") {
    throw new Error(`${filename} is not a string.`);
  }
  const date = object.date;
  if (date == null) {
    throw new Error(
      `frontmatter of ${filename} does not contain "date" field.`
    );
  }
  if (!(date instanceof Date)) {
    throw new Error(
      `date of ${filename} has invalid type. expected Date, but got ${date}`
    );
  }
  const tags = object.tags;
  if (tags != null) {
    if (!Array.isArray(tags)) {
      throw new Error(`tags of ${filename} is not an array.`);
    }
    for (const tag of tags) {
      if (typeof tag !== "string") {
        throw new Error(`tags of ${filename} has non-string value.`);
      }
    }
  }
  return {
    title: title,
    timestamp: date.getTime(),
    tags: tags as ReadonlyArray<string>,
  };
}

function extractIdFromPath(filename: string): string {
  console.assert(filename.endsWith);
  return path.basename(filename, ".md");
}

export function getPostById(id: string): Promise<Post> {
  return getPost(id + ".md");
}

export async function getPost(filename: string): Promise<Post> {
  const filepath = path.join(POSTDIR, filename);
  const rawMarkdown = await fs.readFile(filepath, { encoding: "utf-8" });
  const matterResult = matter(rawMarkdown);
  const meta = ensurePostMeta(filename, matterResult.data);
  const content = await remark()
    .use(gfm)
    .use(prism)
    .use(html)
    .process(matterResult.content);
  return {
    id: extractIdFromPath(filename),
    content: content.toString(),
    rawMarkdown,
    meta,
  };
}

export async function getPostsOrderByDate(): Promise<Post[]> {
  const postFiles = await fs.readdir(POSTDIR);
  const posts = await Promise.all(postFiles.map(getPost));
  posts.sort((a, b) => b.meta.timestamp - a.meta.timestamp);
  return posts;
}
