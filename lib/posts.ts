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
  content: string;
  meta: PostMeta;
};

export type PostMeta = {
  slug: string;
  title: string;
  timestamp: number;
  tags: ReadonlyArray<string>;
  rawMarkdown: string;
  contentMarkdown: string;
};

function ensurePostMeta(
  filename: string,
  meta: unknown
): {
  slug: string;
  title: string;
  timestamp: number;
  tags: ReadonlyArray<string>;
} {
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
    slug: extractIdFromPath(filename),
    title: title,
    timestamp: date.getTime(),
    tags: tags as ReadonlyArray<string>,
  };
}

function extractIdFromPath(filename: string): string {
  console.assert(filename.endsWith);
  return path.basename(filename, ".md");
}

export async function getPostMeta(filename: string): Promise<PostMeta> {
  const filepath = path.join(POSTDIR, filename);
  const rawMarkdown = await fs.readFile(filepath, { encoding: "utf-8" });
  const matterResult = matter(rawMarkdown);
  const meta = ensurePostMeta(filename, matterResult.data);
  return {
    rawMarkdown,
    contentMarkdown: matterResult.content,
    ...meta,
  };
}

export async function getPostBySlug(slug: string): Promise<Post> {
  const meta = await getPostMeta(slug + ".md");
  const content = await remark()
    .use(gfm)
    .use(prism)
    .use(html)
    .process(meta.contentMarkdown);
  return {
    content: content.toString(),
    meta,
  };
}

export async function getPostMetasOrderByDate(): Promise<PostMeta[]> {
  const postFiles = await fs.readdir(POSTDIR);
  const postMetas = await Promise.all(postFiles.map(getPostMeta));
  postMetas.sort((a, b) => b.timestamp - a.timestamp);
  return postMetas;
}
