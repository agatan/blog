import fs from 'fs';
import path from 'path';

const postsDir = "posts";

export function getAllPostIds(): ReadonlyArray<{params: {id: string}}> {
  const postDirNames = fs.readdirSync(postsDir);
  return postDirNames.map(postDirName => {
    return {
      params: {
        id: postDirName,
      }
    }
  });
}

export type PostData = {
  id: string,
  markdown: string,
}

export function getPostData(id: string): PostData {
  const fullPath = path.join(postsDir, id, "index.md");
  const fileContents = fs.readFileSync(fullPath, "utf-8");
  return {
    id,
    markdown: fileContents,
  }
}
