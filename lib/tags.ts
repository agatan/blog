import { getPostMetasOrderByDate } from "./posts";

export type TagWithCount = {
  tag: string;
  count: number;
};

export async function getTagWithCounts(): Promise<TagWithCount[]> {
  const postMetas = await getPostMetasOrderByDate();
  const counter: Record<string, number> = {};
  for (const meta of postMetas) {
    for (const tag of meta.tags) {
      counter[tag] = counter[tag] ? counter[tag] + 1 : 1;
    }
  }
  const tags = Object.keys(counter);
  tags.sort((a, b) => counter[b] - counter[a]);
  return tags.map((tag) => {
    return { tag, count: counter[tag] };
  });
}
