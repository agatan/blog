import { getCollection } from "astro:content";
import generateOgImage from "@utils/generateOgImage";
import type { APIRoute } from "astro";

export const GET: APIRoute = async ({ params }) =>
  new Response(
    JSON.stringify({
      body: await generateOgImage(params.ogTitle),
    }),
    {
      status: 200,
      headers: {
        "Content-Type": "application/json",
      },
    }
  );

const postImportResult = await getCollection("blog", ({ data }) => !data.draft);
const posts = Object.values(postImportResult);

export function getStaticPaths() {
  return posts
    .filter(({ data }) => !data.ogImage)
    .map(({ data }) => ({
      params: { ogTitle: data.title },
    }));
}
