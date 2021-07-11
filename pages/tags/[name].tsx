import React from "react";
import { useRouter } from "next/router";
import { Container, Divider, Heading, Text } from "@chakra-ui/react";

import { PostList } from "../../components/PostList";
import { getPostMetasOrderByDate, PostMeta } from "../../lib/posts";

type Props = {
  tag: string;
  postMetas: ReadonlyArray<PostMeta>;
};

const TagPage: React.FC<Props> = (props) => {
  const { tag, postMetas } = props;
  const router = useRouter();
  const page = parseInt((router.query.page as string) || "0");
  return (
    <Container maxW="container.xl">
      <Heading>
        <Text>{tag}</Text>に関する記事
      </Heading>
      <Divider />
      <PostList postMetas={postMetas} page={page} />
    </Container>
  );
};
export default TagPage;

export async function getStaticPaths() {
  const postMetas = await getPostMetasOrderByDate();
  const counter: Record<string, number> = {};
  for (const meta of postMetas) {
    for (const tag of meta.tags) {
      counter[tag] = counter[tag] ? counter[tag] + 1 : 1;
    }
  }
  const tags = Object.keys(counter);
  tags.sort((a, b) => counter[b] - counter[a]);
  return {
    paths: tags.map((tag) => `/tags/${tag}`),
    fallback: false,
  };
}

export async function getStaticProps({
  params,
}: {
  params: { name: string };
}): Promise<{ props: Props }> {
  const tag = params.name;
  const postMetas = (await getPostMetasOrderByDate()).filter((v) =>
    v.tags.includes(tag)
  );
  return { props: { tag, postMetas } };
}
