import React from "react";
import { useRouter } from "next/router";
import { Container, Divider, Heading, Text } from "@chakra-ui/react";

import { PostList } from "../../components/PostList";
import { getPostMetasOrderByDate, PostMeta } from "../../lib/posts";
import { getTagWithCounts } from "../../lib/tags";

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
      <Container maxW="container.lg">
        <Heading as="h1">
          <Text as="span">#{tag}</Text>
        </Heading>
      </Container>
      <Divider />
      <PostList postMetas={postMetas} page={page} />
    </Container>
  );
};
export default TagPage;

export async function getStaticPaths() {
  const tags = await getTagWithCounts();
  return {
    paths: tags.map((tag) => `/tags/${tag.tag}`),
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
