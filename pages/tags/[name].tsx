import React from "react";
import { useRouter } from "next/router";
import { Container, Divider, Heading, Text } from "@chakra-ui/react";

import { PostList } from "../../components/PostList";
import { getPostMetasOrderByDate, PostMeta } from "../../lib/posts";
import { getTagWithCounts } from "../../lib/tags";
import { SEO } from "../../components/SEO";
import { MainLayout } from "../../components/MainLayout";

type Props = {
  tag: string;
  postMetas: ReadonlyArray<PostMeta>;
};

const TagPage: React.FC<Props> = (props) => {
  const { tag, postMetas } = props;
  return (
    <>
      <SEO title={`#${tag}`} description={`#${tag}に関する投稿`} />
      <MainLayout>
        <Container maxW="container.md">
          <Heading paddingBottom="4" fontSize={{base: "large", lg: "3xl"}} color="blue.500">
            #{tag}
          </Heading>
          <Divider />
          <PostList postMetas={postMetas} />
        </Container>
      </MainLayout>
    </>
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
