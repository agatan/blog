import { useRouter } from "next/router";

import { getPostMetasOrderByDate, PostMeta } from "../lib/posts";
import { PostList } from "../components/PostList";
import { SEO } from "../components/SEO";
import { MainLayout } from "../components/MainLayout";
import { publishRss } from "../lib/rss";
import { Container } from "@chakra-ui/react";

type Props = {
  postMetas: ReadonlyArray<PostMeta>;
};

export default function Home(props: Props) {
  const { postMetas } = props;
  return (
    <>
      <SEO
        title="Home"
        description="agatan のブログです。主にエンジニアリングに関する内容を書きます。"
      />
      <MainLayout>
        <Container maxWidth="container.md">
          <PostList postMetas={postMetas} />
        </Container>
      </MainLayout>
    </>
  );
}

export async function getStaticProps(): Promise<{ props: Props }> {
  const postMetas = await getPostMetasOrderByDate();
  await publishRss(postMetas);
  return {
    props: {
      postMetas,
    },
  };
}
