import Head from "next/head";
import { useRouter } from "next/router";

import { getPostMetasOrderByDate, PostMeta } from "../lib/posts";
import { PostList } from "../components/PostList";
import { Container } from "@chakra-ui/react";
import { SEO } from "../components/SEO";

type Props = {
  postMetas: ReadonlyArray<PostMeta>;
};

export default function Home(props: Props) {
  const { postMetas } = props;
  const router = useRouter();
  const page = parseInt((router.query.page as string) || "0");
  return (
    <Container maxWidth="container.lg">
      <SEO title="Home" description="agatan のブログです。主にエンジニアリングに関する内容を書きます。"/>
      <PostList postMetas={postMetas} page={page} />
    </Container>
  );
}

export async function getStaticProps(): Promise<{ props: Props }> {
  const postMetas = await getPostMetasOrderByDate();
  return {
    props: {
      postMetas,
    },
  };
}
