import React from "react";
import { Container, Heading } from "@chakra-ui/react";

import { getPostBySlug, getPostMetasOrderByDate, Post } from "../../lib/posts";
import { SEO } from "../../components/SEO";

type Props = {
  post: Post;
};

const PostPage: React.VFC<Props> = (props: Props) => {
  const { post } = props;
  return (
    <Container maxWidth="container.md">
      <SEO title={post.meta.title} />
      <Heading>{post.meta.title}</Heading>
      <div dangerouslySetInnerHTML={{ __html: post.content }}></div>
    </Container>
  );
};
export default PostPage;

export async function getStaticPaths() {
  const postMetas = await getPostMetasOrderByDate();
  const paths = postMetas.map((v) => `/posts/${v.slug}`);
  return {
    paths,
    fallback: false,
  };
}

export async function getStaticProps({
  params,
}: {
  params: { slug: string };
}): Promise<{ props: Props }> {
  const post = await getPostBySlug(params.slug);
  return {
    props: {
      post,
    },
  };
}
