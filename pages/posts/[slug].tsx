import React from "react";
import { Heading } from "@chakra-ui/react";

import { getPostBySlug, getPostMetasOrderByDate, Post } from "../../lib/posts";

type Props = {
  post: Post;
};

const PostPage: React.VFC<Props> = (props: Props) => {
  const { post } = props;
  return (
    <div>
      <Heading>{post.meta.title}</Heading>
      <div dangerouslySetInnerHTML={{ __html: post.content }}></div>
    </div>
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
