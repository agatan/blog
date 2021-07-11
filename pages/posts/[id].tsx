import React from "react";
import { Heading } from "@chakra-ui/react";

import { getPostById, getPostsOrderByDate, Post } from "../../lib/posts";

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
  const posts = await getPostsOrderByDate();
  const paths = posts.map((v) => `/posts/${v.id}`);
  return {
    paths,
    fallback: false,
  };
}

export async function getStaticProps({
  params,
}: {
  params: { id: string };
}): Promise<{ props: Props }> {
  const post = await getPostById(params.id);
  return {
    props: {
      post,
    },
  };
}
