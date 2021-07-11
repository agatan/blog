import Head from "next/head";
import { Container, StackDivider, VStack } from "@chakra-ui/react";

import { PostItem } from "../components/PostItem";
import { getPostsOrderByDate, Post } from "../lib/posts";

type Props = {
  posts: ReadonlyArray<Post>;
  page?: number;
};

const POSTS_PER_PAGE = 10;

export default function Home(props: Props) {
  const { page, posts } = { page: 0, ...props };
  const totalPages = (posts.length - 1) / POSTS_PER_PAGE + 1;
  const slice = posts.slice(POSTS_PER_PAGE * page, POSTS_PER_PAGE * (page + 1));
  return (
    <div>
      <Head>
        <title>Create Next App</title>
        <link rel="icon" href="/favicon.ico" />
      </Head>

      <main>
        <Container maxW="container.xl">
          <VStack
            divider={<StackDivider borderColor="gray.200" />}
            spacing={4}
            align="stretch"
          >
            {slice.map((post) => <PostItem key={post.id} post={post} />)}
          </VStack>
        </Container>
      </main>
    </div>
  );
}


export async function getStaticProps(): Promise<{ props: Props }> {
  const posts = await getPostsOrderByDate();
  return {
    props: {
      posts,
      page: 0,
    }
  }
}
