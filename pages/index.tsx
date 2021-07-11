import Head from "next/head";
import { useRouter } from "next/router";
import { Container, Flex, Spacer, StackDivider, Text, VStack } from "@chakra-ui/react";

import { getPostsOrderByDate, Post } from "../lib/posts";
import { Link } from "../components/Link";
import { PostItem } from "../components/PostItem";

type Props = {
  posts: ReadonlyArray<Post>;
};

const POSTS_PER_PAGE = 10;

export default function Home(props: Props) {
  const { posts } = props;
  const router = useRouter();
  const page = parseInt((router.query.page as string) || "0");
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
            {slice.map((post) => (
              <PostItem key={post.id} post={post} />
            ))}
          </VStack>
          <Flex>
            {page <= 0 ? null : (
              <Link p="4" href={`/?page=${page - 1}`}>
                <Text fontSize="lg">
                  ← Previous
                </Text>
              </Link>
            )}
            <Spacer />
            {page >= totalPages ? null : (
              <Link p="4" href={`/?page=${page + 1}`}>
                <Text fontSize="lg">
                  Next →
                </Text>
              </Link>
            )}
          </Flex>
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
    },
  };
}
