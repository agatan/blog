import Head from "next/head";
import { useRouter } from "next/router";
import {
  Container,
  Flex,
  Spacer,
  StackDivider,
  Text,
  VStack,
} from "@chakra-ui/react";

import { getPostMetasOrderByDate, PostMeta } from "../lib/posts";
import { Link } from "../components/Link";
import { PostItem } from "../components/PostItem";

type Props = {
  postMetas: ReadonlyArray<PostMeta>;
};

const POSTS_PER_PAGE = 10;

export default function Home(props: Props) {
  const { postMetas } = props;
  const router = useRouter();
  const page = parseInt((router.query.page as string) || "0");
  const totalPages = (postMetas.length - 1) / POSTS_PER_PAGE + 1;
  const slice = postMetas.slice(
    POSTS_PER_PAGE * page,
    POSTS_PER_PAGE * (page + 1)
  );
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
            {slice.map((postMeta) => (
              <PostItem key={postMeta.slug} postMeta={postMeta} />
            ))}
          </VStack>
          <Flex>
            {page <= 0 ? null : (
              <Link p="4" href={`/?page=${page - 1}`}>
                <Text fontSize="lg">← Previous</Text>
              </Link>
            )}
            <Spacer />
            {page >= totalPages ? null : (
              <Link p="4" href={`/?page=${page + 1}`}>
                <Text fontSize="lg">Next →</Text>
              </Link>
            )}
          </Flex>
        </Container>
      </main>
    </div>
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
