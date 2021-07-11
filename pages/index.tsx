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
import { PostList } from "../components/PostList";

type Props = {
  postMetas: ReadonlyArray<PostMeta>;
};

export default function Home(props: Props) {
  const { postMetas } = props;
  const router = useRouter();
  const page = parseInt((router.query.page as string) || "0");
  return (
    <div>
      <Head>
        <title>Create Next App</title>
        <link rel="icon" href="/favicon.ico" />
      </Head>

      <main>
        <PostList postMetas={postMetas} page={page} />
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
