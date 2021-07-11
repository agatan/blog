import React from "react";
import { Container, VStack, StackDivider, Flex, Text, Spacer } from "@chakra-ui/react";

import { PostItem } from "./PostItem";
import { Link } from "./Link";
import { PostMeta } from "../lib/posts";

type Props = {
  postMetas: ReadonlyArray<PostMeta>;
  page?: number;
}

const POSTS_PER_PAGE = 10;

export const PostList: React.FC<Props> = (props) => {
  const { postMetas, page } = { page: 0, ...props };
  const totalPages = (postMetas.length - 1) / POSTS_PER_PAGE + 1;
  const slice = postMetas.slice(
    POSTS_PER_PAGE * page,
    POSTS_PER_PAGE * (page + 1)
  );
  return (
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
  );
}
