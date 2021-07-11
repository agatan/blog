import React from "react";
import { Container, Heading, HStack, Tag, Link } from "@chakra-ui/react";
import NextLink from "next/link";

import { Post } from "../lib/posts";


type Props = {
  post: Post;
}

export const PostItem: React.VFC<Props> = (props) => {
  const { post } = props;
  return (
    <Container maxW="container.lg">
      <Heading>
        <NextLink href={`/posts/${post.id}`}>
          <Link>
            {post.meta.title}
          </Link>
        </NextLink>
      </Heading>
      <HStack>
        {post.meta.tags.map((tag) => <Tag key={`${post.id}-${tag}`} colorScheme="blue" variant="outline">{tag}</Tag>)}
      </HStack>
    </Container>
  );
};
