import React from "react";
import { Container, Heading, HStack, Tag } from "@chakra-ui/react";

import { Link } from "./Link";
import { Post } from "../lib/posts";


type Props = {
  post: Post;
}

export const PostItem: React.VFC<Props> = (props) => {
  const { post } = props;
  return (
    <Container maxW="container.lg">
      <Heading>
        <Link href={`/posts/${post.id}`}>
          {post.meta.title}
        </Link>
      </Heading>
      <HStack>
        {post.meta.tags.map((tag) => <Tag key={`${post.id}-${tag}`} colorScheme="blue" variant="outline">{tag}</Tag>)}
      </HStack>
    </Container>
  );
};
