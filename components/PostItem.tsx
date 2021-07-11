import React from "react";
import { Container, Heading, HStack, Tag } from "@chakra-ui/react";

import { Link } from "./Link";
import { PostMeta } from "../lib/posts";

type Props = {
  postMeta: PostMeta;
};

export const PostItem: React.VFC<Props> = (props) => {
  const { postMeta } = props;
  return (
    <Container maxW="container.lg">
      <Heading size="lg">
        <Link href={`/posts/${postMeta.slug}`}>{postMeta.title}</Link>
      </Heading>
      <HStack marginTop="4" marginBottom="4">
        {postMeta.tags.map((tag) => (
          <Tag
            key={`${postMeta.slug}-${tag}`}
            colorScheme="blue"
            variant="outline"
          >
            <Link href={`/tags/${tag}`}>{tag}</Link>
          </Tag>
        ))}
      </HStack>
    </Container>
  );
};
