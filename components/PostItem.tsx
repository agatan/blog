import React from "react";
import { Container, Heading, HStack, Text } from "@chakra-ui/react";

import { Link } from "./Link";
import { PostMeta } from "../lib/posts";
import { TagLink } from "./TagLink";

type Props = {
  postMeta: PostMeta;
};

export const PostItem: React.VFC<Props> = (props) => {
  const { postMeta } = props;
  return (
    <Container>
      <Heading size="md">
        <Link href={`/posts/${postMeta.slug}`}>{postMeta.title}</Link>
      </Heading>
      <Text color="gray.800">
        {new Date(postMeta.timestamp).toDateString()}
      </Text>
      <HStack paddingTop="4" paddingBottom="4" spacing="0" wrap="wrap">
        {postMeta.tags.map((tag) => (
          <TagLink key={`${postMeta.slug}-${tag}`} tag={tag} />
        ))}
      </HStack>
    </Container>
  );
};
