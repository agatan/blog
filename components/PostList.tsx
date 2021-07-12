import React from "react";
import {
  Container,
  VStack,
  StackDivider,
  Flex,
  Text,
  Spacer,
} from "@chakra-ui/react";

import { PostItem } from "./PostItem";
import { Link } from "./Link";
import { PostMeta } from "../lib/posts";

type Props = {
  postMetas: ReadonlyArray<PostMeta>;
};

const POSTS_PER_PAGE = 10;

export const PostList: React.FC<Props> = (props) => {
  const { postMetas } = props;
  return (
    <Container maxW="container.md">
      <VStack
        divider={<StackDivider />}
        spacing={4}
        paddingTop="4"
        paddingBottom="4"
        align="stretch"
      >
        {postMetas.map((postMeta) => (
          <PostItem key={postMeta.slug} postMeta={postMeta} />
        ))}
      </VStack>
    </Container>
  );
};
