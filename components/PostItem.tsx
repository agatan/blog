import React from "react";
import { Box, Heading, HStack, Text } from "@chakra-ui/react";

import { Link } from "./Link";
import { PostMeta } from "../lib/posts";
import { TagLink } from "./TagLink";

type Props = {
  postMeta: PostMeta;
};

export const PostItem: React.VFC<Props> = (props) => {
  const { postMeta } = props;
  return (
    <Box width="100%" padding="2">
      <Heading fontSize={{ base: "14", lg: "20" }}>
        <Link href={`/posts/${postMeta.slug}`}>{postMeta.title}</Link>
      </Heading>
      <Text color="gray.500" fontSize={{ base: "10", lg: "14"}} paddingY="1">
        {new Date(postMeta.timestamp).toLocaleDateString()} 公開
      </Text>
      <HStack spacing="1" wrap="wrap">
        {postMeta.tags.map((tag) => (
          <TagLink key={`${postMeta.slug}-${tag}`} tag={tag} />
        ))}
      </HStack>
    </Box>
  );
};
