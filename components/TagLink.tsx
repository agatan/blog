import { Divider, Flex, HStack, Spacer, Tag, TagLabel, Text } from "@chakra-ui/react";
import React from "react";
import { Link } from "./Link";


type Props = {
  tag: string;
  count?: number;
};


export const TagLink: React.VFC<Props> = (props) => {
  const { tag, count } = props;
  return (
    <Link href={`/tags/${tag}`}>
      <Tag
        colorScheme="blue"
        variant="outline"
        padding="1"
      >
        <Flex align="center" paddingLeft="1" paddingRight="1">
          <TagLabel fontSize="small">{tag}</TagLabel>
          {count != null && <Text fontSize="x-small" color="blue.500" paddingLeft="1">{count}</Text>}
        </Flex>
      </Tag>
    </Link >
  );
};
