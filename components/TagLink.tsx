import { Flex, Tag, TagLabel, Text } from "@chakra-ui/react";
import React from "react";
import { Link } from "./Link";

type Props = {
  tag: string;
  count?: number;
};

export const TagLink: React.VFC<Props> = (props) => {
  const { tag, count } = props;
  return (
    <Link href={`/tags/${tag}`} padding="0">
      <Tag colorScheme="blue" variant="outline" padding="1" size="sm">
        <Flex align="center" paddingLeft="1" paddingRight="1">
          <TagLabel fontSize={{ base: "x-small", lg: "small"}}>{tag}</TagLabel>
          {count != null && (
            <Text fontSize="xs" color="blue.500" paddingLeft="1">
              {count}
            </Text>
          )}
        </Flex>
      </Tag>
    </Link>
  );
};
