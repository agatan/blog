import { Box, Container, Flex } from "@chakra-ui/react";
import React from "react";
import { MainLayout } from "../../components/MainLayout";
import { SEO } from "../../components/SEO";
import { TagLink } from "../../components/TagLink";
import { getTagWithCounts, TagWithCount } from "../../lib/tags";

type Props = {
  tagWithCounts: ReadonlyArray<TagWithCount>,
}

const TagsPage: React.FC<Props> = (props) => {
  return (
    <>
      <SEO title="tags" />
      <MainLayout>
        <Flex wrap="wrap">
          {props.tagWithCounts.map(({ tag, count }) => (
            <Box key={tag} padding="2">
              <TagLink tag={tag} count={count} />
            </Box>
          ))}
        </Flex>
      </MainLayout>
    </>
  )
};
export default TagsPage;

export async function getStaticProps(): Promise<{ props: Props }> {
  const tagWithCounts = await getTagWithCounts();
  return {
    props: { tagWithCounts }
  };
}
