import { Box, Container, Flex, Heading, Wrap } from "@chakra-ui/react";
import React from "react";
import { MainLayout } from "../../components/MainLayout";
import { SEO } from "../../components/SEO";
import { TagLink } from "../../components/TagLink";
import { getTagWithCounts, TagWithCount } from "../../lib/tags";

type Props = {
  tagWithCounts: ReadonlyArray<TagWithCount>;
};

const TagsPage: React.FC<Props> = (props) => {
  return (
    <>
      <SEO title="tags" />
      <MainLayout>
        <Container maxWidth="container.md">
          <Heading paddingBottom="4" fontSize={{ base: "xl", lg: "3xl" }}>
            #tags
          </Heading>
          <Wrap spacing="1">
            {props.tagWithCounts.map(({ tag, count }) => (
              <TagLink key={tag} tag={tag} count={count} />
            ))}
          </Wrap>
        </Container>
      </MainLayout>
    </>
  );
};
export default TagsPage;

export async function getStaticProps(): Promise<{ props: Props }> {
  const tagWithCounts = await getTagWithCounts();
  return {
    props: { tagWithCounts },
  };
}
