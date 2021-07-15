import React from "react";
import {
  Box,
  chakra,
  Code,
  Container,
  Divider,
  Flex,
  Heading,
  HStack,
  Icon,
  Image,
  Link as ChakraLink,
  ListItem,
  OrderedList,
  Text,
  UnorderedList,
} from "@chakra-ui/react";
import unified from "unified";
import rehypeParse from "rehype-parse";
import rehypeReact from "rehype-react";
import rehypeSlug from "rehype-slug";
import { FaLink, FaGithub } from "react-icons/fa";

import { getPostBySlug, getPostMetasOrderByDate, Post } from "../../lib/posts";
import { SEO } from "../../components/SEO";
import { MainLayout } from "../../components/MainLayout";
import { Link } from "../../components/Link";
import { TagLink } from "../../components/TagLink";

type Props = {
  post: Post;
};

const ContentH1 = (props: any) => (
  <Heading
    fontSize={{ base: "large", lg: "x-large" }}
    paddingTop="2"
    paddingBottom="2"
    {...props}
  >
    <Flex align="center">
      <ChakraLink href={`#${props.id}`} padding="2">
        <Icon as={FaLink} color="gray.500" boxSize="4" />
      </ChakraLink>
      {props.children}
    </Flex>
    <Divider />
  </Heading>
);

const ContentH2 = (props: any) => (
  <Heading
    fontSize={{ base: "large", lg: "x-large" }}
    paddingTop="2"
    paddingBottom="2"
    {...props}
  >
    <Flex align="center">
      <ChakraLink href={`#${props.id}`} padding="2">
        <Icon as={FaLink} color="gray.500" boxSize="4" />
      </ChakraLink>
      {props.children}
    </Flex>
    <Divider />
  </Heading>
);

const ContentH3 = (props: any) => (
  <Heading
    fontSize={{ base: "medium", lg: "large" }}
    paddingTop="2"
    paddingBottom="2"
    {...props}
  >
    <Flex align="center">
      <ChakraLink href={`#${props.id}`} padding="2">
        <Icon as={FaLink} color="gray.500" boxSize="4" />
      </ChakraLink>
      {props.children}
    </Flex>
  </Heading>
);

const ContentH4 = (props: any) => (
  <Heading
    fontSize={{ base: "medium", lg: "large" }}
    paddingTop="2"
    paddingBottom="2"
    {...props}
  >
    <Flex align="center">{props.children}</Flex>
  </Heading>
);

const ContentUl = (props: any) => (
  <UnorderedList paddingLeft="4">
    {props.children as React.ReactNode}
  </UnorderedList>
);

const ContentOl = (props: any) => (
  <OrderedList paddingLeft="4">{props.children as React.ReactNode}</OrderedList>
);

const ContentLi = (props: any) => (
  <ListItem>{props.children as React.ReactNode}</ListItem>
);

const ContentParagraph = (props: any) => (
  <Text
    as="p"
    fontSize={{ base: "sm", lg: "medium" }}
    lineHeight={{ base: "2", lg: "8" }}
    padding="1"
  >
    {props.children as React.ReactNode}
  </Text>
);

const ContentAnchor = (props: any) => (
  <Link href={props.href as string}>
    <Text as="span" color="green.600">
      {props.children as React.ReactNode}
    </Text>
  </Link>
);

const ContentImage = (props: any) => <Image src={props.src} alt={props.alt} />;

const ContentCode = (props: any) => (
  <Code fontSize={{ base: "xs", lg: "sm" }} {...props} />
);

const ContentDiv = (props: any) => {
  if (props.className === "remark-highlight") {
    return (
      <chakra.div fontSize={{ base: "xs", lg: "sm" }} paddingX="2" {...props} />
    );
  }
  return <chakra.div {...props} />;
};

const PostPage: React.VFC<Props> = (props: Props) => {
  const { post } = props;
  const content = unified()
    .use(rehypeParse, { fragment: true })
    .use(rehypeSlug)
    .use(rehypeReact, {
      createElement: React.createElement,
      components: {
        h2: ContentH2,
        h3: ContentH3,
        h4: ContentH4,
        ul: ContentUl,
        ol: ContentOl,
        li: ContentLi,
        p: ContentParagraph,
        a: ContentAnchor,
        img: ContentImage,
        code: ContentCode,
        div: ContentDiv,
      },
    })
    .processSync(post.content);
  return (
    <>
      <SEO title={post.meta.title} />
      <MainLayout>
        <Container maxWidth="container.md">
          <Heading fontSize={{ base: "16", lg: "24" }}>
            {post.meta.title}
          </Heading>
          <Text
            color="gray.500"
            fontSize={{ base: "10", lg: "14" }}
            paddingY="1"
          >
            {new Date(post.meta.timestamp).toLocaleDateString()} 公開
          </Text>
          <HStack spacing="1" wrap="wrap">
            {post.meta.tags.map((tag) => (
              <TagLink key={tag} tag={tag} />
            ))}
          </HStack>
          <Divider padding="2" />
          <Box paddingTop="4" fontSize={{ base: "xs", lg: "md" }}>
            {content.result as React.ReactNode}
          </Box>
          <Divider padding="2" />
          <Flex justify="flex-end" align="center" paddingTop="2">
            <Link
              href={`https://github.com/agatan/blog/blob/main/posts/${post.meta.slug}.md`}
              color="green.500"
            >
              <Icon as={FaGithub} boxSize="6" />
              <Text as="span" padding="2">
                GitHub で編集リクエスト
              </Text>
            </Link>
          </Flex>
        </Container>
      </MainLayout>
    </>
  );
};
export default PostPage;

export async function getStaticPaths() {
  const postMetas = await getPostMetasOrderByDate();
  const paths = postMetas.map((v) => `/posts/${encodeURIComponent(v.slug)}`);
  return {
    paths,
    fallback: false,
  };
}

export async function getStaticProps({
  params,
}: {
  params: { slug: string };
}): Promise<{ props: Props }> {
  const post = await getPostBySlug(params.slug);
  return {
    props: {
      post,
    },
  };
}
