import React from "react";
import {
  Box,
  Container,
  Heading,
  Flex,
  Image,
  Text,
  Icon,
  HStack,
  Spacer,
  Stack,
  VStack,
  Center,
  Divider,
  Wrap,
} from "@chakra-ui/react";
import { FaGithub, FaTwitter, FaTags, FaRss } from "react-icons/fa";

import { Link } from "./Link";

type Props = {
  children: React.ReactNode;
};

const GoogleAnalyticsTerm = () => (
  <Wrap paddingX="4" paddingTop="8" align="center" justify="center">
    <Text fontSize={{ base: "8", lg: "sm" }}>
      このサイトはGoogle Analyticsを使用しています。
    </Text>
    <Text fontSize={{ base: "8", lg: "sm" }}>
      <Link href="https://policies.google.com/technologies/partner-sites?hl=ja">
        詳しく見る
      </Link>
    </Text>
  </Wrap>
);

export const MainLayout: React.VFC<Props> = (props) => {
  return (
    <Center backgroundColor="green.50" minHeight="100vh" minWidth="100vw">
      <VStack
        minHeight="100vh"
        width="100%"
        maxWidth="container.xl"
        spacing="0"
        backgroundColor="white"
        shadow="md"
      >
        <Flex
          align="flex-end"
          padding={{ base: "2", lg: "8" }}
          backgroundColor="green.600"
          justify="space-between"
          width="100%"
        >
          <Heading>
            <Link href="/" color="white">
              <Text as="u" fontSize={{ base: "large", lg: "3xl" }}>
                ↗ agatan blog ↗
              </Text>
            </Link>
          </Heading>
          <Box>
            <Link href="/feed.xml" paddingX="2">
              <Icon as={FaRss} color="white" boxSize={{ base: "4", lg: "6" }} />
            </Link>
            <Link href="/tags" paddingX="2">
              <Icon
                as={FaTags}
                color="white"
                boxSize={{ base: "4", lg: "6" }}
              />
            </Link>
          </Box>
        </Flex>
        <Flex paddingTop="4" paddingBottom="4" width="100%" wrap="wrap">
          <Box width={{ base: "100%", lg: "85%" }}>
            {props.children}
            <Box display={{ base: "none", lg: "block" }}>
              <GoogleAnalyticsTerm />
            </Box>
          </Box>
          <Container width="auto" paddingLeft={{ base: "4", lg: "0" }}>
            <Flex
              direction="column"
              justify="center"
              align="center"
              padding="4"
              paddingLeft={{ base: "4", lg: "0" }}
            >
              <Link href="https://twitter.com/@agatan_">
                <Center>
                  <Image
                    src="/i/agatan.png"
                    layout="fill"
                    objectFit="cover"
                    alt="agatan"
                    width={{ base: "10", lg: "20" }}
                    height={{ base: "10", lg: "20" }}
                  />
                </Center>
                <Text fontSize={{ base: "small", lg: "large" }} align="center">
                  @agatan
                </Text>
              </Link>
              <HStack>
                <Link href="https://twitter.com/@agatan_" padding="1">
                  <Icon
                    as={FaTwitter}
                    focusable
                    boxSize={{ base: "0.8em", lg: "1.2em" }}
                  />
                </Link>
                <Link href="https://github.com/agatan" padding="1">
                  <Icon
                    as={FaGithub}
                    focusable
                    boxSize={{ base: "0.8em", lg: "1.2em" }}
                  />
                </Link>
              </HStack>
            </Flex>
          </Container>
          <Box display={{ base: "block", lg: "none" }}>
            <GoogleAnalyticsTerm />
          </Box>
          <Spacer display={{ base: "none", lg: "block" }} />
        </Flex>
      </VStack>
    </Center>
  );
};
