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
} from "@chakra-ui/react";
import { FaGithub, FaTwitter, FaTags, FaRss } from "react-icons/fa";

import { Link } from "./Link";

type Props = {
  children: React.ReactNode;
};

export const MainLayout: React.VFC<Props> = (props) => {
  return (
    <Box backgroundColor="green.50" minHeight="100vh">
      <Container maxWidth="container.lg" backgroundColor="green.600">
        <Flex align="flex-end" padding="8">
          <Heading>
            <Link href="/" color="white">
              <Text as="u">↗ agatan blog ↗</Text>
            </Link>
          </Heading>
          <Spacer />
          <Box>
            <Link href="/feed" padding="2">
              <Icon as={FaRss} color="white" boxSize="6" />
            </Link>
            <Link href="/tags" padding="2">
              <Icon as={FaTags} color="white" boxSize="6" />
            </Link>
          </Box>
        </Flex>
      </Container>
      <Container
        maxWidth="container.lg"
        backgroundColor="white"
        minHeight="100vh"
      >
        <Flex paddingTop="4" paddingBottom="4">
          <Container maxWidth="container.lg" padding="0" margin="0">
            {props.children}
          </Container>
          <Container maxWidth="44">
            <Flex direction="column" align="center" padding="4">
              <Link href="https://twitter.com/@agatan_">
                <Box position="relative" width="20" height="20">
                  <Image
                    src="/i/agatan.png"
                    layout="fill"
                    objectFit="cover"
                    alt="agatan"
                  />
                </Box>
                <Text fontSize="large" align="center">
                  @agatan
                </Text>
              </Link>
              <HStack>
                <Link href="https://twitter.com/@agatan_" padding="1">
                  <Icon as={FaTwitter} focusable boxSize="1.2em" />
                </Link>
                <Link href="https://github.com/agatan" padding="1">
                  <Icon as={FaGithub} focusable boxSize="1.2em" />
                </Link>
              </HStack>
            </Flex>
          </Container>
        </Flex>
      </Container>
    </Box>
  );
};
