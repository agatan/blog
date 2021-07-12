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
} from "@chakra-ui/react";
import { FaGithub, FaTwitter } from "react-icons/fa";

import { Link } from "./Link";

type Props = {
  children: React.ReactNode;
};

export const MainLayout: React.VFC<Props> = (props) => {
  return (
    <Box backgroundColor="green.50" minHeight="100vh">
      <Container maxWidth="container.lg" backgroundColor="green.600">
        <Heading padding="8" color="white">
          <Link href="/" color="white">
            agatan blog
          </Link>
        </Heading>
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
                    src="/agatan.png"
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
