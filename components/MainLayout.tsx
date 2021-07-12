import React from "react";
import { Box, Container, Heading, Flex, Image, Text } from "@chakra-ui/react";

import { Link } from "./Link";

type Props = {
  children: React.ReactNode
};

export const MainLayout: React.VFC<Props> = (props) => {
  return (
    <Box backgroundColor="green.50" minHeight="100vh">
      <Container maxWidth="container.lg" backgroundColor="green.600">
        <Heading padding="8" color="white" >
          <Link href="/" color="white">
            agatan blog
          </Link>
        </Heading>
      </Container>
      <Container maxWidth="container.lg" backgroundColor="white" minHeight="100vh">
        <Flex paddingTop="4" paddingBottom="4">
          <Container maxWidth="container.lg" padding="0" margin="0">
            {props.children}
          </Container>
          <Container maxWidth="44">
            <Flex direction="column" align="center" padding="4">
              <Box position="relative" width="16" height="16">
                <Image src="/agatan.png" layout="fill" objectFit="cover" alt="agatan" />
              </Box>
              <Link href="https://twitter.com/@agatan_">
                <Text>@agatan</Text>
              </Link>
            </Flex>
          </Container>
        </Flex>
      </Container>
    </Box>
  );
};
