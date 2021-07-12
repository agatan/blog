import type { AppProps } from "next/app";
import React from "react";
import { Box, ChakraProvider, Container, Flex, Heading, Image, Spacer, Text, } from "@chakra-ui/react";

import "prismjs/themes/prism-tomorrow.css";
import { Link } from "../components/Link";

const App: React.FC<AppProps> = ({ Component, pageProps }) => {
  return (
    <ChakraProvider>
      <Box backgroundColor="green.50">
        <Container maxWidth="container.lg" backgroundColor="green.600">
          <Heading padding="8" color="white" >
            <Link href="/" color="white">
              agatan blog
            </Link>
          </Heading>
        </Container>
        <Container maxWidth="container.lg" backgroundColor="white">
          <Flex paddingTop="4" paddingBottom="4">
            <Component {...pageProps} />
            <Container maxWidth="44">
              <Flex direction="column" align="center" padding="4">
                <Box position="relative" width="16" height="16">
                  <Image src="/agatan.png" layout="fill" objectFit="cover" alt="agatan" />
                </Box>
                <Link href="https://twitter.com/@agatan_">
                  <Text>@agatan</Text>
                </Link>
                <Spacer />
              </Flex>
            </Container>
          </Flex>
        </Container>
      </Box>
    </ChakraProvider>
  );
};

export default App;
