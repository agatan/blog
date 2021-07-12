import type { AppProps } from "next/app";
import React from "react";
import { Box, ChakraProvider, Container, Divider, Heading, Text } from "@chakra-ui/react";

import "prismjs/themes/prism-tomorrow.css";
import Head from "next/head";
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
          <Component {...pageProps} />
        </Container>
      </Box>
    </ChakraProvider>
  );
};

export default App;
