import type { AppProps } from "next/app";
import React from "react";
import { ChakraProvider, Container } from "@chakra-ui/react";

import "prismjs/themes/prism-tomorrow.css";
import Head from "next/head";

const App: React.FC<AppProps> = ({ Component, pageProps }) => {
  return (
    <ChakraProvider>
      <Head>
        <title>右上↗</title>
      </Head>
      <Container maxWidth="container.lg">
        <Component {...pageProps} />
      </Container>
    </ChakraProvider>
  );
};

export default App;
