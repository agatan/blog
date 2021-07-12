import type { AppProps } from "next/app";
import React from "react";
import { Box, ChakraProvider, Container, Flex, Heading, Image, Spacer, Text, } from "@chakra-ui/react";

import "prismjs/themes/prism-tomorrow.css";
import { Link } from "../components/Link";

const App: React.FC<AppProps> = ({ Component, pageProps }) => {
  return (
    <ChakraProvider>
      <Component {...pageProps} />
    </ChakraProvider>
  );
};

export default App;
