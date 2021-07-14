import type { AppProps } from "next/app";
import React, { useEffect } from "react";
import { ChakraProvider } from "@chakra-ui/react";

import "prismjs/themes/prism-okaidia.css";
import { useRouter } from "next/router";
import * as gtag from "../lib/gtag";

const App: React.FC<AppProps> = ({ Component, pageProps }) => {
  const router = useRouter();
  useEffect(() => {
    const handleRouteChange = (url: string) => {
      gtag.pageview(url);
    };
    router.events.on("routeChangeComplete", handleRouteChange);
    return () => {
      router.events.off("routeChangeComplete", handleRouteChange);
    };
  }, [router.events]);
  return (
    <ChakraProvider>
      <Component {...pageProps} />
    </ChakraProvider>
  );
};

export default App;
