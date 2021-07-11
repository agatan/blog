import React from "react";
import NextLink from "next/link";
import { Link as ChakraLink, LinkProps } from "@chakra-ui/react";

export const Link: React.VFC<
  LinkProps & { href: string; children: React.ReactNode }
> = (props) => {
  return (
    <NextLink href={props.href}>
      <ChakraLink
        {...props}
        onClick={(ev) => {
          if (props.onClick != null) {
            props.onClick(ev);
          }
          (document.activeElement as HTMLElement).blur();
        }}
      />
    </NextLink>
  );
};
