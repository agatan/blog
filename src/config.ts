import type { Site, SocialObjects } from "./types";

export const SITE: Site = {
  website: "https://blog.agatan.dev/",
  author: "agatan",
  desc: "agatan blog",
  title: "agatan blog",
  ogImage: "",
  lightAndDarkMode: true,
  postPerPage: 10,
};

export const LOCALE = ["ja-JP", "en-EN"]; // set to [] to use the environment default

export const LOGO_IMAGE = {
  enable: false,
  svg: true,
  width: 216,
  height: 46,
};

export const SOCIALS: SocialObjects = [
  {
    name: "Github",
    href: "https://github.com/agatan/blog",
    linkTitle: ` ${SITE.title} on Github`,
    active: true,
  },
  {
    name: "Mastodon",
    href: "https://mstdn.jp/@agatan",
    linkTitle: `Mastodon`,
    active: true,
  },
  {
    name: "X",
    href: "https://twitter.com/agatan_",
    linkTitle: `X`,
    active: true,
  },
  {
    name: "LinkedIn",
    href: "www.linkedin.com/in/agatan",
    linkTitle: `LinkedIn`,
    active: true,
  },
];
