import type { Site, SocialObjects } from "./types";

export const SITE: Site = {
  website: "https://astro-paper.pages.dev/",
  author: "Sat Naing",
  desc: "A minimal, responsive and SEO-friendly Astro blog theme.",
  title: "AstroPaper",
  ogImage: "astropaper-og.jpg",
  lightAndDarkMode: true,
  postPerPage: 3,
};

export const LOCALE = ["en-EN"]; // set to [] to use the environment default

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
