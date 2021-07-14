import Head from "next/head";
import React from "react";

const SITE_TITLE = "↗agatan blog↗";

export const SEO: React.VFC<{
  title?: string;
  description?: string;
  image?: string;
}> = (props) => {
  const { title, description, image } = props;
  const titleString = title != null ? `${title} | ${SITE_TITLE}` : SITE_TITLE;
  const imageUrl = image || "https://blog.agatan.dev/favicon.ico";
  return (
    <Head>
      <title>{titleString}</title>
      {description && <meta name="description" content={description} />}
      <meta
        property="og:type"
        content={title == null ? "website" : "article"}
      />
      <meta property="og:title" content={titleString} />
      <meta property="og:image" content={imageUrl} />
      {description && <meta property="og:description" content={description} />}
      <meta property="og:site_name" content={SITE_TITLE} />
      <meta property="twitter:card" content="summary" />
      <meta property="twitter:creator" content="@agatan_" />
      <meta property="twitter:title" content={titleString} />
      {description && (
        <meta property="twitter:description" content={description} />
      )}
    </Head>
  );
};
