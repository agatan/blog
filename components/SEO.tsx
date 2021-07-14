import Head from "next/head";
import React from "react";

const SITE_TITLE = "↗agatan blog↗";

export const SEO: React.VFC<{
  title: string;
  description?: string;
  image?: string;
}> = (props) => {
  const { title, description, image } = props;
  const imageUrl = image || "https://blog.agatan.dev/favicon.ico";
  return (
    <Head>
      <title>{`${title} | ${SITE_TITLE}`}</title>
      {description && <meta name="description" content={description} />}
      <meta property="og:type" content="website" />
      <meta property="og:title" content={title} />
      <meta property="og:image" content={imageUrl} />
      {description && <meta property="og:description" content={description} />}
      <meta property="og:site_name" content={SITE_TITLE} />
      <meta property="twitter:card" content="summary" />
      <meta property="twitter:creator" content="@agatan_" />
      <meta property="twitter:title" content={title} />
      {description && (
        <meta property="twitter:description" content={description} />
      )}
    </Head>
  );
};
