import React from "react";
import { getAllPostIds, getPostData, PostData } from "../../lib/posts";

type Props = {
  postData: PostData,
}

const Post: React.VFC<Props> = (props: Props) => {
  return (
    <div>
      {props.postData.id}
      {props.postData.markdown}
    </div>
  );
};
export default Post;

export async function getStaticPaths() {
  const paths = getAllPostIds();
  return {
    paths,
    fallback: false,
  }
}

export async function getStaticProps({ params }: { params: { id: string } }): Promise<{ props: Props }> {
  const postData = getPostData(params.id);
  return {
    props: {
      postData
    }
  }
}
