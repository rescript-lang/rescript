open Globals

type featuredMediaItemType = {
  imagePath: string,
  link: string,
}

let featuredMediaContent: array<featuredMediaItemType> = [
  {
    imagePath: "/img/featured-media/hackernoon.svg",
    link: "https://hackernoon.com/connecting-the-dots-between-blockchain-and-sustainability-6ncc3234",
  },
  {
    imagePath: "/img/featured-media/ubisoft.svg",
    link: "https://news.ubisoft.com/en-us/article/0fbsdbsFNL5T0vTDkx924/how-the-ubisoft-entrepreneurs-lab-empowers-communities",
  },
  {
    imagePath: "/img/featured-media/decrypt.svg",
    link: "https://decrypt.co/10561/ubisoft-picks-eight-blockchain-startups-for-entrepreneur-lab",
  },
  {
    imagePath: "/img/featured-media/ventureburn.svg",
    link: "https://ventureburn.com/2020/01/sa-crypto-startup-wildcards-cv-vc-investment/",
  },
  {
    imagePath: "/img/featured-media/cointelegraph.svg",
    link: "https://cointelegraph.com/news/wildcards-purports-to-save-endangered-species-with-technical-first-for-ethereum",
  },
  {
    imagePath: "/img/featured-media/coindesk.svg",
    link: "https://www.coindesk.com/nfts-boring-make-fun",
  },
]

let featuredItemImgStyle = {
  open Css
  style(list{width(#percent(80.)), minWidth(#px(30)), padding(#rem(1.))})
}

let featuredContainerStyles = {
  open Css
  style(list{maxWidth(px(1200)), margin(auto)})
}

@react.component
let make = () =>
  <Rimble.Box className=Styles.horizantalBlueTile>
    <p className=Styles.explainerLargeText> {"Featured in "->restr} </p>
    <Rimble.Flex
      alignItems="center" justifyContent="center" flexWrap="wrap" className=featuredContainerStyles>
      {React.array(
        featuredMediaContent->Array.mapWithIndex((index, x) =>
          <Rimble.Box key={index->string_of_int} p=2 mb=2 width=[0.5, 0.5, 0.16]>
            <a href=x.link> <img src=x.imagePath className=featuredItemImgStyle /> </a>
          </Rimble.Box>
        ),
      )}
    </Rimble.Flex>
  </Rimble.Box>
