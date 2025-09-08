let gorillaOnVine = "/img/wildcardsimages/gorilla-on-vine.png"

open Globals
open Rimble

module CoreConcepts = {
  @react.component
  let make = () => {
    let usedtranslationModeContext = ReactTranslate.useTranslationModeContext()
    let translation = ReactTranslate.useTranslate(. usedtranslationModeContext.translationMode)

    <Box className=Styles.infoBackground>
      <Flex flexWrap="wrap">
        <Box width=[0.28] className=Styles.animalImage>
          <img src=gorillaOnVine width="100%" />
        </Box>
        <Box width=[1., 1., 0.59] className=Styles.infoCardContainer>
          <Card className=Styles.infoCardStyles>
            <Heading className=Styles.centerText _as="h2" fontSize=5>
              {"Core Concepts"->restr}
            </Heading>
            <br />
            <hr />
            <br />
            <Heading _as="h3"> {translation(. "nft")->restr} </Heading>
            <br />
            <Text>
              {"A "->restr}
              <strong> {translation(. "nft")->restr} </strong>
              {translation(. "whatIsANFT")->restr}
            </Text>
            <br />
            <Heading _as="h3"> {"Always for Sale"->restr} </Heading>
            <br />
            <Text>
              {"When an asset is bought a new selling price is stipulated by the buyer. Assets are "->restr}
              <strong> {"Always for Sale"->restr} </strong>
              {", forever!"->restr}
            </Text>
            <br />
            <Heading _as="h3"> {translation(. "harbergerTax")->restr} </Heading>
            <br />
            <Text>
              {"The owner of an asset pays a "->restr}
              <strong> {translation(. "harbergerTax")->restr} </strong>
              {"  which is a percentage of the selling price they stipulate."->restr}
            </Text>
          </Card>
        </Box>
      </Flex>
    </Box>
  }
}
