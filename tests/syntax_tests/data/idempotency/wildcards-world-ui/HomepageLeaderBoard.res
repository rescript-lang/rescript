let gorillaOnVine = "/img/wildcardsimages/gorilla-on-vine.png"

open Rimble
open Globals

@react.component
let make = () => {
  let clearAndPush = RootProvider.useClearNonUrlStateAndPushRoute()

  <Box className=Styles.infoBackground>
    <Flex flexWrap="wrap">
      <Box width=[0.28] className=Styles.animalImage> <img src=gorillaOnVine width="100%" /> </Box>
      <Box width=[1., 1., 0.59] className=Styles.infoCardContainer>
        <Card className=Styles.infoCardStyles>
          <MonthlyContribution numberOfLeaders=6 />
          <br />
          <Rimble.Box width=[1.]>
            <Button
              className=Styles.centerItemsMargin
              onClick={e => {
                ReactEvent.Form.preventDefault(e)
                clearAndPush(j`/#leaderboards/monthly-contribution`)
              }}>
              {"View Other Leaderboards"->restr}
            </Button>
          </Rimble.Box>
        </Card>
      </Box>
    </Flex>
  </Box>
}
