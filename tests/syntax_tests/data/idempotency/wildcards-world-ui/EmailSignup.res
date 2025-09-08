let gorillaOnVine = "/img/wildcardsimages/gorilla-on-vine.png"

open Globals
open Rimble

@react.component
let make = () =>
  <div className=Styles.horizantalBlueTile id="signup">
    <Box className=Styles.floatingSignupBox>
      <Flex flexWrap="wrap" alignItems="center" className=Styles.floatingSignupBoxInner>
        <Box className=Styles.emailTextBox width=[1., 1., 0.4]>
          <p className=Styles.emailSignupHeader>
            <strong> {"Want to stay in the loop?"->restr} </strong>
          </p>
          <span> {"Sign up for our newsletter"->restr} </span>
          <br />
          <br />
        </Box>
        <Box width=[1., 1., 0.55]>
          <Rimble.Form
            action="https://world.us20.list-manage.com/subscribe/post?u=11056167825e6c3d276b0a362&id=66d23199dd"
            _method="post">
            <Flex flexWrap="wrap">
              <Box width=[1., 0.6] className=Styles.inputElements>
                <Form.Input
                  _type="email"
                  name="EMAIL"
                  width=1.
                  onChange={(e: ReactEvent.Form.t) =>
                    @warning("-20")
                    ReactEvent.Form.target(e)["parentNode"]["classList"]["add"]("was-validated")}
                />
              </Box>
              <Box width=[1., 0.4] className=Styles.inputElements>
                <Button width=1.> {"Submit"->restr} </Button>
              </Box>
            </Flex>
          </Rimble.Form>
        </Box>
      </Flex>
    </Box>
  </div>
