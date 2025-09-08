let raisingHandsEmoji = "/img/emoji/raising_hands.png"

module FinalNote = {
  open Globals
  open Rimble

  @react.component
  let make = () =>
    <Box className=Styles.infoBackground>
      <Flex>
        <Box className=Styles.extraInfoFooterBox>
          <Box className=Styles.emoticonHeader>
            <img className=Styles.emojiStyles src=raisingHandsEmoji alt="Emoji" />
          </Box>
          <Box className=Styles.finalNoteContent>
            <Heading className=Styles.whiteText>
              {"Wildcards is currently under active development."->restr}
            </Heading>
            <br />
            <a
              className=Styles.linkPillBox
              href="https://youtu.be/ibBAlrrwjp0?t=322"
              target="_blank"
              rel="noopener noreferrer">
              <span className=Styles.linkPillText>
                {"Watch presentation at the EthCapeTown hackathon."->restr}
              </span>
            </a>
          </Box>
        </Box>
      </Flex>
    </Box>
}
