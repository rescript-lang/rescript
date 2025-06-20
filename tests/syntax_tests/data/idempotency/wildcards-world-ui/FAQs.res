open Rimble
open Globals
open React

// FAQ Styles

let faqExplainerText = {
  open Css
  style(list{paddingTop(#px(10)), paddingBottom(#px(10)), lineHeight(#rem(1.6))})
}

let accordionContent = {
  open Css
  style(list{overflow(hidden), transition(~duration=800, ~delay=0, ~timingFunction=ease, "all")})
}

let rotate = angle => {
  open Css
  style(list{
    transform(#rotate(deg(angle))),
    transition(~duration=400, ~delay=0, ~timingFunction=ease, "all"),
  })
}

let accordionSection = {
  open Css
  style(list{display(#flex), flexDirection(column)})
}

/* FAQ Styles End */

let inlineStyle = ReactDOMRe.Style.make

module Chevron = {
  @react.component
  let make = (~className) =>
    <svg className height="14px" width="14px" viewBox="0 0 320 512">
      <path
        fill="#777"
        d="M285.476 272.971L91.132 467.314c-9.373 9.373-24.569 9.373-33.941 0l-22.667-22.667c-9.357-9.357-9.375-24.522-.04-33.901L188.505 256 34.484 101.255c-9.335-9.379-9.317-24.544.04-33.901l22.667-22.667c9.373-9.373 24.569-9.373 33.941 0L285.475 239.03c9.373 9.372 9.373 24.568.001 33.941z"
      />
    </svg>
}

@get external scrollHeight: Dom.element => int = "scrollHeight"

module FaqItem = {
  @react.component
  let make = (~isOpen, ~toggleAccordion, ~title, ~content) => {
    let (height, setHeight) = useState(_ => "0px")
    let accordianContentRef = useRef(Js.Nullable.null)

    useEffect2(() => {
      let optHeight =
        accordianContentRef.current->Js.Nullable.toOption->Belt.Option.map(_, scrollHeight)

      let height = switch optHeight {
      | Some(height) => string_of_int(height) ++ "px"
      | None => "0px"
      }
      setHeight(_ => !isOpen ? "0px" : height)
      None
    }, (isOpen, setHeight))

    <Fragment>
      <div onClick={_ => toggleAccordion()}>
        <Text className="accordion-title">
          {title->restr} <Chevron className={rotate(isOpen ? 90. : 0.)} />
        </Text>
      </div>
      <div
        className=accordionContent
        ref={ReactDOMRe.Ref.domRef(accordianContentRef)}
        style={inlineStyle(~maxHeight=height, ())}>
        <div>
          <div className=faqExplainerText> <Text className=Styles.colorGrey> content </Text> </div>
        </div>
      </div>
    </Fragment>
  }
}

type faqContentItem = {
  title: string,
  content: element,
}
let content = [
  {
    title: "Do the different wildcards represent real animals?",
    content: "Some wildcards do represent real animals, while other wildcards are symoblic of the coservation efforts of that organisation. When partnering with a conservation organisation, if that organisation has specific ainimals they track and care for, we aim to list those real animals. In other cases, conservation efforts can occur on a more macro level (without specific animals). To raise funds for these efforts we create an animal representative of that organisation. In both cases, an animal will simply represent an organisation, and funds generated from that animal will flow to the organisation it represents."->restr,
  },
  {
    title: "How do I know my funds are going to the organization?",
    content: <Fragment>
      {"Our platform uses ethereum (a blockchain), which is essentially a public transaction ledger. This means that every single action happening on this platform is completely public, verifiable and auditable. If you would like to learn more, "->restr}
      <a
        href="https://blog.wildcards.world/where-is-the-money-going/"
        target="_blank"
        rel="noopener noreferrer">
        {"read our blog post"->restr}
      </a>
      {"  which details how funds are transparently recieved by our conservation partners."->restr}
    </Fragment>,
  },
  {
    title: "Can I only buy a wildcard using cryptocurrency?",
    content: <Fragment>
      {"Unfortunately, for now cryptocurrency is the only way to buy a wildcard. We are actively working on FIAT currency solutions. In the mean time, we have intergrated with Torus, allowing you to use your credit card easily to buy the required ETH necessary to purchase a wildcard."->restr}
      <a
        href="https://blog.wildcards.world/how-to-buy-a-wildcard-web3-ethereum/"
        target="_blank"
        rel="noopener noreferrer">
        {" Read our guide"->restr}
      </a>
      {" to find out more about how to buy a wildcard."->restr}
    </Fragment>,
  },
  {
    title: "What is a non-fungible token?",
    content: "A non-fungible token (NFT), is simply put, a token that is unique. This is in contrast to bitcoin (which is fungible), as each bitcoin is always equivalent to another one in value. Every wildcard is an NFT representing a unique animal. Since wildcards are non-fungible (unique), every wildcard has a unique history and independant value."->restr,
  },
  {
    title: "What does 'always for sale' mean?",
    content: "Always for sale means exactly what it says, the asset or thing in question can be bought by any person at anytime, and is hence, always for sale. How this functions in practice is very interesting. The owner of the asset is required to always have a listed selling price for this asset at which anyone could buy this asset from them at any point in time. The owner won’t set the selling price too high because they are required to continually pay a percentage of the selling price in order to keep ownership of the asset."->restr,
  },
  {
    title: "What is a deposit and what happens if my deposit runs out?",
    content: "Your deposit is used to contiually pay the percentage fee of the selling price you set, in order for you to continue your ownership of the asset. If your deposit runs out then anyone can claim the asset for free and set a new selling price."->restr,
  },
  {
    title: "Can I join wildcards and raise funds for my animal conservation project?",
    content: "Yes! Please send an email to jonjon@wildcards.world"->restr,
  },
]

@react.component
let make = () => {
  let (activeIndex, setActiveIndex) = useState(() => -1)
  let toggleAccordion = (faqItemIndex, ()) =>
    setActiveIndex(_ => faqItemIndex == activeIndex ? -1 : faqItemIndex)
  <div className=Styles.infoBackground>
    <Rimble.Box className=Styles.floatingSignupBox>
      <Rimble.Flex flexWrap="wrap" alignItems="center" className=Styles.floatingSignupBoxInner>
        <Rimble.Box width=[1.]>
          <Rimble.Card>
            <Heading> {"Frequently Asked Questions (FAQs)"->restr} </Heading>
            <div className=accordionSection>
              {Array.mapWithIndex(content, (index, {title, content}) =>
                <FaqItem
                  key={index->string_of_int}
                  isOpen={index == activeIndex}
                  toggleAccordion={toggleAccordion(index)}
                  title
                  content
                />
              )->array}
            </div>
          </Rimble.Card>
        </Rimble.Box>
      </Rimble.Flex>
    </Rimble.Box>
  </div>
}
