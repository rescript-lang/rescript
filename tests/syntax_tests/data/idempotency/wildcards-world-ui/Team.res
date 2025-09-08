open Globals

type teamMember = {
  name: string,
  img: string,
  title: string,
  linkedIn: option<string>,
  twitterHandle: option<string>,
  githubHandle: option<string>,
}

let cardStyle = {
  open Css
  style(list{height(#percent(100.)), backgroundColor(#hex("73C8D7"))})
}

let titleStyle = {
  open Css
  style(list{color(#hex("cccccc"))})
}

let profileImageStyle = {
  open Css
  style(list{
    borderWidth(px(6)),
    borderStyle(#solid),
    borderColor(#hex("73C8D7")),
    width(#percent(100.)),
  })
}

let plantStyle = {
  open Css
  style(list{width(#percent(100.)), transform(translateY(px(30)))})
}

let iconStyle = {
  open Css
  style(list{height(px(20)), paddingRight(px(3)), margin(#auto)})
}

@react.component
let make = () => {
  // Can maybe move this to the db, although overkill at this point
  let teamMembers: array<teamMember> = [
    {
      name: "JonJon Clark",
      img: "https://dd2wadt5nc0o7.cloudfront.net/team/jonjon.jpg",
      title: "Co-founder",
      linkedIn: Some("https://www.linkedin.com/in/jonathan-clark-637344143/"),
      twitterHandle: Some("jonjonclark"),
      githubHandle: Some("moose-code"),
    },
    {
      name: "Jason Smythe",
      img: "https://dd2wadt5nc0o7.cloudfront.net/team/jason.jpg",
      title: "Co-founder",
      linkedIn: Some("https://www.linkedin.com/in/jason-smythe-0501ab88/"),
      twitterHandle: Some("jasoonsmythe"),
      githubHandle: Some("jasoons"),
    },
    {
      name: "Denham Preen",
      img: "https://dd2wadt5nc0o7.cloudfront.net/team/denham.jpg",
      title: "Co-founder",
      linkedIn: Some("https://www.linkedin.com/in/denhampreen/"),
      twitterHandle: Some("denhampreen"),
      githubHandle: Some("DenhamPreen"),
    },
    {
      name: "Rio Button",
      img: "https://dd2wadt5nc0o7.cloudfront.net/team/rio.jpg",
      title: "Lead conservationist",
      linkedIn: Some("https://www.linkedin.com/in/riob/"),
      twitterHandle: Some("biologistbutton"),
      githubHandle: None,
    },
    {
      name: "Luke Gillott",
      img: "https://dd2wadt5nc0o7.cloudfront.net/team/luke.jpg",
      title: "Executive board",
      linkedIn: Some("https://www.linkedin.com/in/luke-gillott/"),
      twitterHandle: None,
      githubHandle: None,
    },
  ]
  <div>
    <Rimble.Flex
      flexWrap="wrap" justifyContent="space-around" alignItems="stretch" pt=20 pb=20 px=50>
      <h1> {"Wildcards Team"->restr} </h1>
    </Rimble.Flex>
    <Rimble.Flex flexWrap="wrap" justifyContent="space-around" alignItems="stretch" pb=50 px=50>
      {React.array(
        teamMembers->Array.mapWithIndex((i, member) =>
          <Rimble.Box key={i->string_of_int} mt=20 mb=20 width=[0.45, 0.45, 0.18]>
            <Rimble.Card className=cardStyle>
              <img className=profileImageStyle src=member.img alt=member.name />
              <p>
                {member.name->React.string}
                <br />
                <span className=titleStyle> <small> {member.title->React.string} </small> </span>
              </p>
              {switch member.linkedIn {
              | Some(link) =>
                <a target="_blank" rel="noopener noreferrer" href=link>
                  <img className=iconStyle src="/img/socials/linkedin.svg" alt=member.name />
                </a>
              | None => React.null
              }}
              {switch member.twitterHandle {
              | Some(handle) =>
                <a
                  target="_blank" rel="noopener noreferrer" href={"https://twitter.com/" ++ handle}>
                  <img className=iconStyle src="/img/socials/twitter.svg" alt=handle />
                </a>
              | None => React.null
              }}
              {switch member.githubHandle {
              | Some(handle) =>
                <a target="_blank" rel="noopener noreferrer" href={"https://github.com/" ++ handle}>
                  <img className=iconStyle src="/img/socials/github.svg" alt=handle />
                </a>
              | None => React.null
              }}
            </Rimble.Card>
          </Rimble.Box>
        ),
      )}
    </Rimble.Flex>
    <Rimble.Flex flexWrap="wrap" justifyContent="space-around" alignItems="stretch" pt=2 pb=2 px=50>
      <img className=plantStyle src="/img/wildcardsimages/plants.png" alt="wildcards plants" />
    </Rimble.Flex>
  </div>
}
