exception UnknownKindOfLink(string)

type schoolStrings = {
  address: option<string>,
  emailAddress: option<string>,
  privacyPolicy: option<string>,
  termsOfUse: option<string>,
}

type file = {
  url: string,
  filename: string,
}

type schoolImages = {
  logoOnLightBg: option<file>,
  coverImage: option<file>,
  icon: file,
}

type linkId = string
type title = string
type url = string

type link =
  | HeaderLink(linkId, title, url)
  | FooterLink(linkId, title, url)
  | SocialLink(linkId, url)

type t = {
  schoolStrings: schoolStrings,
  schoolImages: schoolImages,
  links: list<link>,
}

let logoOnLightBg = t => t.schoolImages.logoOnLightBg
let icon = t => t.schoolImages.icon
let coverImage = t => t.schoolImages.coverImage

let url = file => file.url
let filename = file => file.filename

let address = t => t.schoolStrings.address
let emailAddress = t => t.schoolStrings.emailAddress
let privacyPolicy = t => t.schoolStrings.privacyPolicy
let termsOfUse = t => t.schoolStrings.termsOfUse

let headerLinks = t =>
  t.links->List.filter(l =>
    switch l {
    | HeaderLink(_, _, _) => true
    | FooterLink(_, _, _) => false
    | SocialLink(_, _) => false
    }
  )

let footerLinks = t =>
  t.links->List.filter(l =>
    switch l {
    | HeaderLink(_, _, _) => false
    | FooterLink(_, _, _) => true
    | SocialLink(_, _) => false
    }
  )

let socialLinks = t =>
  t.links->List.filter(l =>
    switch l {
    | HeaderLink(_, _, _) => false
    | FooterLink(_, _, _) => false
    | SocialLink(_, _) => true
    }
  )

let unpackLinks = links =>
  links->List.map(l =>
    switch l {
    | HeaderLink(id, title, url)
    | FooterLink(id, title, url) => (id, title, url)
    | SocialLink(id, url) => (id, "", url)
    }
  )

let addLink = (link, t) => {...t, links: \"@"(t.links, list{link})}

let removeLink = (linkId, t) => {
  ...t,
  links: t.links->List.filter(l =>
    switch l {
    | HeaderLink(id, _, _)
    | FooterLink(id, _, _) =>
      id != linkId
    | SocialLink(id, _) => id != linkId
    }
  ),
}

let optionalString = s =>
  switch s->String.trim {
  | "" => None
  | nonEmptyString => Some(nonEmptyString)
  }

let updatePrivacyPolicy = (privacyPolicy, t) => {
  ...t,
  schoolStrings: {
    ...t.schoolStrings,
    privacyPolicy: privacyPolicy->optionalString,
  },
}

let updateTermsOfUse = (termsOfUse, t) => {
  ...t,
  schoolStrings: {
    ...t.schoolStrings,
    termsOfUse: termsOfUse->optionalString,
  },
}

let updateAddress = (address, t) => {
  ...t,
  schoolStrings: {
    ...t.schoolStrings,
    address: address->optionalString,
  },
}

let updateEmailAddress = (emailAddress, t) => {
  ...t,
  schoolStrings: {
    ...t.schoolStrings,
    emailAddress: emailAddress->optionalString,
  },
}

let decodeFile = json => {
  open Json.Decode
  {
    url: json->field("url", string),
    filename: json->field("filename", string),
  }
}

let decodeImages = json => {
  open Json.Decode
  {
    logoOnLightBg: json->field("logoOnLightBg", optional(decodeFile)),
    coverImage: json->field("coverImage", optional(decodeFile)),
    icon: json->field("icon", decodeFile),
  }
}

let updateImages = (json, t) => {...t, schoolImages: json->decodeImages}

let decodeStrings = json => {
  open Json.Decode
  {
    address: json->field("address", optional(string)),
    emailAddress: json->field("emailAddress", optional(string)),
    privacyPolicy: json->field("privacyPolicy", optional(string)),
    termsOfUse: json->field("termsOfUse", optional(string)),
  }
}

let decodeLink = json => {
  let (kind, id, url) = {
    open Json.Decode
    (field("kind", string, json), field("id", string, json), field("url", string, json))
  }

  let title = switch kind {
  | "header"
  | "footer" =>
    open Json.Decode
    field("title", string, json)
  | _ => ""
  }

  switch kind {
  | "header" => HeaderLink(id, title, url)
  | "footer" => FooterLink(id, title, url)
  | "social" => SocialLink(id, url)
  | unknownKind => throw(UnknownKindOfLink(unknownKind))
  }
}

let decode = json => {
  open Json.Decode
  {
    schoolStrings: json->field("strings", decodeStrings),
    schoolImages: json->field("images", decodeImages),
    links: json->field("links", list(decodeLink)),
  }
}
