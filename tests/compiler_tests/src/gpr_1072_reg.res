type t

@obj
external make: (
  ~localeMatcher: @string [#lookup | @as("best fit") #best_fit]=?,
  ~timeZone: string=?,
  ~hour12: bool=?,
  ~formatMatcher: @string [#basic | @as("best fit") #best_fit]=?,
  ~weekday: [#narrow | #short | #long]=?,
  ~era: [#narrow | #short | #long]=?,
  ~year: @string [#numeric | @as("2-digit") #two_digit]=?,
  ~month: @string
  [
    | #narrow
    | #short
    | #long
    | #numeric
    | @as("2-digit") #two_digit
  ]=?,
  ~day: @string [#numeric | @as("2-digit") #two_digit]=?,
  ~hour: @string [#numeric | @as("2-digit") #two_digit]=?,
  ~minute: @string [#numeric | @as("2-digit") #two_digit]=?,
  ~second: @string [#numeric | @as("2-digit") #two_digit]=?,
  ~timeZoneName: [#short | #long]=?,
  unit,
) => t = ""

let v1 = make(
  ~localeMatcher=#best_fit,
  ~formatMatcher=#basic,
  ~day=#two_digit,
  ~timeZoneName=#short,
  (),
)

/* In the future we might allow below cases , issues are [`num] maybe escaped
    we need prevent its escaping

  external make2 :
    ?localeMatcher:
      ([`lookup | `best_fit [@as \"best fit\"]] [@string]) ->

    ?timeZone:string ->
    ?hour12:bool ->
    ?formatMatcher:
      ([`basic | `best_fit [@as \"best fit\"]] [@string]) ->

    ?weekday:([`narrow | `short | `long] [@string]) ->
    ?era:([`narrow | `short | `long] [@string]) ->
    ?year:([`numeric | `two_digit [@as \"2-digit\"]] [@string]) ->
    ?month:
      ([`narrow |
        `short |
        `long |
        `numeric |
        `two_digit [@as \"2-digit\"]] [@string]) ->

    ?day:(([`numeric | `two_digit [@as \"2-digit\"]] [@string]) as 'num) ->
    ?hour:('num) ->
    ?minute:('num) ->
    ?second:('num) ->
    ?timeZoneName:([`short | `long] [@string]) ->
    unit ->
    t =
    \"\" [@@obj]


let v2 =
     make2 
    ~localeMatcher:`best_fit     
    ~formatMatcher:`basic
    ~day:`two_digit
    ~timeZoneName:`short 
    ~hour:`two_digit
    ()
*/
