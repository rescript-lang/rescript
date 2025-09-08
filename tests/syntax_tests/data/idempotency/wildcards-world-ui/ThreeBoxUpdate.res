// This is a very big component because it contains full 3box. They should be lazy loaded!
type threeBoxError
type boxSpaceFunctions = {
  get: (. string) => Promise.Js.t<string, threeBoxError>,
  remove: (. string) => Promise.Js.t<string, threeBoxError>,
  set: (. string, string) => Promise.Js.t<unit, threeBoxError>,
  all: (. unit) => Promise.Js.t<string, threeBoxError>,
}
type space = {
  syncDone: Promise.Js.t<bool, threeBoxError>,
  public: boxSpaceFunctions,
}
type verified3Box = {addTwitter: (. string) => Promise.Js.t<string, threeBoxError>}
type threeBox = {
  syncDone: Promise.Js.t<bool, threeBoxError>,
  openSpace: (. string) => Promise.Js.t<space, threeBoxError>,
  public: boxSpaceFunctions,
  verified: verified3Box,
  @as("DID")
  _DID: string,
}
type defaultViewSubstate =
  | NoState
  | Saved
  | FailedToSave
type threeBoxStatus =
  | DefaultView(defaultViewSubstate)
  | Loading3Box
  | Load3BoxError
  | LoggedIn(threeBox, bool) // the boolean here is if the `Space` needs to be synced too.
  | SyncedBox(threeBox)
  | SyncedBoxWithSpace(threeBox, space)
  | SyncedSpace(threeBox, space)

@module("3box")
external openBox: (. Web3.ethAddress, Web3.rawProvider) => Promise.Js.t<threeBox, threeBoxError> =
  "openBox"

module ProfileItem = {
  @react.component
  let make = (~fieldName, ~fieldTitle, ~fieldDescription, ~fieldValue, ~onEdit) => {
    let (isEdit, setIsEdit) = React.useState(_ => fieldValue->String.trim->String.length == 0)

    <React.Fragment>
      {if isEdit {
        <div>
          <Rimble.Input
            _type="string"
            value=fieldValue
            placeholder=fieldName
            onChange={event => {
              let value = ReactEvent.Form.target(event)["value"]->Belt.Option.getWithDefault("")
              onEdit(_ => value)
            }}
          />
        </div>
      } else {
        <div
          onClick={e => {
            e->ReactEvent.Mouse.preventDefault
            setIsEdit(_ => true)
          }}>
          <small>
            <strong>
              {fieldTitle->React.string}
              <Rimble.Tooltip message=fieldDescription placement="top">
                <span> {`ⓘ`->React.string} </span>
              </Rimble.Tooltip>
            </strong>
          </small>
          <br />
          {fieldValue->React.string}
          <br />
        </div>
      }}
    </React.Fragment>
  }
}

module ProfileDetails = {
  @react.component
  let make = (
    ~profileName,
    ~profileDescription,
    ~reloadUser,
    ~setThreeBoxState,
    ~threeBoxState,
  ) => {
    let (editedName, setEditedName) = React.useState(_ => profileName)
    let (editedDescription, setEditedDescription) = React.useState(_ => profileDescription)

    let nameChange = editedName != profileName
    let descriptionChange = editedDescription != profileDescription
    let areChanges = nameChange || descriptionChange

    let undoChanges = () => {
      setEditedName(_ => profileName)
      setEditedDescription(_ => profileDescription)
    }

    let optEthereumWallet = RootProvider.useCurrentUser()
    let optWeb3Provider = RootProvider.useWeb3()

    let saveChanges = () => {
      setThreeBoxState(_ => Loading3Box)
      switch (optEthereumWallet, optWeb3Provider) {
      | (Some(ethereumWallet), Some(web3Provider)) =>
        openBox(. ethereumWallet, web3Provider.provider)
        ->Promise.Js.toResult
        ->Promise.getOk(threeBoxInstance => {
          setThreeBoxState(_ => LoggedIn(threeBoxInstance, true))
          threeBoxInstance.syncDone
          ->Promise.Js.toResult
          ->Promise.get(isBoxLoaded => {
            let state = switch isBoxLoaded {
            | Ok(_finishedBoxSync) =>
              let namePromise = if profileName == editedName {
                Promise.resolved(Ok())
              } else {
                threeBoxInstance.public.set(. "name", editedName)->Promise.Js.toResult
              }
              let descriptionPromise = if profileDescription == editedDescription {
                Promise.resolved(Ok())
              } else {
                threeBoxInstance.public.set(. "description", editedDescription)->Promise.Js.toResult
              }
              Promise.all2(namePromise, descriptionPromise)->Promise.get(a => {
                let (nameSet, descriptionSet) = a
                switch (nameSet, descriptionSet) {
                | (Ok(), Ok()) =>
                  reloadUser(true)
                  setThreeBoxState(_ => DefaultView(Saved))
                | _ => setThreeBoxState(_ => DefaultView(FailedToSave))
                }
              })
              SyncedBox(threeBoxInstance)
            | Error(_) => Load3BoxError
            }
            setThreeBoxState(_ => state)
          })
          // NOTE: The below code is if we also require the wildcards space.
          // Promise.all2(
          //   threeBoxInstance.syncDone->Promise.Js.toResult,
          //   threeBoxInstance.openSpace(. "wildcards")->Promise.Js.toResult,
          // )
          // ->Promise.get(isBoxLoaded => {
          //     let (finishedBoxSyncResult, wildcardsSpaceResult) = isBoxLoaded;
          //     let state =
          //       switch (finishedBoxSyncResult, wildcardsSpaceResult) {
          //       | (Ok(_finishedBoxSync), Ok(wildcardsSpace)) =>
          //         wildcardsSpace.syncDone
          //         ->Promise.Js.toResult
          //         ->Promise.getOk(_isSpaceLoaded => {
          //             setThreeBoxState(_ =>
          //               SyncedSpace(threeBoxInstance, wildcardsSpace)
          //             )
          //           });
          //         SyncedBoxWithSpace(threeBoxInstance, wildcardsSpace);
          //       | _ => Load3BoxError
          //       };
          //     setThreeBoxState(_ => state);
          //   });
        })
        ()
      | _ => ()
      }
    }

    <Rimble.Box p=1>
      {switch threeBoxState {
      | DefaultView(_) =>
        <React.Fragment>
          <p> {"click on any item to edit it"->React.string} </p>
          <ProfileItem
            fieldDescription="Your 3box name"
            fieldName="name"
            fieldTitle="Name"
            fieldValue=editedName
            onEdit=setEditedName
          />
          <ProfileItem
            fieldDescription="Your 3box description"
            fieldName="description"
            fieldTitle="Description"
            fieldValue=editedDescription
            onEdit=setEditedDescription
          />
          {areChanges
            ? <React.Fragment>
                <Rimble.Button
                  onClick={e => {
                    e->ReactEvent.Form.preventDefault
                    saveChanges()
                  }}>
                  {"Save"->React.string}
                </Rimble.Button>
                <Rimble.Button
                  onClick={e => {
                    e->ReactEvent.Form.preventDefault
                    undoChanges()
                  }}>
                  {"Cancel"->React.string}
                </Rimble.Button>
              </React.Fragment>
            : React.null}
        </React.Fragment>
      | Loading3Box => <Rimble.Heading> {"Loading 3Box..."->React.string} </Rimble.Heading>
      | LoggedIn(_threeBoxInstance, _requiresSpace) =>
        <Rimble.Heading> {"3 box is loaded - syncing profile"->React.string} </Rimble.Heading>
      | SyncedBox(_threeBoxInstance) =>
        <Rimble.Heading> {"3 box is synced, SAVING profile data"->React.string} </Rimble.Heading>
      | SyncedBoxWithSpace(_box, _space) =>
        <React.Fragment>
          <Rimble.Heading>
            {"3 box is synced, loading 'wildcards' space"->React.string}
          </Rimble.Heading>
        </React.Fragment>
      | SyncedSpace(_box, _space) =>
        <React.Fragment>
          <Rimble.Heading> {"3 box and 'wildcards' space is synced"->React.string} </Rimble.Heading>
        </React.Fragment>
      | Load3BoxError =>
        <React.Fragment>
          <Rimble.Heading> {"There was an error loading your box"->React.string} </Rimble.Heading>
        </React.Fragment>
      }}
    </Rimble.Box>
  }
}

type registerRequestData = {
  did: string,
  twitterHandle: string,
}
type did = string
type twitterVerificationSteps =
  | Uninitialized
  | PreparePostToTwitter
  | PostToTwitter(did)
  | VerifyWithServer(did)

let getResult: string => option<string> = %raw(`
  function(result) {
    const parsedResult = JSON.parse(result);
    if (parsedResult.status === "success") {
      return parsedResult.data.verification
    } else {
      return undefined
    }
  }
`)

module TwitterVerification = {
  @react.component
  let make = (~twitterVerification, ~threeBoxState, ~setThreeBoxState, ~reloadUser) => {
    let currentUser = RootProvider.useCurrentUser()->Belt.Option.mapWithDefault("", a => a)

    let optEthereumWallet = RootProvider.useCurrentUser()
    let optWeb3Provider = RootProvider.useWeb3()
    let (twitterVerificationStep, setTwitterVerificationStep) = React.useState(_ => Uninitialized)

    let verifyTwitter = () => {
      setThreeBoxState(_ => Loading3Box)
      let _ = switch (optEthereumWallet, optWeb3Provider) {
      | (Some(ethereumWallet), Some(web3Provider)) =>
        setTwitterVerificationStep(_ => PreparePostToTwitter)
        openBox(. ethereumWallet, web3Provider.provider)
        ->Promise.Js.toResult
        ->Promise.getOk(threeBoxInstance => {
          setThreeBoxState(_ => LoggedIn(threeBoxInstance, true))
          setTwitterVerificationStep(_ => PostToTwitter(threeBoxInstance._DID))
          threeBoxInstance.syncDone
          ->Promise.Js.toResult
          ->Promise.get(isBoxLoaded => {
            let state = switch isBoxLoaded {
            | Ok(_finishedBoxSync) => SyncedBox(threeBoxInstance)
            | Error(_) => Load3BoxError
            }
            setThreeBoxState(_ => state)
          })
        })
      | _ => ()
      }
    }

    let removeTwitter = () => {
      setThreeBoxState(_ => Loading3Box)
      let _ = switch (optEthereumWallet, optWeb3Provider) {
      | (Some(ethereumWallet), Some(web3Provider)) =>
        setTwitterVerificationStep(_ => PreparePostToTwitter)
        openBox(. ethereumWallet, web3Provider.provider)
        ->Promise.Js.toResult
        ->Promise.getOk(threeBoxInstance => {
          setThreeBoxState(_ => LoggedIn(threeBoxInstance, true))
          setTwitterVerificationStep(_ => PostToTwitter(threeBoxInstance._DID))
          threeBoxInstance.syncDone
          ->Promise.Js.toResult
          ->Promise.get(isBoxLoaded => {
            let state = switch isBoxLoaded {
            | Ok(_finishedBoxSync) =>
              let _ = threeBoxInstance.public.remove(. "proof_twitter")
              reloadUser(true)
              SyncedBox(threeBoxInstance)
            | Error(_) => Load3BoxError
            }
            setThreeBoxState(_ => state)
          })
        })
      | _ => ()
      }
    }

    let submitTwitterVerification = (did, twitterHandle) => {
      setTwitterVerificationStep(_ => VerifyWithServer(did))
      let _ = {
        open Js.Promise
        Fetch.fetchWithInit(
          "https://wildcards.xyz/verification3boxTwitter",
          Fetch.RequestInit.make(
            ~method_=Post,
            ~headers=Fetch.HeadersInit.make({"Content-Type": "application/json"}),
            ~body=Fetch.BodyInit.make(
              Js.Json.stringifyAny({
                did: did,
                twitterHandle: twitterHandle,
              })->Belt.Option.mapWithDefault("{}", a => a),
            ),
            (),
          ),
        )
        ->then_(Fetch.Response.text)
        ->then_(text => {
          let optTwitterProof = getResult(text)
          switch optTwitterProof {
          | Some(twitterProof) =>
            switch threeBoxState {
            | SyncedBox(threeBoxInstance)
            | SyncedBoxWithSpace(threeBoxInstance, _)
            | SyncedSpace(threeBoxInstance, _) =>
              threeBoxInstance.verified.addTwitter(. twitterProof)
              ->Promise.Js.toResult
              ->Promise.getOk(_result => {
                reloadUser(true)
                setTwitterVerificationStep(_ => Uninitialized)
              })
              ()
            | DefaultView(_)
            | Loading3Box
            | Load3BoxError
            | LoggedIn(_, _) => ()
            }
          | _ => ()
          }
          Js.Promise.resolve()
        })
      }
    }

    let (twitterHandle, setTwitterHandle) = React.useState(_ => "")
    switch twitterVerification {
    | Some(_) =>
      <React.Fragment>
        <p> {"Your twitter is verified"->React.string} </p>
        <Rimble.Button
          onClick={e => {
            e->ReactEvent.Form.preventDefault
            let _ = removeTwitter()
          }}>
          {"Remove Verification"->React.string}
        </Rimble.Button>
      </React.Fragment>
    | None =>
      switch twitterVerificationStep {
      | Uninitialized =>
        <React.Fragment>
          <Rimble.Heading> {"verify your twitter"->React.string} </Rimble.Heading>
          <Rimble.Input
            _type="string"
            value=twitterHandle
            placeholder="twitter_handle"
            onChange={event => {
              let value = ReactEvent.Form.target(event)["value"]->Belt.Option.getWithDefault("")
              setTwitterHandle(_ => value)
            }}
          />
          <Rimble.Button
            onClick={e => {
              e->ReactEvent.Form.preventDefault
              let _ = verifyTwitter()
            }}>
            {"Verify"->React.string}
          </Rimble.Button>
        </React.Fragment>
      | PreparePostToTwitter => <p> {"Please login to 3box!"->React.string} </p>
      | PostToTwitter(did) =>
        let link = `https://twitter.com/intent/tweet?text=This Tweet links my Twitter account to my 3Box profile!\\n%0D%0A%0D%0Ahttps://wildcards.world/%23user/$currentUser\\n%0D%0A%0D%0ASupport Animal conservation @wildcards_world\\n%0D%0A✅\\n%0D%0A$did\\n%0D%0A✅`

        <React.Fragment>
          <p> {"Post the following proof to twitter"->React.string} </p>
          <p> {did->React.string} </p>
          <button>
            <a
              href=link
              target="_blank"
              rel="noopener noreferrer"
              className="modal__github__description__copy__tweet">
              {"Tweet this"->React.string}
            </a>
          </button>
          {switch threeBoxState {
          | SyncedBox(_threeBoxInstance) =>
            <Rimble.Button
              onClick={e => {
                e->ReactEvent.Form.preventDefault
                let _ = submitTwitterVerification(did, twitterHandle)
              }}>
              {"NEXT"->React.string}
            </Rimble.Button>
          | DefaultView(_)
          | Loading3Box
          | Load3BoxError
          | LoggedIn(_, _)
          | SyncedBoxWithSpace(_, _)
          | SyncedSpace(_, _) =>
            <p> {"waiting to sync 3box before continuing"->React.string} </p>
          }}
        </React.Fragment>
      | VerifyWithServer(_did) => <p> {"Verifying with server!"->React.string} </p>
      }
    }
  }
}

module ThreeBoxUpdate = {
  @react.component
  let make = () => {
    let currentUser = RootProvider.useCurrentUser()->Belt.Option.mapWithDefault("", a => a)

    let userInfoContext = UserProvider.useUserInfoContext()
    let reloadUser = forceReload => userInfoContext.update(currentUser, forceReload) // double check that data is loaded.
    reloadUser(false)

    let optThreeBoxData = UserProvider.use3BoxUserData(currentUser)

    let (threeBoxState, setThreeBoxState) = React.useState(() => DefaultView(NoState))

    switch optThreeBoxData {
    | Some(threeBoxData) =>
      <React.Fragment>
        {switch threeBoxData.profile {
        | Some(profile) =>
          <div>
            <ProfileDetails
              profileName={profile.name->Belt.Option.mapWithDefault("", a => a)}
              profileDescription={profile.description->Belt.Option.mapWithDefault("", a => a)}
              reloadUser
              threeBoxState
              setThreeBoxState
            />
          </div>
        | None => <Rimble.Loader />
        }}
        {switch threeBoxData.verifications {
        | Some(verification) =>
          <TwitterVerification
            twitterVerification=verification.twitter threeBoxState setThreeBoxState reloadUser
          />
        | None =>
          <TwitterVerification twitterVerification=None threeBoxState setThreeBoxState reloadUser />
        }}
      </React.Fragment>
    | None => <Rimble.Loader />
    }
  }
}
module Main = {
  @react.component
  let make = () => {
    let currentUser = RootProvider.useCurrentUser()->Belt.Option.mapWithDefault("", a => a)
    let (isIntegrated3Box, setIsIntegarted3Box) = React.useState(_ => false)
    <div>
      <Rimble.Heading> {"Verify your identity"->React.string} </Rimble.Heading>
      <br />
      {if isIntegrated3Box {
        <ThreeBoxUpdate />
      } else {
        <React.Fragment>
          <Rimble.Text>
            {"Please use"->React.string}
            <strong>
              <a target="_blank" rel="noopener noreferrer" href={"https://3box.io/" ++ currentUser}>
                {" 3Box.io "->React.string}
              </a>
            </strong>
            {"to validate your identity on twitter."->React.string}
          </Rimble.Text>
          <br />
          <Rimble.Text>
            <a onClick={_e => setIsIntegarted3Box(_ => true)}>
              {React.string("Try our experimental 3box integration")}
            </a>
            {React.string(" - but if in doubt go to the official 3box website.")}
          </Rimble.Text>
        </React.Fragment>
      }}
    </div>
  }
}

let default = Main.make
