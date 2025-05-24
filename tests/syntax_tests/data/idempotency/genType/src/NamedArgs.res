open GenTypeCommon

type groupedArg =
  | Group(fields)
  | Arg(type_)

@ocaml.doc("
 * For convenient processing turns consecutive named arguments into a
 * `NamedArgs` group, and individual non-named arguments into `Arg`s.
 ")
let rec groupReversed = (~revCurGroup, ~revResult, labeledTypes) =>
  switch (revCurGroup, labeledTypes) {
  | (list{}, list{(Nolabel, type_), ...tl}) =>
    groupReversed(~revCurGroup=list{}, ~revResult=list{Arg(type_), ...revResult}, tl)
  /* Add it to the current group, not result. */
  | (_, list{(OptLabel(name), type_), ...tl}) =>
    groupReversed(
      ~revCurGroup=list{
        {
          mutable_: Immutable,
          nameJS: name,
          nameRE: name,
          optional: Optional,
          type_: type_,
        },
        ...revCurGroup,
      },
      ~revResult,
      tl,
    )
  | (_, list{(Label(name), type_), ...tl}) =>
    groupReversed(
      ~revCurGroup=list{
        {
          mutable_: Immutable,
          nameJS: name,
          nameRE: name,
          optional: Mandatory,
          type_: type_,
        },
        ...revCurGroup,
      },
      ~revResult,
      tl,
    )
  | (list{}, list{}) => revResult
  | (list{_grpHd, ..._grpTl}, list{} as _tl)
  /* Just form the group, and recurse ignoring the (None, t) in that case.
   * it will be handled in recursion. */
  | (list{_grpHd, ..._grpTl}, list{(Nolabel, _), ..._tl}) =>
    groupReversed(
      ~revCurGroup=list{},
      ~revResult=list{Group(revCurGroup), ...revResult},
      labeledTypes,
    )
  }

@ocaml.doc("
 * Special reverse that not only reverses the entire list but also the order of
 * items in the NamedArgs grouping.
 ")
let rec reverse = (~soFar=list{}, lst) =>
  switch lst {
  | list{} => soFar
  | list{Arg(type_)}
    if type_ == unitT && soFar == list{} => // treat a single argument of type unit as no argument
    list{}
  | list{Arg(type_), ...tl} => reverse(~soFar=list{{aName: "", aType: type_}, ...soFar}, tl)
  | list{Group(fields), ...tl} =>
    reverse(~soFar=list{{aName: "", aType: GroupOfLabeledArgs(List.rev(fields))}, ...soFar}, tl)
  }

let group = labeledTypes =>
  labeledTypes->groupReversed(~revCurGroup=list{}, ~revResult=list{})->reverse
