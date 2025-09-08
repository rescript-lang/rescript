include {
          include (
                    {
                      type t('value) = {
                        mutable size: int,
                        mutable root: option(node('value)),
                        compare:
                          Js.Internal.fn([ | `Arity_2('value, 'value)], int),
                      };
                    }: {

                    }
                  );
          type t('value);
          external t:
            (
              ~size: int,
              ~root: option(node('value)),
              ~compare: Js.Internal.fn([ | `Arity_2('value, 'value)], int)
            ) =>
            t('value) =
            ""
            "BS:6.0.1\x84\x95\xa6\xbe\0\0\0#\0\0\0\r\0\0\0&\0\0\0#\x91\xa0\xa0A\xa0$size@\xa0\xa0A\xa0$root@\xa0\xa0A\xa0'compare@@";
          external sizeSet: (t('value), int) => unit =
            "size"
            "BS:6.0.1\x84\x95\xa6\xbe\0\0\0\x15\0\0\0\t\0\0\0\x1a\0\0\0\x19\xb0\xa0\xa0A\x91@\xa0\xa0A\x04\x03@E\x97\xa0$size@";
          [@ocaml.deprecated
            "use sizeGet instead or use {abstract = light} explicitly"
          ]
          external size: t('value) => int =
            ""
            "BS:6.0.1\x84\x95\xa6\xbe\0\0\0\x10\0\0\0\x07\0\0\0\x14\0\0\0\x13\xb0\xa0\xa0A\x91@@A\x98\xa0$size@";
          external sizeGet: t('value) => int =
            ""
            "BS:6.0.1\x84\x95\xa6\xbe\0\0\0\x10\0\0\0\x07\0\0\0\x14\0\0\0\x13\xb0\xa0\xa0A\x91@@A\x98\xa0$size@";
          external rootSet: (t('value), option(node('value))) => unit =
            "root"
            "BS:6.0.1\x84\x95\xa6\xbe\0\0\0\x15\0\0\0\t\0\0\0\x1a\0\0\0\x19\xb0\xa0\xa0A\x91@\xa0\xa0A\x04\x03@E\x97\xa0$root@";
          [@ocaml.deprecated
            "use rootGet instead or use {abstract = light} explicitly"
          ]
          external root: t('value) => option(node('value)) =
            ""
            "BS:6.0.1\x84\x95\xa6\xbe\0\0\0\x10\0\0\0\x07\0\0\0\x14\0\0\0\x13\xb0\xa0\xa0A\x91@@A\x98\xa0$root@";
          external rootGet: t('value) => option(node('value)) =
            ""
            "BS:6.0.1\x84\x95\xa6\xbe\0\0\0\x10\0\0\0\x07\0\0\0\x14\0\0\0\x13\xb0\xa0\xa0A\x91@@A\x98\xa0$root@";
          [@ocaml.deprecated
            "use compareGet instead or use {abstract = light} explicitly"
          ]
          external compare:
            t('value) => Js.Internal.fn([ | `Arity_2('value, 'value)], int) =
            ""
            "BS:6.0.1\x84\x95\xa6\xbe\0\0\0\x13\0\0\0\x07\0\0\0\x14\0\0\0\x13\xb0\xa0\xa0A\x91@@A\x98\xa0'compare@";
          external compareGet:
            t('value) => Js.Internal.fn([ | `Arity_2('value, 'value)], int) =
            ""
            "BS:6.0.1\x84\x95\xa6\xbe\0\0\0\x13\0\0\0\x07\0\0\0\x14\0\0\0\x13\xb0\xa0\xa0A\x91@@A\x98\xa0'compare@";
        };
let has = (rbt, value) => _findNode(rbt, rootGet(rbt), value) !== None;
let rec minNode = node =>
let findMin = rbt =>
let removeNode = (rbt, node) => {
  let nodeToRemove =
    switch (leftGet(node), rightGet(node)) {
    | (Some(_), Some(_)) =>
      let successor = castNotOption(minNode(rightGet(node)));
      valueSet(node, valueGet(successor));
      heightSet(node, heightGet(successor));
      successor;
    | _ => node
    };
  let successor =
    switch (leftGet(nodeToRemove)) {
    | None => rightGet(nodeToRemove)
    | left => left
    };
  let (successor, isLeaf) =
    switch (successor) {
    | None =>
      let leaf =
        createNode(
          ~value=Js.Internal.raw_expr("0"),
          ~color=Black,
          ~height=0.,
        );
      let isLeaf = Js.Internal.fn_mk1(x => x === leaf);
      (leaf, isLeaf);
    | Some(successor) => (successor, Js.Internal.fn_mk1(_ => false))
    };
  let nodeParent = parentGet(nodeToRemove);
  parentSet(successor, nodeParent);
  switch (nodeParent) {
  | None => ()
  | Some(parent) =>
    leftOrRightSet(parent, ~node=nodeToRemove, Some(successor))
  };
  updateSumRecursive(rbt, successor);
  if (colorGet(nodeToRemove) === Black) {
    if (colorGet(successor) === Red) {
      colorSet(successor, Black);
      if (parentGet(successor) === None) {
        rootSet(rbt, Some(successor));
      };
    } else {
      let break = ref(false);
      let successorRef = ref(successor);
      while (!break.contents) {
        let successor = successorRef.contents;
        switch (parentGet(successor)) {
        | None =>
          rootSet(rbt, Some(successor));
          break.contents = true;
        | Some(successorParent) =>
          let sibling = siblingOf(successor);
          if (sibling !== None && colorGet(castNotOption(sibling)) === Red) {
            colorSet(successorParent, Red);
            colorSet(castNotOption(sibling), Black);
            if (isLeft(successor)) {
              rotateLeft(rbt, successorParent);
            } else {
              rotateRight(rbt, successorParent);
            };
          };
          let sibling = siblingOf(successor);
          let siblingNN = castNotOption(sibling);
          if (colorGet(successorParent) === Black
              && (
                sibling === None
                || (
                  colorGet(siblingNN) === Black
                  && (
                    leftGet(siblingNN) === None
                    || colorGet(castNotOption(leftGet(siblingNN))) === Black
                  )
                )
                && (
                  rightGet(siblingNN) === None
                  || colorGet(castNotOption(rightGet(siblingNN))) === Black
                )
              )) {
            if (sibling !== None) {
              colorSet(siblingNN, Red);
            };
            successorRef.contents = successorParent;
          } else if (colorGet(successorParent) === Red
                     && (
                       sibling === None
                       || (
                         colorGet(siblingNN) === Black
                         && (
                           leftGet(siblingNN) === None
                           || colorGet(castNotOption(leftGet(siblingNN)))
                           === Black
                         )
                       )
                       && (
                         rightGet(siblingNN) === None
                         || colorGet(castNotOption(rightGet(siblingNN)))
                         === Black
                       )
                     )) {
            if (sibling !== None) {
              colorSet(siblingNN, Red);
            };
            colorSet(successorParent, Black);
            break.contents = true;
          } else if (sibling !== None
                     && colorGet(castNotOption(sibling)) === Black) {
            let sibling = castNotOption(sibling);
            if ((
                  (
                    isLeft(successor)
                    && (
                      rightGet(sibling) === None
                      || colorGet(castNotOption(rightGet(sibling)))
                      === Black
                    )
                  )
                  && leftGet(sibling) !== None
                )
                && colorGet(castNotOption(leftGet(sibling))) === Red) {
              colorSet(sibling, Red);
              colorSet(castNotOption(leftGet(sibling)), Black);
              rotateRight(rbt, sibling);
            } else if ((
                         (
                           !isLeft(successor)
                           && (
                             leftGet(sibling) === None
                             || colorGet(castNotOption(leftGet(sibling)))
                             === Black
                           )
                         )
                         && rightGet(sibling) !== None
                       )
                       && colorGet(castNotOption(rightGet(sibling))) === Red) {
              colorSet(sibling, Red);
              colorSet(castNotOption(rightGet(sibling)), Black);
              rotateLeft(rbt, sibling);
            };
            break.contents = true;
          } else {
            let sibling = siblingOf(successor);
            let sibling = castNotOption(sibling);
            colorSet(sibling, colorGet(successorParent));
            if (isLeft(successor)) {
              colorSet(castNotOption(rightGet(sibling)), Black);
              rotateRight(rbt, successorParent);
            } else {
              colorSet(castNotOption(leftGet(sibling)), Black);
              rotateLeft(rbt, successorParent);
            };
          };
        };
      };
    };
  };
  if (Js.Internal.fn_run1(isLeaf, successor)) {
    if (rootGet(rbt) === Some(successor)) {
      rootSet(rbt, None);
    };
    switch (parentGet(successor)) {
    | None => ()
    | Some(parent) => leftOrRightSet(parent, ~node=successor, None)
    };
  };
};
let remove = (rbt, value) =>
  switch (_findNode(rbt, rootGet(rbt), value)) {
  | Some(node) =>
    removeNode(rbt, node);
    sizeSet(rbt, sizeGet(rbt) - 1);
    Some(heightGet(node));
  | None => None
  };
let findThroughCallback = (rbt, cb) => {
  let rec findThroughCallback = (rbt, node, cb) =>
    switch (node) {
    | None => None
    | Some(node) =>
      let cmp = Js.Internal.fn_run1(cb, valueGet(node));
      if (cmp === 0) {
        Some(node);
      } else if (cmp < 0) {
        findThroughCallback(rbt, leftGet(node), cb);
      } else {
        findThroughCallback(rbt, rightGet(node), cb);
      };
    };
  switch (findThroughCallback(rbt, rootGet(rbt), cb)) {
  | None => None
  | Some(node) => Some(valueGet(node))
  };
};
let make = (~compare) => t(~size=0, ~root=None, ~compare);
let rec heightOfInterval = (rbt, node, lhs, rhs) =>
  switch (node) {
  | None => 0.
  | Some(n) =>
    if (lhs === None && rhs === None) {
      sumGet(n);
    } else if (lhs !== None
               && Js.Internal.fn_run2(
                    compareGet(rbt),
                    valueGet(n),
                    castNotOption(lhs),
                  )
               < 0) {
      heightOfInterval(rbt, rightGet(n), lhs, rhs);
    } else if (rhs !== None
               && Js.Internal.fn_run2(
                    compareGet(rbt),
                    valueGet(n),
                    castNotOption(rhs),
                  )
               > 0) {
      heightOfInterval(rbt, leftGet(n), lhs, rhs);
    } else {
      heightGet(n)
      +. heightOfInterval(rbt, leftGet(n), lhs, None)
      +. heightOfInterval(rbt, rightGet(n), None, rhs);
    }
  };
let heightOfInterval = (rbt, lhs, rhs) =>
  heightOfInterval(rbt, rootGet(rbt), lhs, rhs);
let rec firstVisibleNode = (node, offset) =>
  switch (node) {
  | None => None
  | Some(node) =>
    if (sumGet(node) <= offset) {
      None;
    } else {
      let nodeHeight = heightGet(node);
      let sumLeft =
        switch (leftGet(node)) {
        | None => 0.0
        | Some(left) => sumGet(left)
        };
      if (sumLeft > offset) {
        firstVisibleNode(leftGet(node), offset);
      } else if (sumLeft +. nodeHeight > offset) {
        Some(node);
      } else {
        firstVisibleNode(rightGet(node), offset -. (sumLeft +. nodeHeight));
      };
    }
  };
let lastVisibleNode = (node, offset) =>
  switch (firstVisibleNode(node, offset)) {
  | None => maxNode(node)
  | first => first
  };
let firstVisible = (rbt, ~offset) =>
  switch (firstVisibleNode(rootGet(rbt), offset)) {
  | None => None
  | Some(node) => Some(valueGet(node))
  };
let rec leftmost = node =>
  switch (leftGet(node)) {
  | None => node
  | Some(node) => leftmost(node)
  };
let rec firstRightParent = node =>
  switch (parentGet(node)) {
  | None => None
  | Some(parent) =>
    if (isLeft(node)) {
      Some(parent);
    } else {
      firstRightParent(parent);
    }
  };
let nextNode = node =>
  switch (rightGet(node)) {
  | None => firstRightParent(node)
  | Some(right) => Some(leftmost(right))
  };
let rec sumLeftSpine = (node, ~fromRightChild) => {
  let leftSpine =
    switch (leftGet(node)) {
    | None => heightGet(node)
    | Some(left) =>
      if (fromRightChild) {
        heightGet(node) +. sumGet(left);
      } else {
        0.0;
      }
    };
  switch (parentGet(node)) {
  | None => leftSpine
  | Some(parent) =>
    leftSpine
    +. sumLeftSpine(parent, ~fromRightChild=rightGet(parent) === Some(node))
  };
};
let getY = node =>
  sumLeftSpine(node, ~fromRightChild=true) -. heightGet(node);
let linearSearch = (rbt, callback) => {
  let rec find = (node, callback) =>
    if (Js.Internal.fn_run1(callback, valueGet(node))) {
      Some(valueGet(node));
    } else {
      switch (nextNode(node)) {
      | None => None
      | Some(node) => find(node, callback)
      };
    };
  switch (minNode(rootGet(rbt))) {
  | None => None
  | Some(node) => find(node, callback)
  };
};
let rec iterate = (~inclusive, firstNode, lastNode, ~callback) =>
  switch (firstNode) {
  | None => ()
  | Some(node) =>
    if (inclusive) {
      Js.Internal.fn_run1(callback, node);
    };
    if (firstNode !== lastNode) {
      if (!inclusive) {
        Js.Internal.fn_run1(callback, node);
      };
      iterate(~inclusive, nextNode(node), lastNode, ~callback);
    };
  };
let rec iterateWithY = (~y=?, ~inclusive, firstNode, lastNode, ~callback) =>
  switch (firstNode) {
  | None => ()
  | Some(node) =>
    let y =
      switch (y) {
      | None => getY(node)
      | Some(y) => y
      };
    if (inclusive) {
      Js.Internal.fn_run2(callback, node, y);
    };
    if (firstNode !== lastNode) {
      if (!inclusive) {
        Js.Internal.fn_run2(callback, node, y);
      };
      iterateWithY(
        ~y=y +. heightGet(node),
        ~inclusive,
        nextNode(node),
        lastNode,
        ~callback,
      );
    };
  };
let rec updateSum = (node, ~delta) =>
  switch (node) {
  | None => ()
  | Some(node) =>
    sumSet(node, sumGet(node) +. delta);
    updateSum(parentGet(node), ~delta);
  };
let setHeight = (rbt, value, ~height) =>
  switch (_findNode(rbt, rootGet(rbt), value)) {
  | None => ()
  | Some(node) =>
    let delta = height -. heightGet(node);
    heightSet(node, height);
    updateSum(Some(node), ~delta);
  };
type nonrec oldNewVisibleNodes('value) = {
  mutable old: array('value),
  mutable new_: array('value),
};
