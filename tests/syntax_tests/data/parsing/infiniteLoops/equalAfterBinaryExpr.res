let rec _addLoop = (rbt, currentNode) => {
  // Case 1: node is root. Violates 1. Paint it black.
  if Some(currentNode) === rbt->root {
    currentNode.color = Black
  }

  // Case 2: parent black. No properties violated. After that, parent is sure
  // to be red.
  else if (currentNode.parent->castNotOption).color === Black {
    ()
  }

  // Case 3: if node's parent and uncle are red, they are painted black.
  // Their parent (node's grandparent) should be painted red, and the
  // grandparent red. Note that node certainly has a grandparent, since at
  // this point, its parent's red, which can't be the root.

  // After the painting, the grandparent might violate 2 or 4.
  else if({
      let uncle = uncleOf(currentNode)
      uncle !== None && (uncle->castNotOption).color === Red
    }) {
    (currentNode.parent->castNotOption).color = Black
    (uncleOf(currentNode)->castNotOption).color = Black
    (grandParentOf(currentNode)->castNotOption).color = Red
    _addLoop(rbt, grandParentOf(currentNode)->castNotOption)
  }
  else {
    // At this point, uncle is either black or doesn't exist.

    // Case 4: parent red, uncle black, node is right child, parent is left
    // child. Do a left rotation. Then, former parent passes through case 5.
    let currentNode =
      if !isLeft(currentNode) && isLeft(currentNode.parent->castNotOption) {
        rotateLeft(rbt, currentNode.parent->castNotOption)
        currentNode.left->castNotOption
      } else if isLeft(currentNode) && !isLeft(currentNode.parent->castNotOption) {
        rotateRight(rbt, currentNode.parent->castNotOption)
        currentNode.right->castNotOption
      } else {
        currentNode
      }

    // Case 5: parent red, uncle black, node is left child, parent is left
    // child. Right rotation. Switch parent and grandparent's color.
    (currentNode.parent->castNotOption).color = Black
    (grandParentOf(currentNode)->castNotOption).color = Red
    if isLeft(currentNode) {
      rotateRight(rbt, grandParentOf(currentNode)->castNotOption)
    } else {
      rotateLeft(rbt, grandParentOf(currentNode)->castNotOption)
    }
  }
}


let removeNode = (rbt, node) => {
  if nodeToRemove.color === Black {
    if successor.color === Red {
      successor.color = Black
      if successor.parent === None {
        rbt->rootSet(Some(successor))
      }
    } else {
      let break = ref(false)
      let successorRef = ref(successor)
      while !break.contents {
        let successor = successorRef.contents
        // Case 1: node is root. Done.
        switch successor.parent {
        | None =>
          rbt->rootSet(Some(successor))
          break.contents = true
        | Some(successorParent) =>
          // Case 2: sibling red. Flip color of P and S. Left rotate P.
          let sibling = siblingOf(successor)
          if sibling !== None && (sibling->castNotOption).color === Red {
            successorParent.color = Red
            (sibling->castNotOption).color = Black
            if isLeft(successor) {
              rotateLeft(rbt, successorParent)
            } else {
              rotateRight(rbt, successorParent)
            }
          }

          // Case 3: parent, sibling and sibling children all black. Paint
          // sibling red. Rebalance parent.
          let sibling = siblingOf(successor)
          let siblingNN = sibling->castNotOption
          if
            successorParent.color === Black &&
            ( sibling === None ||
              ( siblingNN.color === Black &&
                ( siblingNN.left === None ||
                  (siblingNN.left->castNotOption).color === Black ) &&
                ( siblingNN.right === None ||
                  (siblingNN.right->castNotOption).color === Black)))
             {
            if sibling !== None {
              siblingNN.color = Red
            }
            successorRef.contents = successorParent
            // continue
          } else if
            // Case 4: sibling and sibling children black. Node parent red. Swap
            // color of sibling and node parent.
            successorParent.color === Red &&
            ( sibling === None ||
              ( siblingNN.color === Black &&
              ( siblingNN.left === None ||
                (siblingNN.left->castNotOption).color === Black) &&
              ( siblingNN.right === None ||
                (siblingNN.right->castNotOption).color === Black)))
             {
            if sibling !== None {
              siblingNN.color = Red
            }
            successorParent.color = Black
            break.contents = true
          } else if
            // Case 5: sibling black, sibling left child red, right child black,
            // node is left child. Rotate right sibling. Swap color of sibling and
            // its new parent.
            sibling !== None && (sibling->castNotOption).color === Black
             {
            let sibling = sibling->castNotOption
            if
              isLeft(successor) &&
              (sibling.right === None || (sibling.right->castNotOption).color === Black) &&
              sibling.left !== None &&
              (sibling.left->castNotOption).color === Red {
              sibling.color = Red
              (sibling.left->castNotOption).color = Black
              rotateRight(rbt, sibling)
            } else if
              !isLeft(successor) &&
              (sibling.left === None || (sibling.left->castNotOption).color === Black) &&
              sibling.right !== None &&
              (sibling.right->castNotOption).color === Red
               {
              sibling.color = Red
              (sibling.right->castNotOption).color = Black
              rotateLeft(rbt, sibling)
            }
            break.contents = true
          } else {
            // Case 6: sibling black, sibling right child red, node is left child.
            // Rotate left node parent. Swap color of parent and sibling. Paint
            // sibling right child black.
            let sibling = siblingOf(successor)
            let sibling = sibling->castNotOption
            sibling.color = successorParent.color
            if isLeft(successor) {
              (sibling.right->castNotOption).color = Black
              rotateRight(rbt, successorParent)
            } else {
              (sibling.left->castNotOption).color = Black
              rotateLeft(rbt, successorParent)
            }
          }
        }
      }
    }
  }
  // Don't forget to detatch the artificially created leaf.
  if isLeaf(. successor) {
    if rbt->root === Some(successor) {
      rbt->root = None
    }
  }
}
