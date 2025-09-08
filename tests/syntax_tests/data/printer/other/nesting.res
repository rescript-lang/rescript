let unitsCommands = state.units->Js.Array2.mapi(({
  unit: targetUnit,
  coordinates: targetCoordinates,
}, i) => {
  // n^2
  let res = []
  state.units->Js.Array2.forEachi(({
    unit: unitThatMightBeAttacking,
    coordinates: unitThatMightBeAttackingCoordinates,
  }, j) => {
    if i !== j {
      switch Js.Array2.unsafe_get(
        unitThatMightBeAttacking.timeline,
        unitThatMightBeAttacking.currentFrame,
      ).effect {
      | Some(UnitAttack({damage, hitBox: _})) =>
        let unitThatMightBeAttackingHitBox_ = Unit.hitBox(unitThatMightBeAttacking)
        let unitThatMightBeAttackingHitBox = {
          ...unitThatMightBeAttackingHitBox_,
          x: unitThatMightBeAttackingCoordinates.x +. unitThatMightBeAttackingHitBox_.x,
          y: unitThatMightBeAttackingCoordinates.y +. unitThatMightBeAttackingHitBox_.y,
        }
        let targetUnitHitBox_ = Unit.hitBox(targetUnit)
        let targetUnitHitBox = {
          ...targetUnitHitBox_,
          x: targetCoordinates.x +. targetUnitHitBox_.x,
          y: targetCoordinates.y +. targetUnitHitBox_.y,
        }
        let hit = hitTest(unitThatMightBeAttackingHitBox, targetUnitHitBox)

        if hit {
          // TODO: it's wrong to put this here. We don't know whether wizard resisted the attack or not
          let sparksX = targetUnitHitBox.x +. targetUnitHitBox.width /. 2.
          let sparksY = targetUnitHitBox.y +. targetUnitHitBox.height /. 2.
          let spark = {
            unit: Spark.make(
              ~spriteSheet=assets.spark,
              ~orientation=targetUnit.orientation,
              ~aspectRatio=1.,
              ~anchor=Middle,
            ),
            coordinates: {x: sparksX, y: sparksY, z: 0.},
          }
          particlesToAdd->Js.Array2.push(spark)->ignore

          res->Js.Array2.push(Unit.CommandAttacked({damage: damage}))->ignore
        }
      | _ => ()
      }
    }
  })
  res
})
