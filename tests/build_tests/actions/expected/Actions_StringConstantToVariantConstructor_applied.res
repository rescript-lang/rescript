type status = Active | Inactive | Pending

let processStatus = (s: status) => {
  switch s {
  | Active => "active"
  | Inactive => "inactive"
  | Pending => "pending"
  }
}

let result = processStatus(Active)

/* === AVAILABLE ACTIONS:
- ReplaceWithVariantConstructor(Active) - Replace with variant constructor Active
*/
