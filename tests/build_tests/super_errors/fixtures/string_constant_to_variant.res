type status = Active | Inactive | Pending

let processStatus = (s: status) => {
  switch s {
  | Active => "active"
  | Inactive => "inactive"
  | Pending => "pending"
  }
}

let result = processStatus("Active")
