@as("User")
type user = {
  @as("user_name") name: string,
  @as("user_age") age: int,
}

type status = Active | Inactive | @as("pending_review") PendingReview

let defaultUser: user = {name: "Anonymous", age: 0}
