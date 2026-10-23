type resourceType =
  | Target
  | TargetGroup

module SortResourceMutation = %graphql(`
   mutation SortCurriculumResourcesMutation($resourceIds: [ID!]!, $resourceType: String!) {
    sortCurriculumResources(resourceIds: $resourceIds, resourceType: $resourceType){
      success
    }
  }
   `)

let resourceTypeToString = resourceType =>
  switch resourceType {
  | Target => "Target"
  | TargetGroup => "TargetGroup"
  }

let sort = (resourceType, resourceIds) =>
  SortResourceMutation.make(~resourceIds, ~resourceType=resourceTypeToString(resourceType), ())
  ->GraphqlQuery.sendQuery(~notify=false)
  ->Promise.then(_response => Promise.resolve())
  ->ignore
