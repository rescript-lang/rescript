type id = string

type t = {
  id: id,
  coachId: string,
  submissionId: string,
  feedback: string,
}

let id = t => t.id
let coachId = t => t.coachId
let submissionId = t => t.submissionId
let feedback = t => t.feedback

let decode = json => {
  open Json.Decode
  {
    id: json->field("id", string),
    coachId: json->field("coachId", string),
    submissionId: json->field("submissionId", string),
    feedback: json->field("feedback", string),
  }
}
