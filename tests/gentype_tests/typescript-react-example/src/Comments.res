/** A module for deciding on a subject matter for a poem.*/
@genType
module DecideSubject = {
  type payload = {
    /** A hint to use as a guide when thinking of your poem.*/
    hint: string,
  }
  /** The input used to generate the prompt and system prompt.*/
  type input
  /** The output from evaluating the llm prompt*/
  type output = {
    /** The text of the poem.*/
    text: string,
    /** The prompt used to generate the poem.*/
    prompt: string,
    /** The system prompt used to generate the poem.*/
    systemPrompt: string,
  }

  /** Decide on a subject matter for a poem.*/
  @genType
  let _placeholder = (
    @ocaml.doc("The runner specification") run: string,
    @ocaml.doc("The number of times to cycle through the runner") times: int,
  ) => (run, times)->ignore
}
