let make: (unit => unit) => GetterProducer.t = %raw(`touch => {
  let reads = 0;
  return {value: 1, get kind() {
    touch();
    reads += 1;
    return reads === 1 ? "b" : "a";
  }};
}`)

let reads = ref(0)
let value = make(() => reads.contents = reads.contents + 1)
let first = GetterProducer.choose(value)
let firstReads = reads.contents
let second = GetterProducer.choose(value)
let totalReads = reads.contents

let destructureReads = ref(0)
let destructureEffects = ref(0)
let destructureAnswer = GetterProducer.bodyDestructure(
  make(() => destructureReads.contents = destructureReads.contents + 1),
  () => destructureEffects.contents = destructureEffects.contents + 1,
)

let nestedReads = ref(0)
let nestedEffects = ref(0)
let nestedAnswer = GetterProducer.bodyMatch(
  make(() => nestedReads.contents = nestedReads.contents + 1),
  () => nestedEffects.contents = nestedEffects.contents + 1,
)
