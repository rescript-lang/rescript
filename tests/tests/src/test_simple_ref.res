include (
  {
    let v = ref(0)

    let gen = () => {
      incr(v)
      v.contents
    }
    let h = ref(0)

    let a = ref(0)
    let c = ref(0)
    let escape = v => v

    let not_real_escape = {
      let b = a
      b.contents
    }
    let real_escape = (f, v) => {
      let b = c
      f(b)
    }

    let u = escape(h)
  }: {
    let u: ref<int>
    let gen: unit => int
    let not_real_escape: int
    let real_escape: (ref<int> => unit, int) => unit
  }
)
