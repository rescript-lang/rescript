@unboxed type absent = | @as(undefined) Absent
@unboxed type t = Unit({kind: unit, value: int}) | Missing({kind: absent, value: int})
