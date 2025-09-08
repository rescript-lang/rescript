module A = (ParenthesizedModule)
module A = ((DoubleParenthesizedModule))
module A = @attr (ParenthesizedModule)

module F = ((A: X) => A)

module A = ({ let a = 1 })
module A = (ModApply(MyMod))
module A = ((A: X))

include (ParenthesizedModule)
include @attr (ParenthesizedModule)

module A = @attr (@attr2 ParenthesizedModule)
