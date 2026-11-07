type rec t<'a> = 'a constraint 'a = int
and bad = t<string>
