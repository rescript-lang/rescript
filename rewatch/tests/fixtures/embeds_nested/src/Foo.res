let b = foo((::sql.one("/* @name A */ select 1")), (::sql.one("/* @name B */ select 2")))
