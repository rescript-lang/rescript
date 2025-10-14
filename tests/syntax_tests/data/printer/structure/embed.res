/* Structure-level embeds in module expressions and include */
module M = ::sql.one("/* @name M */ select 1")

include ::sql.one("/* @name I */ select 1")

