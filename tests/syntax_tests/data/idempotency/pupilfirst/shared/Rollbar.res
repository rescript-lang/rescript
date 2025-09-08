/* Rollbar.critical("Connection error from remote Payments API"); */
@val @scope("Rollbar") external critical: string => unit = "critical"

/* Rollbar.error("Some unexpected condition"); */
@val @scope("Rollbar") external error: string => unit = "error"

/* Rollbar.warning("Connection error from Twitter API"); */
@val @scope("Rollbar") external warning: string => unit = "warning"

/* Rollbar.info("User opened the purchase dialog"); */
@val @scope("Rollbar") external info: string => unit = "info"

/* Rollbar.debug("Purchase dialog finished rendering"); */
@val @scope("Rollbar") external debug: string => unit = "debug"
