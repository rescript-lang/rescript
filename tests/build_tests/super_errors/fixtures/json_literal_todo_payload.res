/* Known bug: json literals are only meaningful in external attributes, but
 this is currently treated as a regular todo payload. */
let value = %todo(json`message`)
