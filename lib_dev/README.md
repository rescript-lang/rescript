# Libraries for development

This is a place for development purposes libraries.

You can use modules here by `#dev/*`

e.g. in `scripts` or `tests`:

```js
import { setup } from '#dev/process';

const { execBuild } = setup(import.meta.url);

// Execute ReScript in the current file location.
await execBuild({ stdio: "inherit" });
```

> [!IMPORTANT]  
> DO NOT USE this modules in the compiler artifacts.
