#!/bin/bash

# This script will publish the compiler.js bundle / packages cmij.js files to our KeyCDN server.
# The target folder on KeyCDN will be the compiler.js' version number.
# This script requires `curl` / `openssl` to be installed.

SCRIPT_DIR=$(cd "$(dirname "$0")"; pwd -P)

# Get the actual version from the compiled playground bundle
VERSION=$(cd $SCRIPT_DIR; node -e 'require("./compiler.js"); console.log(rescript_compiler.make().rescript.version)')

if [ -z "${KEYCDN_USER}" ]; then
  echo "KEYCDN_USER environment variable not set. Make sure to set the environment accordingly."
  exit 1
fi

if [ -z "${KEYCDN_PASSWORD}" ]; then
  echo "KEYCDN_PASSWORD environment variable not set. Make sure to set the environment accordingly."
  exit 1
fi

KEYCDN_SRV="ftp.keycdn.com"
NETRC_FILE="${SCRIPT_DIR}/.netrc"

# To make sure to not leak any secrets in the bash history, we create a NETRC_FILE
# with the credentials provided via ENV variables.
if [ ! -f "${NETRC_FILE}" ]; then
  echo "No .netrc file found. Creating file '${NETRC_FILE}'"
  echo "machine ${KEYCDN_SRV} login $KEYCDN_USER password $KEYCDN_PASSWORD" > "${NETRC_FILE}"
fi

PACKAGES=("compiler-builtins" "@rescript/react")

echo "Uploading compiler.js file..."
curl --ftp-create-dirs -T "${SCRIPT_DIR}/compiler.js" --ssl --tls-max 1.2 --netrc-file $NETRC_FILE ftp://${KEYCDN_SRV}/v${VERSION}/compiler.js

echo "---"
echo "Uploading packages cmij files..."
for dir in ${PACKAGES[@]};
do
  SOURCE="${SCRIPT_DIR}/packages/${dir}"
  TARGET="ftp://${KEYCDN_SRV}/v${VERSION}/${dir}"

  echo "Uploading '$SOURCE/cmij.js' to '$TARGET/cmij.js'..."

  curl --ftp-create-dirs -T "${SOURCE}/cmij.js" --ssl --tls-max 1.2 --netrc-file $NETRC_FILE "${TARGET}/cmij.js"
done

# we now upload the bundled stdlib runtime files

DIR="compiler-builtins/stdlib"
SOURCE="${SCRIPT_DIR}/packages/${DIR}"
TARGET="ftp://${KEYCDN_SRV}/v${VERSION}/${DIR}"

echo "Uploading '$SOURCE/*.js' to '$TARGET/*.js'..."

# we use TLS 1.2 because 1.3 sometimes causes data losses (files capped at 16384B)
# https://github.com/curl/curl/issues/6149#issuecomment-1618591420
find "${SOURCE}" -type f -name "*.js" -exec sh -c 'curl --ftp-create-dirs --ssl --tls-max 1.2 --netrc-file "$0" -T "$1" "${2}/$(basename "$1")"' "$NETRC_FILE" {} "$TARGET" \;
