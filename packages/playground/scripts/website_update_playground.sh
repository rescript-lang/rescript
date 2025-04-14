#!/usr/bin/env bash

if [ -z "${NEXT_REVALIDATE_SECRET_TOKEN}" ]; then
  echo "NEXT_REVALIDATE_SECRET_TOKEN environment variable not set."
  exit 1
fi

echo "Sending request to rescript-lang.org/api/revalidate"

STATUS_CODE=$(curl -s -o /dev/null -w "%{http_code}" "https://rescript-lang.org/api/revalidate?secret=${NEXT_REVALIDATE_SECRET_TOKEN}")

if [[ "$STATUS_CODE" == 200 ]]; then
  echo "Revalidation finished"
  exit 0
fi

echo "Failed to revalidate"
echo "Status Code: $STATUS_CODE"
exit 1
