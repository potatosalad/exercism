#!/usr/bin/env bash

set -eo pipefail
set -x

EXERCISE="$1"

if [[ -z "${EXERCISE}" ]]; then
  echo "Usage: ./download.sh EXERCISE" >&2
  exit 1
fi

exercism download --exercise="${EXERCISE}" --track=kotlin
cd "${EXERCISE}"
rm -rf gradle*
ln -svf ../.gradle/gradlew
ln -svf ../.gradle/gradlew.bat
ln -svf ../.gradle/gradle

git add .
git commit -S -m "Add kotlin/${EXERCISE}"

exit 0

