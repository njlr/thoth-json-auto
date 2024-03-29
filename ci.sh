#! /bin/sh

set -e

dotnet tool restore
dotnet restore

yarn install --immutable --immutable-cache --check-cache

dotnet build -c Release

dotnet run --project ./thoth.json.auto.tests

(cd ./thoth.json.auto.tests && yarn build && yarn test)
