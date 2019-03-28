#####################
FROM node:10.15.0 AS builder
WORKDIR /
COPY package.json /
COPY package-lock.json /
RUN npm ci
COPY elm.json /
COPY src/ src/
COPY tests/ tests/
RUN mkdir /src/elm && \
    echo "module Dummy exposing (..)\n\nimport DateRangePicker exposing (..)\nimport DatePicker exposing (..)\nimport DateRangePicker.Common.Internal exposing (..)\nimport DateRangePicker.Common exposing (..)\n\ntype Model = Int\n" > /src/elm/dummy.elm && \
    ./node_modules/elm/bin/elm make --output=/dev/null /src/elm/dummy.elm
RUN ./node_modules/.bin/elm-test --compiler=./node_modules/.bin/elm