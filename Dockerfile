#####################
FROM node:18.16.0 AS builder
WORKDIR /
COPY package.json /
COPY package-lock.json /
RUN npm ci
COPY elm.json /
COPY src/ src/
COPY tests/ tests/
RUN mkdir /elm && \
    echo "module Dummy exposing (..)\n\nimport DateRangePicker exposing (..)\n\ntype Model = Int\n" > /elm/Dummy.elm && \
    ./node_modules/elm/bin/elm make --output=/dev/null /elm/Dummy.elm
RUN ./node_modules/.bin/elm-test --compiler=./node_modules/.bin/elm