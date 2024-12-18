#!/bin/sh

js="public/elm.js"
min="public/elm.min.js"

elm make --optimize --output=$js "src/Main.elm"