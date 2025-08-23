#!/bin/bash

SETTINGS_FILE=".vscode/settings.json"

mkdir -p .vscode
if [ ! -f "$SETTINGS_FILE" ]; then
  echo '{}' > "$SETTINGS_FILE"
fi


read -p "Enter the word to add to cSpell.words: " new_word


if ! command -v jq &> /dev/null; then
  echo "jq is required but not installed. Please install jq first."
  exit 1
fi

jq_exists=$(jq 'has("cSpell.words")' "$SETTINGS_FILE")
if [ "$jq_exists" = "false" ]; then

  jq --arg word "$new_word" '. + {"cSpell.words": [$word]}' "$SETTINGS_FILE" > tmp.$$.json && mv tmp.$$.json "$SETTINGS_FILE"
  echo "Added $new_word to cSpell.words"
  exit 0
fi


word_exists=$(jq --arg word "$new_word" '.["cSpell.words"] | index($word)' "$SETTINGS_FILE")

if [ "$word_exists" != "null" ]; then
  echo "The word '$new_word' already exists in cSpell.words."
  exit 0
fi


jq --arg word "$new_word" '.["cSpell.words"] += [$word]' "$SETTINGS_FILE" > tmp.$$.json && mv tmp.$$.json "$SETTINGS_FILE"
echo "Added $new_word to cSpell.words."
