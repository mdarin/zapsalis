#!/bin/bash
# тестовый скрипт 
# поля заполняются своими именами
cd ".."
ROOT=$(pwd)
echo "working root: $ROOT"
cd "./test"

TEMPLATES="$ROOT/templates"
echo "templates: $TEMPLATES"

TEST="$ROOT/test"
echo "test: $TEST"

SCRIPTS="$ROOT/perl_src"
echo "scripts: $SCRIPTS"

echo "creating invoice..."

echo "filling fields and generating perl filling script"
pdftk "$TEMPLATES/blank.pdf" dump_data_fields | perl "$SCRIPTS/fields2pl.pl" > "$TEST/foo.pl"

echo "createing filledin template (FDF) file"
perl "$SCRIPTS/genfdf.pl" "$TEST/foo.pl" >"$TEST/foo.fdf"

echo "generateing result filledin (PDF) file"
pdftk "$TEMPLATES/blank.pdf" fill_form "$TEST/foo.fdf" output "$TEST/filled.pdf"

echo "clinning up..."
cd $TEST
rm *.fdf *.pl
cd $ROOT


