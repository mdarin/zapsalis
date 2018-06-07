#!/bin/bash
# v.1.0
# генерирование скрипта для заполнения формы счёта
# заполнение формы
# генерирования файла счёта в формате pdf



#TODO(darin-m): добавить проверки!

echo "script: $0"
echo "workdir: $(dirname $0)" 			# винтажный вариант `dirname $0`
echo "scriptname: $(basename $0)" 	# винтажный вариант `basename $0`



# Проверяемая переменная заключена в двойные кавычки.
INVOICE="invoice"
if [ -n "$1" ]              
then
	INVOICE=$1
	shift
	echo "invoice: $INVOICE"
else
	echo "invoice: default $INVOICE"     
fi

# Добавит путь к имени файла (см. 'basename')
FIELDVALUES=""
if [ -n "$1" ]              
then
	FIELDVALUES=$1
	shift
	#echo "firstvals: $INVOICE"
fi
until [ -z "$1" ]  # Пока все параметры не будут разобраны
do
  #echo -n "$1 "
	#echo "$FIELDVALUES"
	FIELDVALUES="$FIELDVALUES,$1"
  shift
done
echo "fiedvals: $FIELDVALUES"
 

cd ".."
ROOT=$(pwd) # аналогично `pwd` более модерновая версия
echo "working root: $ROOT"
cd "./bin"

TEMPLATES="$ROOT/templates"
echo "templates: $TEMPLATES"

SCRIPTS="$ROOT/perl_src"
echo "scripts: $SCRIPTS"

RESULT="$ROOT/result"
echo "result: $RESULT"


echo "creating $INVOICE..."

echo "filling fields and generating perl filling script"
pdftk "$TEMPLATES/blank.pdf" dump_data_fields | perl "$SCRIPTS/fields2pl.pl" "--values=$FIELDVALUES" >"$RESULT/$INVOICE.pl"

echo "createing filledin template (FDF) file"
perl "$SCRIPTS/genfdf.pl" "$RESULT/$INVOICE.pl" >"$RESULT/$INVOICE.fdf"

echo "generateing result filledin (PDF) file"
pdftk "$TEMPLATES/blank.pdf" fill_form "$RESULT/$INVOICE.fdf" output "$RESULT/$INVOICE.pdf"

echo "cleanig up..."

#cd "$RESULT"
#rm *.fdf *.pl
#cd "$ROOT"

exit 0
