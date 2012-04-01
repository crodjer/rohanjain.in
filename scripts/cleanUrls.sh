#!/usr/bin/env bash

############################################################
# This converts the urls with trailing .html generated by
# static site generators to clean the dirctory/index.html
# based
############################################################
SITE_DIR="_site/"
HOST="www.rohanjain.in"

cd $SITE_DIR

if [ $? ]; then

    HTML_PAGES=$(find . -iname "*.html" | sed 's/^\.\///' | sed 's/.html$//')
    XML_PAGES=$(find . -iname "*.xml" | sed 's/^\.\///' | sed 's/.xml$//')

    SED_CHAIN=""
    ABS_SED_CHAIN=""


    for page in $HTML_PAGES
    do
        if [ `echo "$page" | sed 's/index$//'` ]; then
            SED_CHAIN="$SED_CHAIN | sed 's:/$page.html:/$page/:g'"
            ABS_SED_CHAIN="$ABS_SED_CHAIN | sed 's:$HOST/$page.html:$HOST/$page/:g'"
        fi
    done

    echo "$SED_CHAIN"
    echo "$ABS_SED_CHAIN"

    for page in $HTML_PAGES
    do
        echo "Updating urls in $page"
        bash -c "cat $page.html $SED_CHAIN > $page.html"

        if [ `echo "$page" | sed 's/index$//'` ]; then
            echo "Moving $page"
            mkdir -p "$page"
            mv "$page.html" "$page/index.html"
        fi

    done

    for page in $XML_PAGES
    do
        echo "Updating urls in $page"
        bash -c "cat $page.xml $ABS_SED_CHAIN > $page.xml"
    done
fi
