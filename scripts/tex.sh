#!/bin/bash

function view_pdflatex
{
    if [[ $1 == *.tex ]]; then
	BASE="${1%.tex}"
    else
	BASE=$1
    fi
    
    TEX=$BASE".tex"
    PDF=$BASE".pdf"
    
    echo "Base: " $BASE
    echo "TeX: " $TEX
    echo "pdf: " $PDF
    
    pdflatex $TEX && biber $BASE && pdflatex $TEX && open $PDF
}
