#!/bin/bash

if [ ! -d ./output ]; then
    echo "Creating output directory."
    mkdir output
fi

if [ "$(ls -A ./output)" ]; then
    archiveDir="output_$(date  +'%Y%m%dT%H%M.%S')"
    echo "Archiving old output folder \"./output\" to \"./${archiveDir}\""
    mv output ${archiveDir}
    mkdir output
fi

Rscript expectancy.R
