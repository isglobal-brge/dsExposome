#!/bin/bash

AGENT_USER_HOMEDIRECTORY=$(echo "${AGENT_HOMEDIRECTORY}" | cut -d/ -f 1-3)
echo "Create user libraries directory R [ ${R_LIBS_USER} ]"
mkdir -p "${R_LIBS_USER}"

Rscript -e "install.packages(c('git2r', 'covr', 'withr', 'devtools', 'lintr', 'mockery', 'pkgdown', 'wkb'), repos='https://cloud.r-project.org', lib='${R_LIBS_USER}')"
Rscript -e "install.packages(c('DSI'), repos='https://cloud.r-project.org', lib='${R_LIBS_USER}')"
Rscript -e "install.packages(c('BiocManager'), repos='https://cloud.r-project.org', lib='${R_LIBS_USER}')"
Rscript -e "BiocManager::install('Biobase', lib='${R_LIBS_USER}')"
Rscript -e "BiocManager::install('rexposome', lib='${R_LIBS_USER}')"
Rscript -e "withr::with_libpaths(new = '${R_LIBS_USER}', devtools::install_github('datashield/dsBase', ref='6.1.1'))"
cd "${BUILD_REPOSITORY_LOCALPATH}"