#!/bin/bash

cd ..
NoMatrix_HOME=`pwd`
cd -

cd ${NoMatrix_HOME}/R

Rscript NoMatrix.R

cd -
