#! /bin/sh

cd /Users/thomasjensen/Documents/Thesis/
DATE=`date`
git add .
git commit -m "$DATE"
git push origin master
