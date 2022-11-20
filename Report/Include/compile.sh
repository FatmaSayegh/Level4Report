#!/bin/bash
for i in *.tex;
do
   mdfileName=$(echo $i | cut -d"." -f1).md;
   mdfileName=Markdown/$mdfileName
   echo $mdfileName
   pandoc -f latex -t markdown $i > "$mdfileName"
done
