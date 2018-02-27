#!/bin/sh

if test "$1" = ""
then echo "Please enter input GFF file name (e.g., Ce10_oris.ech2.40a.40b.40c.3rep.gff)."
     read f
else f=$1
fi

echo ""
echo ""
echo " *** Now working on "$f" ***"
name=`echo $f | sed -e 's|Ce10_oris\.||' -e 's|\.gff$||'`
nb_real=`cat $f | wc -l`
echo "Date: "`date`
./Module_negative_control_generator.pl $f Chromosome_organization.dat
for file in `ls Random_set_of_boxes_[0-9]*.dat`
do if test `cat $file | wc -l` -ne $nb_real
   then rm -f $file
   else new=`echo $file | sed 's|Random_set_of_boxes_|Random_set_of_boxes_'$name'_|'`
        mv $file $new
   fi
done
