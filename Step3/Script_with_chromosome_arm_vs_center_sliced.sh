#/bin/sh

wig=$1

#display_date=`date +%s`
   #rm Chunk_*
   display_wig_file=`echo $wig | sed 's|.*/||'`
   #date=`date +%s`
   #display_date=`echo $display_date $date`
   ./Module_finds_chopping_sites.pl $wig
   #date=`date +%s`
   #display_date=`echo $display_date $date`
   for ORI_file in `ls /mnt/data/home/herve.seitz/Analyses_for_paper_July2015/Maximum_probe_coordinates/maximum_probes_coordinates_* | grep -v coordinates_oris`    
   do display_ORI_file=`echo $ORI_file | sed 's|.*/||'`
      for f in `ls Chunk_*`
      do for region in autosome_arm autosome_center X_arm X_center
         do ./Module_peaks_vs_ChIP_arm_vs_center.pl $ORI_file $f $region Chromosome_organization.dat
         done
      done
      #date=`date +%s`
      #display_date=`echo $display_date $date`

      for region in autosome_arm autosome_center X_arm X_center
      do for type in middle #limits
         do representative_file=`wc -l ChIP_vs_ORI_$type'_'$display_ORI_file'_'$region'_on_Chunk_'[0-9]*'_from_'$display_wig_file'.txt' | grep -m 1 -v '^ *1 ' | awk '{print $2}'`
            for f in `ls ChIP_vs_ORI_$type'_'$display_ORI_file'_'$region'_on_Chunk_'[0-9]*'_from_'$display_wig_file'.txt'`
            do sort -g $f > tmp_$f
               mv tmp_$f $f
            done
            nb_data_lines=`tail -n +2 $representative_file | wc -l`
            echo "#Distance Signal_mean Signal_sd Signal_se Number_of_contributing_ORIs" > Stats_$region'_'$type'_'$display_ORI_file'_on_'$display_wig_file'.dat'
            for i in `seq 1 $nb_data_lines`
            do d=`tail -n +2 $representative_file | head -$i | tail -1 | sed 's| .*||'`
               for f in `ls ChIP_vs_ORI_$type'_'$display_ORI_file'_'$region'_on_Chunk_'[0-9]*'_from_'$display_wig_file'.txt'`
               do tail -n +2 $f | head -$i | tail -1 | sed 's|^[0-9-]* *||'
               done > tmp_$d
               out=`./Module_stats_chopped_input.pl tmp_$d`
               echo $d $out >> Stats_$region'_'$type'_'$display_ORI_file'_on_'$display_wig_file'.dat'
            done
            bzip2 ChIP_vs_ORI_$type'_'$display_ORI_file'_'$region'_on_Chunk_'[0-9]*'_from_'$display_wig_file'.txt'
            mv ChIP_vs_ORI_$type'_'$display_ORI_file'_'$region'_on_Chunk_'[0-9]*'_from_'$display_wig_file'.txt.bz2' ChIP_files/
            cp Stats_$region'_'$type'_'$display_ORI_file'_on_'$display_wig_file'.dat' /mnt/data/home/herve.seitz/Analyses_for_paper_July2015/Comparison_to_ChIP/Results/
         done

         #date=`date +%s`
         #display_date=`echo $display_date $date`
         #date=`date +%s`
         #display_date=`echo $display_date $date`
      done
   done
#   echo $display_wig_file $display_date >> chrono.dat
   echo "***"
   echo "Done with "$display_wig_file"."
   mv Chunk_* Chunk_backup/
   cd /mnt/data/home/herve.seitz/Analyses_for_paper_July2015/Comparison_to_ChIP/Results
   for region in autosome_arm autosome_center X_arm X_center
   do for stage in '2-40_cells' 'Mix_only'
      do Rscript R_commands_display_comparison_to_ChIP $region $stage $display_wig_file
      done
   done
   cd ..
