#!/usr/bin/perl

$WINDOW=10000; #Size of the flanks (on each side of the peak) to be analyzed
$AVERAGING=50; #Size of the minimal sequence window on which array signal will be averaged (but if the array span is larger than this, then $AVERAGING will be set to the span value, so that $AVERAGING will always be equal to the poorest resolution among the two array experiments: ORI determination vs. epigenetic mark measurement)

if ($ARGV[3] eq '')
{
    print "Please enter input file names and type of ORIs to analyze (e.g., ./Module_peaks_vs_ChIP_on_chopped_input_averaged.pl ../Highest_peaks/maximum_probes_coordinates_2_40.txt Chunk_18_from_01_H4K20ME1_Ab2_EEMB_NormLis_1.wig autosome_arm Chromosome_organization.dat).\n";
}
else
{
    open(ORGANIZATION,$ARGV[3]);
    while(<ORGANIZATION>)
    {
        chomp;
        if ($_ !~ /^Chromosome,/)
        {
            @array=split(',',$_);
            $chr=$array[0];
            $left_arm_end{$chr}=$array[1];
            $center_end{$chr}=$array[2];
            $right_arm_end{$chr}=$array[3];
        }
    }
    close(ORGANIZATION);


    $file=$ARGV[0];
    $initial_date=`date +%s`;
    chomp $initial_date;
    print "***\nNow loading file $file (initial date: $initial_date).\n";
    $file_name=$file;
    $file_name=~s/.*\///;
    open(DATA,$file);
    while(<DATA>)
    {
	chomp;
	@array=split(' ',$_);
	$select=0;
	if ($ARGV[2] eq 'autosome_arm')
	{
	   if (($array[0] ne 'chrX') && (($array[4]<=$left_arm_end{$array[0]}) || ($array[3]>=$center_end{$array[0]})))
	   {
	      $select=1;
	   }
	}
        if ($ARGV[2] eq 'autosome_center')
        {
           if (($array[0] ne 'chrX') && (($array[4]>$left_arm_end{$array[0]}) && ($array[3]<$center_end{$array[0]})))
           {
              $select=1;
           }
        }
        if ($ARGV[2] eq 'X_arm')
        {
           if (($array[0] eq 'chrX') && ($array[4]<=$left_arm_end{$array[0]}))
           {
              $select=1;
           }
        }
        if ($ARGV[2] eq 'X_center')
        {
           if (($array[0] eq 'chrX') && ($array[4]>$left_arm_end{$array[0]}))  
           {
              $select=1;
           }
        }
	if ($select)
	{
	   push(@{ $ORI_start{$array[0]} },$array[3]); # $array[0] is the ORI's chromosome, $array[3] is the ORI's start
	   push(@{ $ORI_end{$array[0]} },$array[4]); # $array[0] is the ORI's chromosome, $array[4] is the ORI's end
	}
    }
    close(DATA);
    $date=`date +%s`;
    chomp $date;
    $delay=$date-$initial_date;
    print "Done loading $file (t=$delay seconds after initial date).\n***\n\n";

    $signal_file=$ARGV[1];
    $date=`date +%s`;
    chomp $date;
    $delay=$date-$initial_date;
    print "***\nNow loading file $signal_file (t=$delay seconds after initial date)...\n";
    $signal_file_name=$signal_file;
    $signal_file_name=~s/.*\///;
#    open(OUT,">ChIP_vs_ORI_limits_$file_name"."_on_$signal_file_name".".txt");
#    print OUT "Distance_to_ORI_limit Signal\n";
#    close(OUT);
    open(OUT,">ChIP_vs_ORI_middle_$file_name"."_$ARGV[2]"."_on_$signal_file_name".".txt");
    print OUT "Distance_to_ORI_middle Signal\n";
    close(OUT);
    open(WIG,$signal_file);
    while(<WIG>)
    {
	chomp;
	if ($_ !~ /^track type/)
	{
	    if (/Step/)
	    {
		($chr,$span)=($_,$_);
		$chr=~s/.* chrom=(\S*) .*/\1/;
		$span=~s/.* span=//;
		if ($span > $AVERAGING) # Caution: this may not work properly if the span is not constant across the array (it is constant in all the datasets we are using)
		{
		    $AVERAGING=$span;
		}
		@sorted_starts = sort {$a <=> $b} @{ $ORI_start{$chr} };
		@sorted_ends = sort {$a <=> $b} @{ $ORI_end{$chr} }; # N.B.: ORI coordinates in @sorted_starts and in @sorted_ends will still be in the same order, because ORIs don't overlap (but the program will behave erratically if you provide it with overlapping ORI lists)
		$n_ORIs=push(@sorted_starts);
		$first_checked=0; #Index (in the sorted list of ORIs) of the first one to check (the previous ones are too far upstream of that ChIP signal)
#		$chr_initial_date=`date +%s`;
#		chomp $chr_initial_date;
#		$delay=$chr_initial_date-$initial_date;
#		print "   now reading ChIP signals for $chr (t=$delay seconds after initial date)...\n";
#		    for ($bp=20000;$bp<=20000000;$bp+=20000)
#		    {
#			push(@progression_display,$bp);
#		    }
	    }
	    else
	    {
#		    ++$lines_read;
		($x,$y)=($_,$_);
		$x=~s/\s.*//;
		$y=~s/.*\s//;
		
#		    foreach $bp (@progression_display)
#		    {
#			if ($x>=$bp)
#			{
#			    splice(@progression_display,0,1);
#			    $date=`date +%s`;
#			    chomp $date;
#			    $delay=$date-$chr_initial_date;
#			    print "      bp $x reached after $delay seconds in that chromosome\n";
#			}
#		    }
		while (($sorted_ends[$first_checked]+$WINDOW < $x) && ($first_checked < $n_ORIs))
		{
		    ++$first_checked;
		}
		
		$index=$first_checked;
		while (($index<$n_ORIs) && ($sorted_starts[$index]-$WINDOW<=$x+$span))
		{
		    if (($x>=$sorted_starts[$index]-$WINDOW) || ($x+$span<=$sorted_ends[$index]+$WINDOW))
		    {
#			$range_start=$x;
#			$range_end=$x+$span;
#			if ($sorted_starts[$index]-$WINDOW>=$range_start)
#			{
#			    $range_start=$sorted_starts[$index]-$WINDOW;
#			}
#			if ($sorted_starts[$index]+$WINDOW<=$range_end)
#			{
#			    $range_end=$sorted_starts[$index]+$WINDOW;
#			}
#			for ($ChIP_bp=$range_start;$ChIP_bp<=$range_end;++$ChIP_bp)
#			{
#			    $d=$ChIP_bp-$sorted_starts[$index];
#			    $for_averaging_limits{$index}{$d}=$y; #no need to index by chromosome, because the Chunk_* input contains data from a single chromosome
#			}
#			
#			$range_start=$x;
#			$range_end=$x+$span;
#			if ($sorted_ends[$index]-$WINDOW>=$range_start)
#			{
#			    $range_start=$sorted_ends[$index]-$WINDOW;
#			}
#			if ($sorted_ends[$index]+$WINDOW<=$range_end)
#			{
#			    $range_end=$sorted_ends[$index]+$WINDOW;
#			}
#			for ($ChIP_bp=$range_start;$ChIP_bp<=$range_end;++$ChIP_bp)
#			{
#			    $d=$sorted_ends[$index]-$ChIP_bp;
#			    $for_averaging_limits{$index}{$d}=$y; #no need to index by chromosome, because the Chunk_* input contains data from a single chromosome
#			}
			
			$middle=($sorted_starts[$index]+$sorted_ends[$index])/2;
			$middle=int($middle+0.5); # rounding $middle (which is always positive)
			$range_start=$x;
			$range_end=$x+$span;
			if ($middle-$WINDOW>=$range_start)
			{
			    $range_start=$middle-$WINDOW;
			}
			if ($middle+$WINDOW<=$range_end)
			{
			    $range_end=$middle+$WINDOW;
			}
			for ($ChIP_bp=$range_start;$ChIP_bp<=$range_end;++$ChIP_bp)
			{
			    $d=$ChIP_bp-$middle;
			    if ($d<0)
			    {
				$d=-$d;
			    }
			    $for_averaging_middle{$index}[$d]=$y; #no need to index by chromosome, because the Chunk_* input contains data from a single chromosome
			}
		    }
		    ++$index;
		}
	    }
	}
    }
    close(WIG);

### Below: for each ORI (defined by $index), averaging over $AVERAGING bp
#    foreach $index (keys %for_averaging_limits)
#    {
#	%collect=();
##	foreach $d (keys $for_averaging_limits{$index}) # Doesn't work on typhon
#	for ($d=-$WINDOW;$d<=$WINDOW;++$d)
#	{
#	    if ($d >= 0)
#	    {
#		$r=int($d/$AVERAGING);
#	    }
#	    else
#	    {
#		$r=int(($d+1)/$AVERAGING)-1;
#	    }
#	    $r=$r*$AVERAGING+int($AVERAGING/2); # This is the position of the middle of the $AVERAGING bp-long interval where $d fell
#	    $collect{$r}=$collect{$r}.' '.$for_averaging_limits{$index}{$d};
#	}
#	foreach $r (keys %collect)
#	{
#	    $each_ORI_limits{$r}=$each_ORI_limits{$r}.' '.&mean($collect{$r});
#	}
#    }

    foreach $index (keys %for_averaging_middle)
    {
	%collect=();
	for ($d=0;$d<=$WINDOW;++$d)
	{
	    $r=int($d/$AVERAGING);
	    $r=$r*$AVERAGING+int($AVERAGING/2); # This is the position of the middle of the $AVERAGING bp-long interval where $d fell
	    $collect{$r}=$collect{$r}.' '.$for_averaging_middle{$index}[$d];
	}
	foreach $r (keys %collect)
	{
	    $each_ORI_middle{$r}=$each_ORI_middle{$r}.' '.&mean($collect{$r});
	}
    }
### Above: for each ORI (defined by $index), averaging over $AVERAGING bp


#    open(OUT,">>ChIP_vs_ORI_limits_$file_name"."_on_$signal_file_name".".txt");
#    foreach $r (keys %each_ORI_limits)
#    {
#	print OUT "$r$each_ORI_limits{$r}\n";
#    }
#    close(OUT);
    open(OUT,">>ChIP_vs_ORI_middle_$file_name"."_$ARGV[2]"."_on_$signal_file_name".".txt");
    foreach $r (keys %each_ORI_middle)
    {
	print OUT "$r$each_ORI_middle{$r}\n";
    }
    close(OUT);
    print "Done. Output file is 'ChIP_vs_ORI_middle_$file_name"."_$ARGV[2]"."_on_$signal_file_name".".txt'.\n";
}


sub mean
{
    @array=split(' ',$_[0]);
    $sum=0;
    $n=0;
    foreach $x (@array)
    {
	if ($x ne 'NA')
	{
	    $sum+=$x;
	    ++$n;
	}
    }
    if ($n > 0)
    {
	$sum/$n;
    }
    else
    {
	'NA';
    }
}
