#!/usr/bin/perl

if ($ARGV[1] eq '')
{
    print "Please enter script arguments (e.g., ./Module_random_box_generator.pl 2_40_subL1_p005.gff.convertedboxes.gff Chromosome_organization.dat). The first argument is the name of a gff file containing the genomic coordinates of real boxes. The second argument is the name of a CSV file containing the description of C. elegans chromosome centers and arms.\n";
}
else
{
    open(ORGANIZATION,$ARGV[1]);
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
    
    open(GFF,$ARGV[0]);
    while(<GFF>)
    {
	chomp;
	@array=split('\t',$_);
	($chr,$start,$end)=($array[0],$array[3],$array[4]);
	$length=$end-$start+1;
	if ($end <= $left_arm_end{$chr})
	{
	    $left_arm{$chr}=$left_arm{$chr}." ".$length;
	}
	else
	{
	    if ($end <= $center_end{$chr})
	    {
		$center{$chr}=$center{$chr}." ".$length;
	    }
	    else
	    {
		$right_arm{$chr}=$right_arm{$chr}." ".$length;
		if ($chr eq 'chrX')
		{
		    print "chr=$chr start=$start end=$end\n";
		}
	    }
	}
    }
    close(GFF);

    foreach $chr (keys %left_arm)
    {
	if ($left_arm{$chr} ne '')
	{
	    open(LEFT,">Left_arm_".$chr."_real_box_lengths_from_".$ARGV[0].".dat");
	    $_=$left_arm{$chr};
	    s/^ //;
	    s/ /\n/g;
	    print LEFT "$_\n";
	    close(LEFT);
	}
	if ($center{$chr} ne '')
	{
	    open(CENTER,">Center_".$chr."_real_box_lengths_from_".$ARGV[0].".dat");
	    $_=$center{$chr};
	    s/^ //;
	    s/ /\n/g;
	    print CENTER "$_\n";
	    close(CENTER);
	}
	if ($right_arm{$chr} ne '')
	{
	    open(RIGHT,">Right_arm_".$chr."_real_box_lengths_from_".$ARGV[0].".dat");
	    $_=$right_arm{$chr};
	    s/^ //;
	    s/ /\n/g;
	    print RIGHT "$_\n";
	    close(RIGHT);
	}
    }

    for ($random=1;$random<=100;++$random) #Will generate 100 random sets of boxes in the inter-box regions of the genome tiled bu the array
#    for ($random=1;$random<=1;++$random)
    {
	print "Now generating random set $random ...\n";
	open(RANDOM,">Random_set_of_boxes_$random".".dat");
	for $chr (keys %left_arm)
#        for $chr ('chrI')
	{
            for $part ('Left_arm','Center','Right_arm')
#    	    for $part ('Left_arm')
	    {
		if ($part eq 'Left_arm')
		{
		    $start=0;
		    $end=$left_arm_end{$chr};
		}
		if ($part eq 'Center')
		{
		    $start=$left_arm_end{$chr}+1;
		    $end=$center_end{$chr};
		}
		if ($part eq 'Right_arm')
		{
		    $start=$center_end{$chr}+1;
		    $end=$right_arm_end{$chr};
		}

		if (($chr ne 'chrX') || ($part ne 'Right_arm'))
		{
		    print "   now working on $part of $chr ...\n";
		    open(R_COMMANDS,">R_commands_pick");
		    print R_COMMANDS "probe_coverage=read.table('".$part."_".$chr."_probes.dat')\n";
		    print R_COMMANDS "initial_t=as.numeric(format(Sys.time(), '%s'))\n";
		    print R_COMMANDS "sink('Chrono_break')\n";
		    print R_COMMANDS "start=".$start.";end=".$end."\n";
		    print R_COMMANDS "real=read.table('$ARGV[0]')\n";
		    print R_COMMANDS "real_ORI_starts=real\$V4[real\$V1=='$chr' & real\$V4>=$start & real\$V4<$end]\n";
		    print R_COMMANDS "real_ORI_ends=real\$V5[real\$V1=='$chr' & real\$V4>=$start & real\$V4<$end]\n";
		    print R_COMMANDS "real_ORI_starts_ordered=real_ORI_starts[order(real_ORI_starts)]\n";
		    print R_COMMANDS "real_ORI_ends_ordered=real_ORI_ends[order(real_ORI_starts)]\n";
		    print R_COMMANDS "unfinished=1\n";
		    print R_COMMANDS "while (unfinished > 0)\n";
		    print R_COMMANDS "{\n";
                    print R_COMMANDS "x=c();y=c()\n";
		    print R_COMMANDS "to_be_picked=sort(read.table('".$part."_".$chr."_real_box_lengths_from_".$ARGV[0].".dat')\$V1,decreasing=TRUE)#random ORIs to be picked (ordered by decreasing size, so that there are available chunks long enough to accomodate the long ones)\n";

		    print R_COMMANDS "for (i in 1:length(real_ORI_starts_ordered))\n";
		    print R_COMMANDS "{\n";
		    print R_COMMANDS "if (i==1)\n";
		    print R_COMMANDS "{\n";
		    print R_COMMANDS "if (real_ORI_starts_ordered[i]>start)\n";
		    print R_COMMANDS "{\n";
		    print R_COMMANDS "available_chunk_starts=c(start,real_ORI_ends_ordered[1]+1)\n";
		    print R_COMMANDS "available_chunk_ends=c(real_ORI_starts_ordered[i]-1)\n";
		    print R_COMMANDS "} else\n";
		    print R_COMMANDS "{\n";
		    print R_COMMANDS "available_chunk_starts=c(real_ORI_ends_ordered[1]+1)\n";
		    print R_COMMANDS "}\n";
		    print R_COMMANDS "}\n";
		    print R_COMMANDS "if (i>1 & i<length(real_ORI_starts_ordered))\n";
		    print R_COMMANDS "{\n";
		    print R_COMMANDS "available_chunk_starts=c(available_chunk_starts,real_ORI_ends_ordered[i]+1)\n";
		    print R_COMMANDS "available_chunk_ends=c(available_chunk_ends,real_ORI_starts_ordered[i]-1)\n";
		    print R_COMMANDS "}\n";
		    print R_COMMANDS "if (i==length(real_ORI_starts_ordered))\n";
		    print R_COMMANDS "if (real_ORI_ends_ordered[i]>end)\n";
		    print R_COMMANDS "{\n";
		    print R_COMMANDS "available_chunk_ends=c(available_chunk_ends,real_ORI_starts_ordered[i]-1)\n";
		    print R_COMMANDS "} else\n";
		    print R_COMMANDS "{\n";
		    print R_COMMANDS "available_chunk_starts=c(available_chunk_starts,real_ORI_ends_ordered[i]+1)\n";
		    print R_COMMANDS "available_chunk_ends=c(available_chunk_ends,real_ORI_starts_ordered[i]-1,end)\n";
		    print R_COMMANDS "}\n";
		    print R_COMMANDS "}\n";
		    print R_COMMANDS "\n";
		    print R_COMMANDS "for (length in to_be_picked)\n";
		    print R_COMMANDS "{\n";
		    print R_COMMANDS "available_for_that_length_starts=available_chunk_starts[available_chunk_ends-available_chunk_starts>length]\n";
		    print R_COMMANDS "available_for_that_length_ends=available_chunk_ends[available_chunk_ends-available_chunk_starts>length]\n";
		    print R_COMMANDS "supporting_probes=0\n";
		    print R_COMMANDS "while (supporting_probes<3) #Picked boxes must be supported by at least 3 probes\n";
		    print R_COMMANDS "{\n";
		    print R_COMMANDS "picked_chunk=sample(c(1:length(available_for_that_length_starts)),1,prob=c(available_for_that_length_ends-available_for_that_length_starts+1))#Picks an available sequence chunk, with picking probability being proportional to chunk length\n";
		    print R_COMMANDS "start=available_for_that_length_starts[picked_chunk]+length-1\n";
		    print R_COMMANDS "end=available_for_that_length_ends[picked_chunk]\n";
		    print R_COMMANDS "picked=round(runif(1,start,end)-0.5)\n";
		    print R_COMMANDS "supporting_probes=length(probe_coverage\$V1[probe_coverage\$V2>=picked-length & probe_coverage\$V3<=picked])\n";
		    print R_COMMANDS "current_t=as.numeric(format(Sys.time(), '%s'))\n";
		    print R_COMMANDS "if (current_t-initial_t > 300) # If you can't finish that random set in 300 seconds, just give up and try again\n";
		    print R_COMMANDS "{\n";
		    print R_COMMANDS "print(c(initial_t,current_t,length(x)))\n";
		    print R_COMMANDS "break\n";
		    print R_COMMANDS "}\n";
		    print R_COMMANDS "}\n";
		    print R_COMMANDS "new_s=available_chunk_starts[picked<available_chunk_starts+length-1 | picked>available_chunk_ends]\n";
		    print R_COMMANDS "new_s=c(new_s,picked+202) #There must be at least 201 bp between two consecutive boxes, hence the next available chunk starts only on the 202nd bp after the end of the picked sequence (hence 'picked+202', not 'picked+1')\n";
		    print R_COMMANDS "new_s=c(new_s,start-length+1)\n";
		    print R_COMMANDS "new_e=available_chunk_ends[picked<available_chunk_starts+length-1 | picked>available_chunk_ends]\n";
		    print R_COMMANDS "new_e=c(new_e,end)\n";
		    print R_COMMANDS "new_e=c(new_e,picked-length)\n";
		    print R_COMMANDS "available_chunk_starts=new_s[order(new_s)]\n";
		    print R_COMMANDS "available_chunk_ends=new_e[order(new_s)]\n";
		    print R_COMMANDS "x=c(x,picked-length+1)\n";
		    print R_COMMANDS "y=c(y,picked)\n";
		    print R_COMMANDS "current_t=as.numeric(format(Sys.time(), '%s'))\n";
		    print R_COMMANDS "if (current_t-initial_t > 300) # If you can't finish that random set in 300 seconds, just give up and try again\n";
		    print R_COMMANDS "{\n";
		    print R_COMMANDS "print(c(initial_t,current_t,length(x)))\n";
		    print R_COMMANDS "break\n";
		    print R_COMMANDS "}\n";
		    print R_COMMANDS "}\n";
		    print R_COMMANDS "unfinished=length(to_be_picked)-length(x)\n";
		    print R_COMMANDS "}\n";
		    print R_COMMANDS "sink()\n";
		    print R_COMMANDS "sink('Picked.dat')\n";
		    print R_COMMANDS "print(cbind(x,y))\n";
		    print R_COMMANDS "sink()\n";
		    close(R_COMMANDS);
		    `rm -f .RData;rm -f Picked.dat;R CMD BATCH R_commands_pick;tail -5 R_commands_pick.Rout`;
		    
		    open(R_OUTPUT,"Picked.dat");
		    while(<R_OUTPUT>)
		    {
			if (/^ *\[[0-9]*,\]/)
			{
			    s/^ *\[[0-9]*,\] *//;
			    s/  */ /g;
			    print RANDOM "$chr $_";
			}
		    }
		}
	    }
	}
	close(RANDOM);
    }
}
