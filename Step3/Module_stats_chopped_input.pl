#!/usr/bin/perl

open(LIST,$ARGV[0]);
while (<LIST>)
{
    chomp;
    $data=$data.' '.$_;
}
close(LIST);

$m=&mean($data);
$sd=&standard_dev($data);
@array=split(' ',$data);
$n=push(@array);
if ($n == 0)
{
    $se='NA';
}
else
{
    $se=$sd/sqrt($n);
}
print "$m $sd $se $n\n";

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

sub standard_dev
{
    @array=split(' ',$_[0]);
    $sum=0;
    $n=0;
    foreach $x (@array)
    {
	if ($x ne 'NA')
	{
	    $sum+=($x-$_[1])**2;
	    ++$n;
	}
    }
    if ($n > 0)
    {
	sqrt($sum/$n);
    }
    else
    {
	'NA';
    }
}
