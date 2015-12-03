set title 'Quick sort / Merge sort timings (log scale)'
set xlabel 'item count'
set ylabel 'latency (ms)'
set logscale xy
set pointsize 1.5 # set the size of the plotted points
set key top left # move the key away from the lines
set timestamp # turn on a date/time indicator
#set xrange [0.75:1.25e3]
#set yrange [0.75:1e6]

set terminal png size 1280,800 enhanced font "Helvetica,16"
set output 'quick_sort_merge_sort.png'

plot "results.csv" using 1:2:(sprintf("%d", $2)) with labels notitle, \
     "results.csv" using 1:2 with linespoints title 'quick sort', \
     "results.csv" using 1:3:(sprintf("%d", $3)) with labels notitle, \
     "results.csv" using 1:3 with linespoints title 'merge sort'
