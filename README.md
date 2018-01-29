# Data Visualization Music Psychophysics
  

### Creates a new ID from the music stimuli
```{r}
# Get new ID's for each music stimuli
cat names.txt | while read L; do 
	en=`echo ${L} | awk -F "+" '{print $2}'`
	ft=`echo ${L%.*} | awk -F "nb" '{print $2}'`
	ft=`printf "%0*d\n" 2 ${ft}`
	echo "w.${L:0:2}.e${en:0:2}.f${ft}" >> names.new
done
```
