
blue="#00AEFF"
pink="#FF007E"

convert - +dither -colors 16 -remap colors.png text:- | 
  grep '$blue|$pink' |
  cut -f 1,4 -d' ' | 
  sed 's/://' |
  while (read line) ; do # e.g.: 123,345 #00AEFF -> 123,345 1
    pos=`cat $line | cut -f 1 -d' '`
    color=`cat $line | cut -f 2 -d' '`
    id=0
    if [ "$blue" == "$color" ] ; then
      id=1;
    fi;
    if [ "$pink" == "$color" ] ; then
      id=2;
    fi;
    echo $pos $id;
  done;


# convert in.jpg +dither -colors 16 in_quantize.jpg
# convert in.jpg  -separate -threshold 10% -combine in_quantize.jpg


