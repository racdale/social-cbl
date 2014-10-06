./sentgen grammar.txt -e 100 -s -h temp.txt -s 120
awk '{split($0,a," XXX ");for (i=1;i<length(a);i++) { printf("%s . ",a[i]); } }' temp.txt > sents.txt
rm -f temp.txt
