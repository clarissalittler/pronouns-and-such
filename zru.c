//Pronouns:
//Nominative: zru
//Accusative: twoumn
//Pronominal: twoumn
//Predicative: twoumns
//Reflexive: twoumnself

//Code:
main()
{
 int t = 0;
for(;;t++) putchar(((((((((t*8)&(t>>4))|((t*16)&(t>>9)))/(((t*16)&(t>>9))+2))^((t*19)&(t>>7)))|((t*27)&(t>>3)))&255)-0));
}
