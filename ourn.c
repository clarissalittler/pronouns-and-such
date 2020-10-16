//Pronouns:
//Nominative: ourn
//Accusative: dreimn
//Pronominal: dreimn
//Predicative: dreimns
//Reflexive: dreimnselves

//Code:
main()
{
 int t = 0;
for(;;t++) putchar(((((((((t*10)&(t>>4))|((t*16)&(t>>2)))|((t*16)&(t>>2)))|((t*19)&(t>>9)))|((t*32)&(t>>3)))&35)-1));
}
