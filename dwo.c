//Pronouns:
//Nominative: dwo
//Accusative: dwaex
//Pronominal: dwaex
//Predicative: dwaexs
//Reflexive: dwaexself

//Code:
main()
{
 int t = 0;
for(;;t++) putchar(((((((((t*8)&(t>>8))^((t*13)&(t>>8)))|((t*13)&(t>>8)))|((t*16)&(t>>6)))^((t*24)&(t>>2)))&255)-0));
}
