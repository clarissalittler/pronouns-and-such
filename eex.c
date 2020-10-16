//Pronouns:
//Nominative: eex
//Accusative: eex
//Pronominal: eex
//Predicative: eexs
//Reflexive: eexself

//Code:
main()
{
 int t = 0;
for(;;t++) putchar(((((((((t*7)&(t>>9))|((t*7)&(t>>9)))|((t*7)&(t>>9)))|((t*10)&(t>>7)))^((t*18)&(t>>3)))&255)-1));
}
